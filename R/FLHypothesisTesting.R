#' @include FLMatrix.R
NULL

# FLFuncName <- c("FLMcNemarTest","FLMcNemarTest","FLMcNemarTest")
# FLStatistic <- c("T_STAT","CHI_SQ","BINOMIAL_EXACT")
# vdf <- data.frame(FLFuncName=FLFuncName,
#                 FLStatistic=FLStatistic)
# flt <- as.FLTable(vdf,tableName="fzzlARHypTestStatsMap",drop=TRUE)
#' McNemar's Chi-squared Test for Count Data.
#'
#' Performs McNemar's chi-squared test for 
#' symmetry of rows and columns in a two-dimensional contingency table.
#'
#' @param x FLVector
#' @param y FLVector
#' @param correct a logical or character indicating whether to apply 
#' continuity correction when computing the test statistic.
#' If TRUE,Available methods are EDWARDS(default),YATES.
#' @return A list with class "htest"
#' @examples
#' flTable <- FLTable("tblMcNemarMulti", "ObsID",whereconditions="datasetID=1")
#' x <- flTable[,"predicted"]
#' y <- flTable[,"observed"]
#' ResulthtestObject <- mcnemar.test(x,y)
#' ResulthtestObject <- mcnemar.test(x,y,FALSE)
#' ResulthtestObject <- mcnemar.test(x,y,"YATES")
#' @export
setGeneric("mcnemar.test",function(x,y=NULL,correct=TRUE)
                standardGeneric("mcnemar.test"))
setMethod("mcnemar.test",signature(x="FLVector"),
    function(x,
            y=NULL,
            correct=TRUE){
        if(is.null(y) || !is.FLVector(y))
            stop("contingency table not supported. x and y should be FLVectors. \n ")
        if(is.logical(correct)){
            if(correct) vcorrection <- "EDWARDS"
            else vcorrection <- "NONE"
        }
        else if(toupper(correct) %in% c("EDWARDS","YATES"))
            vcorrection <- toupper(correct)
        else stop("correct can be logical or character(YATES) \n ")

        vcall <- paste(all.vars(sys.call())[1:2],collapse=" and ")

        ## Casting to BYTEINT
        x <- as.FLByteInt(x)
        y <- as.FLByteInt(y)

        vsqlstr <- constructAggregateSQL(pFuncName="FLMcNemarTest",
                                        pFuncArgs=c("c.FLStatistic",
                                                    fquote(vcorrection),
                                                    "a.vectorValueColumn",
                                                    "b.vectorValueColumn"),
                                        pAddSelect=c(stat="c.FLStatistic",
                                                    df="COUNT(DISTINCT a.vectorValueColumn)"),
                                        pFrom=c(a=constructSelect(x),
                                                b=constructSelect(y),
                                                c="fzzlARHypTestStatsMap"),
                                        pWhereConditions=c("a.vectorIndexColumn=b.vectorIndexColumn",
                                                            "c.FLFuncName='FLMcNemarTest'"),
                                        pGroupBy="c.FLStatistic")

        # vsqlstr <- paste0("SELECT FLMcNemarTest(c.FLStatistic,",fquote(vcorrection),
        #                                         ",a.vectorValueColumn,b.vectorValueColumn) AS val, \n ",
        #                             "c.FLStatistic as stat, \n ",
        #                             "count(distinct a.vectorValueColumn) as df \n ",
        #                   " FROM (",constructSelect(x),") a, \n ",
        #                         "(",constructSelect(y),") b, \n ",
        #                         "fzzlARHypTestStatsMap c \n ",
        #                   " WHERE a.vectorIndexColumn=b.vectorIndexColumn \n ",
        #                   " AND c.FLFuncName='FLMcNemarTest' ",
        #                   " GROUP BY c.FLStatistic ")
        vres <- sqlQuery(connection,vsqlstr)
        r <- vres[["df"]][1]
        vresList <- list(statistic=c("McNemar's chi-squared"=as.vector(vres[vres[,"stat"]=="T_STAT","OutVal"])),
                        parameter=c(df=r*(r-1)/2),
                        p.value=as.vector(vres[vres[,"stat"]=="CHI_SQ","OutVal"]),
                        data.name=vcall,
                        binomial_exact=c(binomial_exact = as.vector(vres[vres[,"stat"]=="BINOMIAL_EXACT","OutVal"])))
        class(vresList) <- "htest"
        return(vresList)
    })




wilcox.test.FLVector <- function(x,y = NULL,mu = 0,...)
{
    if(!is.FLVector(x) || !is.FLVector(y))

        stop("Must be FLVector")

    else
    {
       
        vviewName <- gen_view_name("wsrTest")
        if(length(x)> length(y))
            res <- sqlSendUpdate(connection, createHypoView(y,x,vviewName))
        else
            res <- sqlSendUpdate(connection, createHypoView(x,y,vviewName))
       
        #  Using Stored Proc Query.

        ret <- sqlStoredProc(connection,
                             "FLWSRTest",
                             TableName = vviewName,
                             Val1ColName = "Num_Val1",
                             Val2ColName = "Num_Val2",
                             WhereClause = NULL ,
                             GroupBy = NULL,
                             TableOutput = 1,
                             outputParameter = c(ResultTable = 'a'))

        sqlstr <- paste0( "SELECT q.W_STAT AS W,
                                  q.P_VALUE AS p  
                           FROM ",ret$ResultTable," AS q")
        result <-  sqlQuery(connection,sqlstr)       
       
        # Extracting the result.
        
        res <- list(statistics = c(W = result$W),
                    p.value = result$p,
                   # data.name = paste0((x)," and ",substitute(y)),
                    alternative = "two.sided",
                    method = "Wilcoxon rank sum test"
                    )
        class(res) <- "htest"
        return(res)
    }
}

# Joining two FLVectors and creating a volatile table.
createHypoView <- function(q,r,pViewName)
{
    vminLength <- length(q)
    
    sqlstr0 <- paste0("CREATE VIEW ",pViewName," AS
                           SELECT a.vectorValueColumn AS Num_Val1,
                                  b.vectorvalueColumn AS Num_Val2
                           FROM (",constructSelect(q),") a, (",constructSelect(r),") b
                           WHERE a.vectorindexcolumn = b.vectorindexcolumn
                           UNION ALL
                           SELECT NULL AS Num_Val1, b.vectorValueColumn AS Num_Val2 
                           FROM (",constructSelect(r),") b 
                           WHERE b.vectorindexcolumn >",vminLength)
    return(sqlstr0)         
}












##################################### Aggregate SQL ###########################################
constructAggregateSQL <- function(pFuncName,
                                  pFuncArgs,
                                  pAddSelect="",
                                  pFrom,
                                  pWhereConditions="",
                                  pGroupBy="",
                                  pOrderBy=""){
    vfunCall <- c(OutVal=paste0(pFuncName,"(",paste0(pFuncArgs,collapse=","),")"))
    vSelects <- c(vfunCall,pAddSelect)
    vSelects <- vSelects[vSelects!=""]

    pWhereConditions <- setdiff(pWhereConditions,"")
    pGroupBy <- setdiff(pGroupBy,"")
    pOrderBy <- setdiff(pOrderBy,"")

    vsqlstr <- paste0("SELECT ",
                    paste0(vSelects," AS ",names(vSelects),collapse=", \n ")," \n ",
                    " FROM ",
                    paste0(ifelse(grepl(" ",pFrom),paste0("(",pFrom,")"),pFrom),
                                    " AS ",names(pFrom),collapse=", \n ")," \n ",
                    ifelse(length(pWhereConditions)>0,
                        paste0(" WHERE ",paste0(pWhereConditions,collapse=" AND ")," \n "),
                        ""),
                    ifelse(length(pGroupBy)>0,
                        paste0(" GROUP BY ",paste0(pGroupBy,collapse=",")," \n "),
                        ""),
                    ifelse(length(pOrderBy)>0,
                        paste0(" ORDER BY ",paste0(pOrderBy,collapse=",")," \n "),
                        ""))
    return(vsqlstr)
}

