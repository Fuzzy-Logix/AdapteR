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




wilcox.test.FLVector <- function(x,y = NULL,paired = TRUE, mu = 0,...)
{
    if(!is.FLVector(x) || !is.FLVector(y))

        stop("Must be FLVector")

    else
    {
        if(paired)
        {
            vviewName <- gen_view_name("wsrTest")
            if(length(x)> length(y))
                res <- sqlSendUpdate(connection, createHypoView(y,x,vviewName))
            else
                res <- sqlSendUpdate(connection, createHypoView(x,y,vviewName))


            vcall <- as.list(sys.call())
            dname = paste0(vcall[2]," and ",vcall[3])
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
                                      q.P_VALUE AS p,
                                      q.W_STAT_Neg AS W_Neg,
                                      q.W_STAT_Posi AS W_Pos
                           FROM ",ret$ResultTable," AS q")
            result <-  sqlQuery(connection,sqlstr)
            if(result$W_Pos > result$W_Neg)
            {
                stats = c(V = result$W_Pos)
            }
            else
                stats <- c(W = result$W_Pos)

            res <- list(statistic = stats,
                        p.value = result$p,
                        data.name =dname,
                        alternative = "two.sided",
                        method = "Wilcoxon rank sum test"
           #            call=vcall
                        )
            class(res) <- "htest"
            return(res)
        }
       
        else
        {
            
            vviewName <- gen_view_name("MWTest")
            sqlstr <- paste0("CREATE VIEW ",vviewName," AS
                        SELECT t.vectorValueColumn AS Num_Val,
                               1 AS GroupID
                        FROM (",constructSelect(x),") AS t
                   UNION ALL
                        SELECT l.vectorValueColumn AS Num_Val,
                              2 AS GroupID
                        FROM (",constructSelect(y),") AS l
                      ")

            vcall <- as.list(sys.call())
            dname = paste0(vcall[2]," and ",vcall[3])

            t <- sqlSendUpdate(connection,sqlstr)
            ret <- sqlStoredProc(connection,
                                 "FLMWTest",
                                 TableName = vviewName,
                                 ValColName = "Num_Val",
                                 GroupColName = "GroupID",
                                 WhereClause = NULL ,
                                 GroupBy = NULL,
                                 TableOutput = 1,
                                 outputParameter = c(ResultTable = 'a'))


            sqlstr <- paste0("SELECT U_STAT AS W,
                             P_VALUE AS P
                     FROM ",ret$ResultTable)
            result <- sqlQuery(connection, sqlstr)

            res <- list(statistic = c(W = result$W),
                        p.value = result$P,
                        data.name = dname,
                        alternative = "two.sided",
                        method = "Wilcoxon rank sum test"

                        )
            class(res) <- "htest"
            return(res)


            
        }


    }
}


WaldWolftest1s  <- function(vFLvector,threshold = 0)

{
    if(!is.FLVector(vFLvector))
       stop("Only take FLVector")
       else
       {
           vviewName <- gen_view_name("ww1sTest")
           if(threshold)
           {
               t <- sqlSendUpdate(connection,
                                  createSignview(vviewName, threshold, vFLvector))
}
           else{
               sqlstr <- paste0("SELECT FLMedianUDT(q.GroupID,q.Num_Val)
                                FROM ",vviewName)
               partition = sqlQuery(connection, sqlstr)
               t <- sqlSendUpdate(connection,
                                  createSignview(vviewName, threshold, vFLvector))
           }

           vcall <- as.list(sys.call())[[2]]
           
                                        #Testing the code part
           ret <- sqlStoredProc(connection,
                                "FLWWTest1S",
                                TableName = vviewName,
                                ObsIDColName = "ObsID",
                                Sign= "Sign",
                                WhereClause = NULL ,
                                GroupBy = NULL,
                                TableOutput = 1,
                                outputParameter = c(ResultTable = 'a')
                                )



           sqlstr <- paste0("SELECT q.Z AS Z, q.P_Value AS P  FROM ",
                            ret$ResultTable," AS q")
           res_1 <- sqlQuery(connection , sqlstr)
           result <- list(statistics = c(Z = res_1$Z),
                          p.value = res_1$P,
                          data.name = vcall,
                          method = "Wald Wolfowitz test"
                          )
           class(result) <- "htest"
           return(result)


                                        #print(res_1)
                                        # print(paste0("P-Value is ",res_1$P,"Z Value is ",res_1$Z))
           
       }
    }


                                        # join FLVecotor assign Sign Value and create a table name.
        
createSignview <- function(vName, vpart, vFLVector)
{
    
    sqlstr <- paste0("CREATE VIEW ",vName," AS
                        SELECT t.vectorValueColumn AS Num_Val,
                               1 AS GroupID,
                               t.vectorindexcolumn AS ObsID,
                              CASE WHEN t.vectorValueColumn > ",vpart,"
                                        THEN 1
                                   WHEN t.vectorValueColumn < ",vpart,"
                                        THEN -1
                                   ELSE 0
                              END AS Sign
                                FROM (",constructSelect(vFLVector),") AS t
                      ")
    return(sqlstr)

}













# Joining two FLVectors and creating a volatile table.
createHypoView <- function(q,r,pViewName)
{
    vminLength <- length(q)
    
    sqlstr0 <- paste0("CREATE VIEW ",pViewName," AS
                           SELECT b.vectorindexcolumn AS ObsID,
                                  a.vectorValueColumn AS Num_Val1,
                                  b.vectorvalueColumn AS Num_Val2
                           FROM (",constructSelect(q),") a, (",constructSelect(r),") b
                           WHERE a.vectorindexcolumn = b.vectorindexcolumn
                           UNION ALL
                           SELECT b.vectorindexcolumn AS ObsID,
                                  NULL AS Num_Val1,
                                  b.vectorValueColumn AS Num_Val2 
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

