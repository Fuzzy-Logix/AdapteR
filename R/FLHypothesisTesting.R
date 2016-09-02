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

setGeneric("z.test",function(x,y=NULL,test_val=0,tails=2,conf.level=0.95,prob=0)
                standardGeneric("z.test"))
setMethod("z.test",signature(x="FLVector"),
    function(x,
            y=NULL,
            test_val=0,
            tails=2,
            conf.level=0.95,
            prob=0){
        if(is.null(x)||!is.FLVector(x))
            stop("Only FLVector is supported")

        if(!tails %in% c("1","2")) stop("Please enter 1 or 2 as tails")

        vcall<-paste(all.vars(sys.call())[1:2],collapse=" and ")

        if(length(y)==0){
            if(!test_val) stop("The testing value is missing")
            if(prob==0)
            {
                pFuncName<-"FLzTest1S"
                vsqlstr<- constructAggregateSQL(pFuncName=pFuncName,
                                                pFuncArgs=c("c.FLStatistic",
                                                            test_val,
                                                            "a.vectorValueColumn",
                                                                tails),
                                                pAddSelect=c(stat="c.FLStatistic"),
                                                pFrom=c(a=constructSelect(x),
                                                        c="fzzlARHypTestStatsMap"),
                                                pWhereConditions="c.FLFuncName='FLzTest1S'",
                                                pGroupBy="c.FLStatistic")

         }

            else { 
                pFuncName<-"FLzTest1P"
                vsqlstr<- constructAggregateSQL(pFuncName=pFuncName,
                                                pFuncArgs=c("c.FLStatistic",
                                                            test_val,
                                                            "a.vectorValueColumn",
                                                                tails),
                                                pAddSelect=c(stat="c.FLStatistic"),
                                                pFrom=c(a=constructSelect(x),
                                                        c="fzzlARHypTestStatsMap"),
                                                pWhereConditions="c.FLFuncName='FLzTest1P'",
                                                pGroupBy="c.FLStatistic")

            }
        }

        else{
            if(prob==0)
            {
            pFuncName<-"FLzTest2S"
            vsqlstr<-constructAggregateSQL(pFuncName=pFuncName,
                                        pFuncArgs=c("c.FLStatistic",
                                                    "a.Num_Val1",
                                                    "a.Num_Val2",
                                                    tails),
                                        pAddSelect=c(stat="c.FLStatistic"),
                                        pFrom=c(a=constructSelect(createHypoView(x,y)),
                                                c="fzzlARHypTestStatsMap"),
                                        pWhereConditions=c("c.FLFuncName='FLzTest2S'"),
                                        pGroupBy="c.FLStatistic")
            }

            else {

            pFuncName<-"FLzTest2P"
            vsqlstr<-constructAggregateSQL(pFuncName=pFuncName,
                                        pFuncArgs=c("c.FLStatistic",
                                                    "a.Num_Val1",
                                                    "a.Num_Val2",
                                                    tails),
                                        pAddSelect=c(stat="c.FLStatistic"),
                                        pFrom=c(a=constructSelect(createHypoView(x,y)),
                                                c="fzzlARHypTestStatsMap"),
                                        pWhereConditions=c("c.FLFuncName='FLzTest2P'"),
                                        pGroupBy="c.FLStatistic")



            }        
         }

    vres<-sqlQuery(connection,vsqlstr)
    cint<-cint(x,conf.level)
    attr(cint,"conf.level") <- conf.level
    vresList<-list(statistic=c("P value"=vres[1,1]),
                   parameter=c("Z stat"=vres[2,1]),
                   data.name=vcall,
                   alternative=paste0("true mean is not equal to ",test_val),
                   estimate =c("mean of x" = mean(x)),
                   method="One Sample z-test",
                   conf.int = cint
                   )
    class(vresList)<-"htest"
    return(vresList)
    }
  )

t.test.FLVector <- function(x,
                            y= NULL,
                            mu = 0,
                            tails=2,
                            conf.level =.95,
                            variance="equal",
                            alternative="two.sided",...)
{       
        if(is.null(x)||!is.FLVector(x))
        stop("Only FLVector is supported")

        if(!tails %in% c("1","2")) stop("Please enter 1 or 2 as tails")

        vcall<-paste(all.vars(sys.call())[1],collapse=" and ")

        if(length(y)==0){
        sqlstr <- constructAggregateSQL(pFuncName = "FLtTest1S",
                                        pFuncArgs = c("c.FLStatistic",
                                                        mu,
                                                      "a.vectorValueColumn",
                                                       tails),
                                        pAddSelect = c(stat="c.FLStatistic",
                                                       df = "COUNT(DISTINCT a.vectorValueColumn)"),
                                                                              
                                        pFrom = c(a = constructSelect(x),
                                                  c = "fzzlARHypTestStatsMap"),
                                        pWhereConditions = c("c.FLFuncName = 'FLtTest1S'"),
                                        pGroupBy = "c.FLStatistic")
        method<-"One Sample t-test"
        }                                         
           
        else{
            if(variance=="equal") var<-"EQUAL_VAR"
            else                  var<-"UNEQUAL_VAR"

        sqlstr<-constructAggregateSQL(pFuncName="FLtTest2S",
                                        pFuncArgs=c("c.FLStatistic",
                                                     fquote(var),
                                                    "a.Num_Val1",
                                                    "a.Num_Val2",
                                                    tails),
                                        pAddSelect=c(stat="c.FLStatistic"),
                                        pFrom=c(a=constructSelect(createHypoView(x,y)),
                                                c="fzzlARHypTestStatsMap"),
                                        pWhereConditions=c("c.FLFuncName='FLtTest2S'"),
                                        pGroupBy="c.FLStatistic")
        method<-"Two Sample t-test"


    }         
    result <<- sqlQuery(connection, sqlstr)
    cint<-cint(x,conf.level,alternative)
    attr(cint,"conf.level") <- conf.level
    res <- list(data.name = vcall,
                statistic =c(t = as.numeric(result[1,1])),
                parameter= c(df=as.numeric(result[1,3])-1),
                p.value=   c("p-value"=as.numeric(result[2,1])),
                alternative=paste0("true mean is not equal to ",mu ),
                estimate =c("mean of x" = mean(x)),
                method=method,
                conf.int = cint,
                alternative="two.sided")                
    
    class(res) <- "htest"
    return(res)
    
}


cint<-function(x,conf.level,alternative="two.sided"){

if (alternative=="two.sided")
{
    df<-length(x)-1
    sd<-sd(x)/sqrt(length(x))
    qt<-qt(conf.level+(1-conf.level)/2,df)*sd
    res<-mean(x)+c(-qt,qt)
}

else stop("Not available for others")

return(res)
}

createHypoView <- function(x,y){

    if(length(x)>length(y)) { q<-y
                              r<-x}
    else{  q<-x
           r<-y
    }

    vminLength<-length(q)
    pViewName<-genRandVarName()
    sqlstr0 <- paste0("CREATE VIEW ",pViewName," AS
                       SELECT a.vectorValueColumn AS Num_Val1,
                       b.vectorvalueColumn AS Num_Val2
                       FROM (",constructSelect(q),") a, (",constructSelect(r),") b
                       WHERE a.vectorindexcolumn = b.vectorindexcolumn
                       UNION ALL
                       SELECT NULL AS Num_Val1, b.vectorValueColumn AS Num_Val2 
                       FROM (",constructSelect(r),") b 
                       WHERE b.vectorindexcolumn >",vminLength)
    sqlQuery(connection,sqlstr0)
    return(pViewName)       
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

