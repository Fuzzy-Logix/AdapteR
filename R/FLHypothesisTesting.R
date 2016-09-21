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

#' Binomial Test
#'
#' Performs an exact test of a simple null hypothesis
#' about the probability of success in a Bernoulli experiment.
#'
#' @param x FLVector
#' @param n number of trials
#' @param p hypothesized probability of success
#' @param alternative indicates the alternative hypothesis and 
#' must be one of "two.sided", "greater" or "less". 
#' Additionally "LT","GT","EXACT" are supported for FL objects.
#' @param conf.level confidence level for the returned confidence interval.
#' Not Applicable for FL objects.
#' @section Constraints:
#' conf.level is not supported currently for FL objects.
#' print is not working for result htest object
#' @return A list with class "htest"
#' @examples
#' flv <- as.FLVector(sample(1:50,10,replace=T))
#' ResulthtestObject <- binom.test(flv,100,p=0.65)
#' ResulthtestObject <- binom.test(flv,100,p=0.65,"greater")
#' ResulthtestObject <- binom.test(flv,100,p=0.65,"LT")
#' expect_equal(ResulthtestObject[10],binom.test(as.R(flv[10]),100,p=0.65,"LT"))
#' @export
setGeneric("binom.test",function(x, n, p = 0.5,
                                alternative = c("two.sided", "less","greater"),
                                conf.level = 0.95)
                standardGeneric("binom.test"))

setMethod("binom.test",signature(x="FLAbstractColumn"),
    function(x,n,
            p,
            alternative
            ){
    return(paste0("FLBinTest(",
            paste0(c(n,x@columnName,
                    p,fquote(alternative)),collapse=","),
            ")"))
})
setMethod("binom.test",signature(x="FLVector"),
    function(x,
            n,
            p = 0.5,
            alternative = c("two.sided", "less","greater"),
            conf.level = 0.95){
        browser()
        if(length(p)>1)
            stop("'p' must be a single number between 0 and 1 \n ")
        else p <- as.vector(p)

        alternative <- match.arg(alternative)

        vcall <- paste(all.vars(sys.call())[1:2],collapse=" and ")

        vAltMapping <- c(EXACT="EXACT",LESS="LE",LE="LE",
                         GREATER="GE",GE="GE",LT="LT",
                         GT="GT",TWO.SIDED="TWO_SIDED",TWO_SIDED="TWO_SIDED")
        alternative1 <- vAltMapping[toupper(alternative)]

        if(!length(alternative1)>0)
            stop("alternative can be GT,LT,EXACT,two.sided,greater,less \n ")
        vres <- constructScalarSQL(pObject=x,
                                pFunc=binom.test,
                                n=n,
                                p=p,
                                alternative=alternative1
                                )
        vres@type <- "double"
        vresList <- list(statistic=c("number of successes"=x),
                        parameter=c("number of trials" =n),
                        p.value=vres,
                        estimate=c("probability of success"=x/n),
                        data.name=vcall,
                        null.value=c("probability of success"=p),
                        alternative=alternative,
                        method="Exact binomial test")
        # vresList <- list(statistic=x,
        #                 parameter=c("number of trials" =n),
        #                 p.value=vres,
        #                 estimate=x/n,
        #                 data.name=vcall,
        #                 null.value=c("probability of success"=p),
        #                 alternative=alternative,
        #                 method="Exact binomial test")
        class(vresList) <- "htest"
        return(vresList)
    })

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

## gk: please use constructUnionSQL
## Joining two FLVectors and creating a volatile table.
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

