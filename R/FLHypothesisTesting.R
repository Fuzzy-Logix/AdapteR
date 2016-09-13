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
         #   vcall <-all.vars(sys.call())
          #  print(vcall)
            res <- list(statistics = c(W = result$W),
                        p.value = result$p,
                                        #data.name = paste0((x)," and ",substitute(y)),
                        alternative = "two.sided",
                        method = "Wilcoxon rank sum test",
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

            t <- sqlSendUpdate(connection,sqlstr)
            str <- paste0("SELECT * FROM ",vviewName)
            retu <- sqlQuery(connection, str)
            

            str <- paste0("CALL FLMWTest('",vviewName,"', 'Num_Val',
                                           'GroupID', NULL, NULL, 1,
                                         ResultTable);")
            res_1 <- sqlQuery(connection, str)
                #        ret <- sqlStoredProc(connection,
     #                            "FLMWTest",
      #                           TableName = vviewName,
       #                          GroupColName = "GroupID",
        #                         Val1ColName = "Num_Val",
         #                        WhereClause = NULL ,
          #                       GroupBy = NULL,
           #                      TableOutput = 1,
            #                     outputParameter = c(ResultTable = 'a'))

            sqlstr <- paste0("SELECT U_STAT AS W,
                             P_VALUE AS P
                     FROM ",res_1$ResultTable)
            result <- sqlQuery(connection, sqlstr)

            res <- list(statistics = c(W = result$W),
                        p.value = result$P,
                                        #data.name = paste0((x)," and ",substitute(y)),
                        alternative = "two.sided",
                        method = "Wilcoxon rank sum test"

                        )
            class(res) <- "htest"
            return(res)


            
        }


    }
}


Waldtest1s  <- function(vFLvector,
                        vSign
                        )

{
    if(!is.FLVector(vFLvector)|| !is.FLVector(vSign))
        stop("Only take FLVector")
    
    if(length(vFLvector) != length(vSign))
        stop("Both FLVector must be of same length")
    else
    {
        vviewName <- gen_view_name("ww1sTest")
        res <- sqlSendUpdate(connection, createHypoView(vFLvector, vSign, vviewName))
                                    #Testing the code part
        ret <- sqlStoredProc(connection,
                             "FLWWTest1S",
                             TableName = vviewName,
                             ObsIDColName = "ObsID",
                             Sign= "Num_Val2",
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
                       method = "Wald Wolfowitz test"
                       )
        class(result) <- "htest"
        return(result)
    }
}
