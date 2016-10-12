#' @include FLMatrix.R
#' @include FLconstructSQL.R
NULL
#' Student's t-Test
#'
#' Performs one and two sample t-tests on vectors of data.
#'
#' @param x FLVector
#' @param y FLVector
#' @param mu The value of hypothesized mean
#' @param var.equal a logical variable indicating whether to treat the two variances as being equal.
#' If TRUE then the pooled variance is used to estimate the
#' variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used.
#' @return A list with class "htest" containing the statistic and p-values.
#' @examples
#' flx<-as.FLVector(rnorm(100))
#' fly<-as.FLVector(rnorm(100))
#' t.test(flx)
#' t.test(flx,fly)
#' t.test(flx,fly,var.equal=F)
#' @export
t.test.FLVector <- function(x,
                            y=NULL,
                            mu = 0,
                            tails=2,
                            conf.level =.95,
                            var.equal=FALSE,
                            alternative="two.sided",...)
{       
        if(is.null(x)||!is.FLVector(x))
        stop("Only FLVector is supported")

        if(!tails %in% c("1","2")) stop("Please enter 1 or 2 as tails")

        if(length(y)==0){
            sqlstr <- constructAggregateSQL(pFuncName = "FLtTest1S",
                                            pFuncArgs = c("c.FLStatistic",
                                                          mu,
                                                          "a.vectorValueColumn",
                                                          tails),
                                            pAddSelect = c(stat="c.FLStatistic"),
                                            pFrom = c(a = constructSelect(x),
                                                      c = "fzzlARHypTestStatsMap"),
                                            pWhereConditions = c("c.FLFuncName = 'FLtTest1S'"),
                                            pGroupBy = "c.FLStatistic")
            vcall<-paste(all.vars(sys.call())[1],collapse=" and ")
            alter<-"two.sided"
            estimate<-c("mean of x"=mean(x))
            method<-"One Sample t-test"
            nullval <- c(mean=0)
            parameter <- c(df=length(x)-1)
        } else {
            if(var.equal==TRUE) {
                    var<-"EQUAL_VAR"
                    method<-" Two Sample t-test" ## note: space at start of string is present in R, possibly for formatting output
            } else {
                var<-"UNEQUAL_VAR"
                method<-"Welch Two Sample t-test"
            }
            vunionSelect <- constructUnionSQL(pFrom=c(a=constructSelect(x),b=constructSelect(y)),
                                              pSelect=list(a=c(groupID=1,num_val="a.vectorValueColumn"),
                                                           b=c(groupID=2,num_val="b.vectorValueColumn")))

            sqlstr<-constructAggregateSQL(pFuncName="FLtTest2S",
                                        pFuncArgs=c("c.FLStatistic",
                                                     fquote(var),
                                                    "a.groupID",
                                                    "a.num_val",
                                                    tails),
                                        pAddSelect=c(stat="c.FLStatistic"),
                                        pFrom=c(a=vunionSelect,
                                                c="fzzlARHypTestStatsMap"),
                                        pWhereConditions=c("c.FLFuncName='FLtTest2S'"),
                                        pGroupBy="c.FLStatistic")
            vcall<-paste(all.vars(sys.call())[1:2],collapse=" and ")
            alter<-"two.sided"
            estimate <-c("mean of x" = mean(x),"mean of y" = mean(y))
            nullval <- c("difference in means"=0)
            parameter <- c(df=length(x)+length(y)-2)
        }  
        result <- sqlQuery(connection, sqlstr)
        rcint <- cint(x,conf.level,alter)
        attr(cint,"conf.level") <- conf.level
        res <- list(statistic =c(t = as.numeric(result[1,1])),
                    parameter=parameter,
                    p.value=   as.numeric(result[2,1]),
                    conf.int = rcint,
                    estimate =estimate,
                    null.value = nullval,
                    alternative=alter,
                    method=method,
                    data.name = vcall)
        class(res) <- "htest"
    return(res)
}

