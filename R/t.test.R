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
setGeneric("t.test",function(x, y = NULL,
                           alternative = c("two.sided"),
                           mu = 0, paired = FALSE, var.equal = FALSE,
                           conf.level = 0.95,tails=2,...)
    standardGeneric("t.test"))

# setGeneric("t.test",function(x,y=NULL,mu=0,tails=2,
#                             conf.level=0.95,var.equal=FALSE,
#                             alternative= "two.sided",...)
#                 standardGeneric("t.test"))

setMethod("t.test",signature(x="FLVector"),
    function(x, y = NULL,
            alternative = c("two.sided"),
            mu = 0, paired = FALSE, var.equal = FALSE,
            conf.level = 0.95,
            tails=2,...){
        # checkHypoSystemTableExists()
        if(is.null(x)||!is.FLVector(x))
        stop("Only FLVector is supported")

        if(!tails %in% c("1","2")) stop("Please enter 1 or 2 as tails")
        df <- NULL
        if(length(y)==0){
        # sqlstr <- constructAggregateSQL(pFuncName = "FLtTest1S",
        #                                 pFuncArgs = c("c.FLStatistic",
        #                                                 mu,
        #                                               "a.vectorValueColumn",
        #                                                tails),
        #                                 pAddSelect = c(stat="c.FLStatistic"),
                                                                              
        #                                 pFrom = c(a = constructSelect(x),
        #                                           c = "fzzlARHypTestStatsMap"),
        #                                 pWhereConditions = c("c.FLFuncName = 'FLtTest1S'"),
        #                                 pGroupBy = "c.FLStatistic")
        
        sqlstr <- constructHypoTestsScalarQuery(pFuncName = "FLtTest1S",
                                                pFuncArgs = c(mu,
                                                              "a.vectorValueColumn",
                                                              tails),
                                                pFrom=c(a=constructSelect(x)),
                                                pStats=c("P_VALUE","T_STAT"))

        vcall<-paste(all.vars(sys.call())[1],collapse=" and ")
        estimate<-c("mean of x"=mean(x))
        method<-"One Sample t-test"
        df <- length(x)-1
        } else {
            if(var.equal==TRUE) {
                    var<-"EQUAL_VAR"
                    method<-" Two Sample t-test"}
            else {
                var<-"UNEQUAL_VAR"
                method<-"Welch Two Sample t-test"
            }
            vunionSelect <- constructUnionSQL(pFrom=c(a=constructSelect(x),b=constructSelect(y)),
                                              pSelect=list(a=c(groupID=1,num_val="a.vectorValueColumn"),
                                                           b=c(groupID=2,num_val="b.vectorValueColumn")))

            # sqlstr<-constructAggregateSQL(pFuncName="FLtTest2S",
            #                             pFuncArgs=c("c.FLStatistic",
            #                                          fquote(var),
            #                                         "a.groupID",
            #                                         "a.num_val",
            #                                         tails),
            #                             pAddSelect=c(stat="c.FLStatistic"),
            #                             pFrom=c(a=vunionSelect,
            #                                     c="fzzlARHypTestStatsMap"),
            #                             pWhereConditions=c("c.FLFuncName='FLtTest2S'"),
            #                             pGroupBy="c.FLStatistic")

            sqlstr <- constructHypoTestsScalarQuery(pFuncName = "FLtTest2S",
                                                pFuncArgs = c(fquote(var),
                                                            "a.groupID",
                                                            "a.num_val",
                                                            tails),
                                                pFrom=c(a=vunionSelect),
                                                pStats=c("P_VALUE","T_STAT"))
            vcall<-paste(all.vars(sys.call())[1:2],collapse=" and ")
            estimate <-c("mean of x" = mean(x),"mean of y" = mean(y))
            }
    result <- sqlQuery(connection, sqlstr)
    colnames(result) <- tolower(colnames(result))
    names(mu)<-if(!is.null(y)) "difference in means" else "mean"
    cint<-cint(x,y,conf.level,mu,var.equal,alternative)
    if(is.null(df))
        df <- cint[3]
    cint<-cint[1:2]
    attr(cint,"conf.level") <- conf.level
    res <- list(statistic =c(t = as.numeric(result$t_stat)),
                parameter= c(df=df),
                p.value=   c(as.vector(result$p_value)),
                conf.int = cint,
                estimate =estimate,
                null.value=mu,
                alternative=alternative,
                method=method,
                data.name = vcall)                
    class(res) <- "htest"
    return(res)
    }
)

setMethod("t.test",signature(x="ANY"),
    function(x, y = NULL,
           alternative = c("two.sided"),
           mu = 0, paired = FALSE, var.equal = FALSE,
           conf.level = 0.95,
           tails=2,
           ...){
        if (!requireNamespace("stats", quietly = TRUE)){
                stop("stats package needed for t.test. Please install it.",
                call. = FALSE)}
        else return(stats::t.test(x=x,y=y,alternative=alternative,
                                mu=mu,paired=paired,var.equal=var.equal,
                                conf.level=conf.level,...))
    })

setMethod("t.test",signature(x="formula"),
    function(x,y,...){
        if (!requireNamespace("stats", quietly = TRUE)){
                stop("stats package needed for t.test. Please install it.",
                call. = FALSE)}
        else{
            vlist <- list(...)
            if(!is.null(vlist[["formula"]]))
                x <- vlist[["formula"]]
            if(!is.null(vlist[["data"]]))
                y <- vlist[["data"]]
            AliasFunction <- function(formula,data,x,y,...)
                return(stats::t.test(formula=x,data=y,...))
            return(AliasFunction(x=x,y=y,...))
        }
    })

