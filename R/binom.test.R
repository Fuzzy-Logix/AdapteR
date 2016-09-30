
NULL
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
#' @return A list with class "FLhtest"
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
        DNAME <- deparse(substitute(x))
        DNAME <- paste(DNAME, "and", deparse(substitute(n)))
        if(length(p)>1)
            stop("'p' must be a single number between 0 and 1 \n ")
        else p <- as.vector(p)

        if(!is.vector(n))
            stop("n must be a vector in binom.test \n ")
        n <- n[1]
        alternative <- match.arg(alternative)

        # vcall <- paste(all.vars(sys.call())[1:2],collapse=" and ")

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
        vxCopy <- x
        names(vxCopy) <- rep("number of successes",length(x))
        vestimate <- x/n
        names(vestimate) <- rep("probability of success",length(x))
        vresList <- list(statistic=vxCopy,
                        parameter=c("number of trials" =n),
                        p.value=vres,
                        estimate=vestimate,
                        data.name=DNAME,
                        null.value=c("probability of success"=p),
                        alternative=alternative,
                        method="Exact binomial test")
        class(vresList) <- "FLhtest"
        return(vresList)
    })