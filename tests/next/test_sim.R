
## runif(N,0,1)
## methods(runif)
## getMethod("runif")
## getfindFunction("runif")
## dumpMethod("runif")

## This file uses R-like syntax to define function mappings very efficiently.
## This code is not evaluated in R, but parsed programmatically:
## function definitions for executing the R function (left of " <- ")
## in-dabase with DB Lytix function (right of " <- ") are created programmatically.
## 
##

x <- runif(FLenv$N)


setwd("/Users/gregor/fuzzylogix/AdapteR/")
devtools::document("AdapteR")
devtools::load_all("AdapteR")
FLcreateUnivariateMethodsFile()

require(testthat)

funName <- "runiv"
Renv <- new.env(parent = globalenv())
FLenv = as.FL(Renv)
Renv$N <- 1000000
FLenv$N <- FLSerial(1,Renv$N)
test_that(paste0('moments of univariate ',funName,' are identical'),{
    eval_expect_equal({
        myrand <- do.call(funName,list(N))
        mysd <- sd(myrand)
        mykurtosis <- kurtosis(myrand)
        myskewness <- skewness(myrand)
        myvar <- var(myrand)
        mymean <- mean(myrand)
        mymedian <- median(myrand)
        mysum <- sum(myrand)/N
    }, Renv,FLenv,
    noexpectation="myrand",
    expectation = c("mysd","mymean","mysum","myvar"),
    tolerance=1/sqrt(Renv$N))
})
FLenv$mysum
Renv$mysum


FLexpect_equal(Renv$mysd,FLenv$mysd)

cat(FLregisterUnivariate("rnorm <- FLSimNormal(n,mean,sd)"))

cat(FLregisterUnivariate("mean <- FLmean(n,min,max)"))

rbinom(


setGeneric('runif', function(n, min=0, max=1) { standardGeneric('runif') })
setMethod('runif', signature(n='integer'),
       function(n, min=0, max=1)
     stats::runif(n, min, max))
setMethod('runif', signature(n='FLSimpleVector'),
          function(n, min=0, max=1) {
    names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_runif'
    n@select@variables[[1]] <- paste0('FLSimUniform(',
                                      n@select@variables[[1]],
                                      ',',min, ',',max,')')
    n
})



setMethod("sd",signature(x="FLSimpleVector"),
          function (x, na.rm = FALSE){
    browser()
    x@select@variables[[1]] <- paste0('FLStdDev(',x@select@variables[[1]],')')
    names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_sd'
    x@select@order <- character()
    x
})

mean.FLSimpleVector <- function (x, trim = 0, na.rm = FALSE, ...){
    x <- modifyXforTrim(x=x,trim=trim)
    x@select@variables[[1]] <- paste0('FLmean(',x@select@variables[[1]],')')
    names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_mean'
    x@select@order <- character()
    x
}


as.vector(mean(runif(FLSerial(1,10000),-1,1)))

as.vector(sd(runif(FLSerial(1,10000),-1,1)))


as.vector(runif(FLSerial(1,100),-1,1))

FL.Table <- function(...){
    
}






