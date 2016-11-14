##to-do: 1.)store things in Results,
##      3.) correct mapping of t-value with variable.
##      4.) plot, cov.unscaled -> summary.rlm
## Robust Regression
## Deep Table
library(MASS)
#options(debugSQL =FALSE)

Renv <- new.env(parent = globalenv())
FLenv <- as.FL(Renv)

deeptbl  <- FLTable("tblRobustRegr", "ObsID","VarID", "Num_Val")
FLenv$DAT <- deeptbl

## varmapping(deeptbl) <- c("a","i","var")
## varmapping(deeptbl) <- X(table="fzzlRegrDataPrepMap",id="Final_VarID", value="COLUMN_NAME")
RD <- as.data.frame(deeptbl, drop.intercept=TRUE)
names(RD) <- c("a","i","var") ## should be done by varmapping
RD$i <- NULL  ## should be dropped during as.data.frame
Renv$DAT <- RD

eval_expect_equal({
    fit <- rlm(a~., data = DAT)
},Renv,FLenv,
noexpectation = "fit")


##Prediction, Residuals
FLenv$pred <- predict(FLenv$fit)
Renv$pred <- predict(Renv$fit)
FLexpect_equal(FLenv$fitP, Renv$fitP)
FLexpect_equal(residuals(FLenv$fit), unname(residuals(Renv$fit)))

## Prediction, Residuals, fitted.values: 
test_that("rlm coefficients, residuals:",{result <- eval_expect_equal({
    pred <- predict(fit);names(pred) <- NULL
    fit.val <- fit$fitted.values;names(fit.val) <- NULL
    res <- residuals(fit);names(res) <- NULL
  },Renv,FLenv,
expectation=c("pred"),
check.attributes=FALSE,
verboose = TRUE)})

## coefficients: 
test_that("rlm coefficients:",{result <- eval_expect_equal({
    fitC <- coefficients(fit)
    names(fitC) <- NULL
    
},Renv,FLenv,
expectation=c("fitC"),
check.attributes=FALSE,
verboose = TRUE)})

## summary, print.
test_that("Summary for rlm and print:",{result <- eval_expect_equal({
    fitS <- summary(fit)
    print(fitS)
},Renv,FLenv,
verbose = TRUE,
expectation = c("fitS"))})





## Wide Table:
FLenv <- new.env(parent = globalenv())
FLenv$widetbl <- FLTable("tblautompg", "ObsID")
Renv <- as.R(FLenv) ## todo: add temporary=FALSE

eval_expect_equal({
    fit <- rlm(Weight ~ Acceleration , data = widetbl)
},Renv,FLenv,
noexpectation=c("fit"),
check.attributes=FALSE,
verbose = TRUE)

##
eval_expect_equal({
    pred <- predict(fit);names(pred) <- NULL
    res <- residuals(fit);names(res) <- NULL
},Renv,FLenv,
expectation=c("pred","res"),
check.attributes=FALSE,
verbose = TRUE)


## 
eval_expect_equal({
    fitC <- coefficients(fit)
    names(fitC) <- NULL
},Renv,FLenv,
expectation=c("fitC"),
check.attributes=FALSE,
verboose = TRUE)
