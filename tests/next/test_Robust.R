

##to-do: 1.)store things in Results,
##      2.) psi parameter not working.
##      3.) correct mapping of t-value with variable.

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

## summary, print.
test_that("Summary for rlm and print:",{result <- eval_expect_equal({
    fitS <- summary(fit)
    print(fitS)
},Renv,FLenv,
verbose = TRUE,
expectation = c("fitS"))})

test_that("rlm predict $ fitted.values:",{result <- eval_expect_equal({
    fitP <- predict(fit)
    fitPbyname <- fit$fitted.values
},Renv,FLenv,
expectation=c("fitP"),
check.attributes=F)
})


## coefficients and Residuals: 
test_that("rlm coefficients, residuals:",{result <- eval_expect_equal({
    fitC <- coefficients(fit)
    fitR <- residuals(fit)
},Renv,FLenv,
expectation=c("fitC","fitR"),
check.attributes=F,
tolerance = .01,
verboose = T)})


## Wide Table:


FLenv <- new.env(parent = globalenv())
FLenv$widetbl <- FLTable("tblautompg", "ObsID")
Renv <- as.R(FLenv) ## todo: add temporary=FALSE

eval_expect_equal({
    fit <- rlm(Weight ~ Acceleration , data = widetbl)
},Renv,FLenv,
noexpectation=c("fit"),
check.attributes=F)


eval_expect_equal({
    fitS <- summary(fit)
    print(fitS)
},Renv,FLenv)

##
eval_expect_equal({
    fitP <- predict(fit) ## todo: use FLSUMPROD
    fitPbyname <- fit$fitted.values
},Renv,FLenv,
expectation=c("fitP","fitPbyname"),
check.attributes=F,
verbose = TRUE)


## 
eval_expect_equal({
    fitC <- coefficients(fit)
    fitR <- residuals(fit)
},Renv,FLenv,
expectation=c("fitC","fitR"),
check.attributes=F,
verboose = T)
