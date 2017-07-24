##to-do: 1.)store things in Results,
##      3.) correct mapping of t-value with variable.
##      4.) plot, cov.unscaled -> summary.rlm
## Robust Regression
## Deep Table
library(MASS)
#options(debugSQL =FALSE)


## JIRA:: https://fuzzyl.atlassian.net/browse/TDFL-838
## https://app.asana.com/0/150173007236461/374811135370878/f

Renv <- new.env(parent = globalenv())
FLenv <- as.FL(Renv)


deeptbl  <- FLTable(getTestTableName("tblRobustRegr"), "ObsID","VarID", "Num_Val")
FLenv$DAT <- deeptbl

## varmapping(deeptbl) <- c("a","i","var")
## varmapping(deeptbl) <- X(table="fzzlRegrDataPrepMap",id="Final_VarID", value="COLUMN_NAME")
RD <- as.data.frame(deeptbl, drop.intercept=TRUE)
names(RD) <- c("a","i","var") ## should be done by varmapping
RD$i <- NULL  ## should be dropped during as.data.frame
Renv$DAT <- RD

## Coefficients match closely for wt.method as huber.
eval_expect_equal({
    fit <- rlm(a~., data = DAT)
    vcoef <- coef(fit)
},Renv,FLenv,
noexpectation = "fit",
expectation = "vcoef",
check.attributes=FALSE,
tolerance=0.01)

## Coefficients dont match for wt.method as bi.square.
eval_expect_equal({
    fit <- rlm(a~., data = DAT,psi="psi.bisquare",maxiter=500)
    vcoef <- coef(fit)
},Renv,FLenv,
noexpectation = "fit",
expectation = "vcoef",
check.attributes=FALSE,
tolerance=0.01)

## Prediction would slightly differ as a result. -- dont compare predicted
test_that("rlm check predict and residuals methods work:",{
    result <- eval_expect_equal({
                pred <- predict(fit);names(pred) <- NULL
                fit.val <- fit$fitted.values;names(fit.val) <- NULL
                res <- residuals(fit);names(res) <- NULL
                FLexpect_equal(length(pred),nrow(DAT))
                },Renv,FLenv,
                noexpectation=c("pred","fit.val","res"),
                check.attributes=FALSE,
                verboose = TRUE)
})

##Prediction, Residuals
# FLenv$pred <- predict(FLenv$fit)
# Renv$pred <- unname(predict(Renv$fit))
# FLexpect_equal(FLenv$pred, Renv$pred)
# FLexpect_equal(residuals(FLenv$fit), unname(residuals(Renv$fit)))

# ## Prediction, Residuals, fitted.values: 
# test_that("rlm coefficients, residuals:",{result <- eval_expect_equal({
#     pred <- predict(fit);names(pred) <- NULL
#     fit.val <- fit$fitted.values;names(fit.val) <- NULL
#     res <- residuals(fit);names(res) <- NULL
#   },Renv,FLenv,
# expectation=c("pred"),
# check.attributes=FALSE,
# verboose = TRUE)})

# ## coefficients: 
# test_that("rlm coefficients:",{result <- eval_expect_equal({
#     fitC <- coefficients(fit)
#     names(fitC) <- NULL
    
# },Renv,FLenv,
# expectation=c("fitC"),
# check.attributes=FALSE,
# verboose = TRUE)})

# ## summary, print.
# test_that("Summary for rlm and print:",{result <- eval_expect_equal({
#     fitS <- summary(fit)
#     print(fitS)
# },Renv,FLenv,
# verbose = TRUE,
# expectation = c("fitS"))
# })





## Wide Table:
FLenv <- new.env(parent = globalenv())

FLenv$widetbl <- FLTable("tblautompg", "ObsID")
FLenv$widetbl@Dimnames[[2]] <- tolower(FLenv$widetbl@Dimnames[[2]])
Renv <- as.R(FLenv) ## todo: add temporary=FALSE

## JIRA:: https://fuzzyl.atlassian.net/browse/TDFL-838
## https://app.asana.com/0/150173007236461/374811135370878/f

## Coefficients dont match for wt.method as huber.
eval_expect_equal({
    fit <- rlm(weight ~ acceleration , data = widetbl)
     vcoef <- coef(fit)
},Renv,FLenv,
noexpectation=c("fit"),
expectation = "vcoef",
check.attributes=FALSE,
verbose = TRUE)

## Coefficients dont match for wt.method as bi.square.
eval_expect_equal({
    fit <- rlm(weight ~ acceleration , data = widetbl,psi="psi.bisquare",maxiter=500)
     vcoef <- coef(fit)
},Renv,FLenv,
noexpectation=c("fit"),
expectation = "vcoef",
check.attributes=FALSE,
verbose = TRUE)

# ##Prediction, Residuals
# eval_expect_equal({
#     pred <- predict(fit)
#     names(pred) <- NULL
#     res <- residuals(fit)
#     names(res) <- NULL
# },Renv,FLenv,
# noexpectation=c("pred","res"),
# check.attributes=FALSE,
# tolerance = 0.1,
# verbose = TRUE)

# ##Prediction, Residuals
# FLenv$pred <- predict(FLenv$fit)
# Renv$pred <- unname(predict(Renv$fit))
# FLexpect_equal(FLenv$pred, Renv$pred)
# FLexpect_equal(residuals(FLenv$fit), unname(residuals(Renv$fit)))


# ## Coefficients:
# eval_expect_equal({
#     fitC <- coefficients(fit)
#     names(fitC) <- NULL
# },Renv,FLenv,
# expectation=c("fitC"),
# check.attributes=FALSE,
# tolerance = 0.1,
# verboose = TRUE)
