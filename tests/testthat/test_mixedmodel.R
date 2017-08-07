## Results do not match with R's lmer.
## Please have a look at the file with the same name in limitations folder

# rm(list = setdiff(ls(),"connection"))
## implement as.R for FLSimpleVector.
## using 1 Random Effects.

FLenv <- new.env(parent = globalenv())
fltbl  <- FLTable(getTestTableName("tblMixedModel"), "ObsID")


## use in testthat.
mod <- lmer(yVal ~ FixVal + (1 | RanVal), data = fltbl)
 
## AIC, Log-Likehhood
test_that("AIC, LogLik:", {
	 vAkaike <- AIC(mod)
	 vLogLik <- logLik(mod)
})

## predict
test_that("predict:", {
    vpred <- as.vector(predict(mod))
    expect_equal(length(vpred), nrow(fltbl) )
})

## residuals.
test_that("residuals:", {
    vres <- residuals(mod)
    expect_equal(length(vres), nrow(fltbl))
})

## Covar Random
test_that("", {
    expect_equal(mod$CovarRandom,260.573310,tolerance = .1 )
})





