rm(list = setdiff(ls(),"connection"))
## implement as.R for FLSimpleVector.
## using 1 Random Effects.
FLenv <- new.env(parent = globalenv())
fltbl  <- FLTable("tblMixedModel", "ObsID")
Renv <- as.R(FLenv)
rtbl <- as.R(fltbl)

## use in 
FLenv$mod <- lmer(yVal ~ FixVal + (1 | RanVal), data = fltbl)
Renv$mod <- lmer(yVal ~ FixVal + (1 | RanVal), data = rtbl, REML = FALSE)
 
## AIC, Log-Likehhood
test_that("AIC, LogLik:", {eval_expect_equal({
    vAkaike <- AIC(mod)
    vLik <- logLik(mod)
    
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c("vAkaike", "vLik"))
})

## predict
test_that("predict:", {eval_expect_equal({
    vpred <- predict(mod)
    
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c("vpred"))
})

## residuals.
test_that("residuals:", {eval_expect_equal({
    vres <- residuals(mod)
    
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c("vpred"))
})

## Covar Random
test_that("", {
    expect_equal(FLenv$mod$CovarRandom,260.573310,tolerance = .001 )
})

## Coeff of Random Effect
test_that("Coeff of Random Effect", {
    expect_equal(FLenv$mod$u,Renv$mod@u,tolerance = .001 )
})


## dblytix Example:
FLenv <- new.env(parent = globalenv())
fltbl  <- FLTable("tblLinMixedModelWide2", "ObsID")
Renv <- as.R(FLenv)
rtbl <- as.R(fltbl)

FLenv$mod <- lmer(MathAch ~ CSes +(1 | School), maxiter = 100, data = fltbl)
Renv$mod <- lmer(MathAch ~ CSes +(1 | School), data = rtbl, REML = FALSE)

## AIC, Log-Likehhood
test_that("AIC, LogLik:", {eval_expect_equal({
    vAkaike <- AIC(mod)
    vLik <- logLik(mod)
    
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c("vAkaike", "vLik"))
})

## predict
test_that("predict:", {eval_expect_equal({
    vpred <- predict(mod)
    
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c("vpred"))
})

## residuals.
test_that("residuals:", {eval_expect_equal({
    vres <- residuals(mod)
    
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c("vpred"))
})



## Coeff of Random Effect
test_that("Coeff of Random Effect", {
    expect_equal(FLenv$mod$u,Renv$mod@u,tolerance = .001 )
})
