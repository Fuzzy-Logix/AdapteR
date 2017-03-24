rm(list = setdiff(ls(),"connection"))
## using 1 Random Effects.
FLenv <- new.env(parent = globalenv())
fltbl  <- FLTable("tblMixedModel", "ObsID")
Renv <- as.R(FLenv)
rtbl <- as.R(fltbl)

FLenv$mod <- lmer(yVal ~ (FixVal | RanVal), data = fltbl)
Renv$mod <- lmer(yVal ~ FixVal + (1 | RanVal), data = rtbl)




##
##FLenv <- new.env(parent = globalenv())
##FLenv$tbl  <- FLTable("tblMixedModel", "ObsID")
##Renv <- as.R(FLenv)
##
##eval_expect_equal({
##    mod <- lmer(lmer(yVal ~ (FixVal | RanVal), data = tbl))
##    },Renv,FLenv,
##    expectations = "mod")


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

## predict
test_that("residuals:", {eval_expect_equal({
    vres <- residuals(mod)
    
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c("vpred"))
})


## using 2 Random Effect.
FLenv <- new.env(parent = globalenv())
fltbl  <- FLTable("tblMixedModelInt", "ObsID")
Renv <- as.R(FLenv)
rtbl <- as.R(fltbl)

FLenv$mod <- lmer(yVal ~ (FixVal |   RanVal1) + (1 | RanVal2 ), data = fltbl)
Renv$mod <- lmer(yVal ~ (FixVal |   RanVal1) + (1 | RanVal2 ), data = rtbl)
##eval_expect_equal({
##    mod <- lmer(yVal ~ (FixVal |   RanVal1) + (1 | RanVal2 ), tbl)
##    },Renv,FLenv,
##    expectations = "mod")
##

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
test_that("AIC, LogLik:", {eval_expect_equal({
    vpred <- predict(mod)
    
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c("vpred"))
})


## predict
test_that("residuals:", {eval_expect_equal({
    vres <- residuals(mod)
    
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c("vpred"))
})
