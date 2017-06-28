rm(list = setdiff(ls(),"connection"))
## implement as.R for FLSimpleVector.
## using 1 Random Effects.
FLenv <- new.env(parent = globalenv())
fltbl  <- FLTable(getTestTableName("tblMixedModel"), "ObsID")

## use in 
FLenv$mod <- mixUDT(yVal ~ FixVal + (1 | RanVal), data = fltbl)


## AIC, Log-Likehhood
test_that("AIC, LogLik:", {
	expect_equal(FLenv$mod$logLik,6970.309,tolerance = .001 )
    expect_equal(FLenv$mod$AIC,6974.309,tolerance = .001 )  
})

## Covar Random
test_that("Covar Random", {
    expect_equal(FLenv$mod$CovRandom,260.573310,tolerance = .001 )
})

## fixedcoef and u
test_that("comparing length of fixed coef and u", {
	expect_equal(1 + length(FLenv$mod$fixedcoef),length(unique(as.vector(fltbl[,"FixVal"]))))
	expect_equal(length(FLenv$mod$u),length(unique(as.vector(fltbl[,"RanVal"]))))
})


## for 2 Random Effect:
FLenv <- new.env(parent = globalenv())
fltbl  <- FLTable(getTestTableName("tblMixedModelInt"), "ObsID")

FLenv$mod <- mixUDT(yVal ~ FixVal + (1 |   RanVal1) + (1 | RanVal2 ), fltbl)

## AIC, Log-Likehhood
test_that("AIC, LogLik:", {expect_equal(FLenv$mod$logLik,38.6181,tolerance = .001 )
    expect_equal(FLenv$mod$AIC,42.6181,tolerance = .001 )  })

## Covar Random
test_that("Covar Random", {
    expect_equal(FLenv$mod$CovRandom,c(0.01482593 ,2.38859471),tolerance = .001 )
})
