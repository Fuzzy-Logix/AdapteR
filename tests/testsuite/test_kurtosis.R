library(moments)

Renv <- new.env(parent=globalenv())
set.seed(1234)
Renv$x <- rnorm(100)

FLenv <- as.FL(Renv)

test_that("Descriptive statistics (min)",{
  eval_expect_equal({
    test <- round(kurtosis(x),6)
  },Renv,FLenv)
})
