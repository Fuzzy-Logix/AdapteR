
library(moments)

Renv <- new.env(parent=globalenv())
set.seed(1234)
Renv$x <- rnorm(10)

FLenv <- as.FL(Renv)

test_that("Descriptive statistics (skewness)",{
  eval_expect_equal({
    test <- skewness(x)
  },Renv,FLenv)
})
