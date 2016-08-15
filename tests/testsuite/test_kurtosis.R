Renv <- new.env(parent=globalenv())
set.seed(1234)
Renv$x <- rnorm(1000)

FLenv <- as.FL(Renv)

test_that("kurtosis",{
  eval_expect_equal({
      test <- kurtosis(x)
  },Renv,FLenv, expectation="test")
})
