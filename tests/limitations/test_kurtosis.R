Renv <- new.env(parent=globalenv())
set.seed(1234)
Renv$x <- rnorm(100)

FLenv <- as.FL(Renv)

test_that("kurtosis: https://fuzzyl.atlassian.net/projects/TDFL/issues/TDFL-752",{
  eval_expect_equal({
      test <- kurtosis(x)
  },Renv,FLenv, expectation="test")
})
