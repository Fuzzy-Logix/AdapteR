
library(moments)

Renv <- new.env(parent=globalenv())
set.seed(1234)
Renv$x <- rnorm(10)

FLenv <- as.FL(Renv)

test_that("Check skewness Results https://app.asana.com/0/143316600934101/166099262880558 ",{
  eval_expect_equal({
    test <- skewness(x)
  },Renv,FLenv)
})
