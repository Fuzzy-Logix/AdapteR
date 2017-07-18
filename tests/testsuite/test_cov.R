Renv = new.env(parent = globalenv())

Renv$a = 1:10
FLenv = as.FL(Renv)

test_that("check covarinace",{
  result = eval_expect_equal({
    test1=cov(a)
  },Renv,FLenv)
 
})
