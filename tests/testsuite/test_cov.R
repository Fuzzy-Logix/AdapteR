Renv = new.env(parent = globalenv())

Renv$a = 1:10
Renv$sw=swiss

FLenv = as.FL(Renv)

test_that("check covarinace",{
  result = eval_expect_equal({
    test1=cov(a)
#swiss gives error
    test2=cov(sw)
    
  },Renv,FLenv)
 
})
