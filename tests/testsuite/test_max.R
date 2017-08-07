Renv = new.env(parent = globalenv())

Renv$v = 1:10
Renv$a=1:5
Renv$b=pi

FLenv = as.FL(Renv)

test_that("check max",{
  result = eval_expect_equal({
    test1=max(v)
    test2=max(a,b)
    
  },Renv,FLenv)
  
})
