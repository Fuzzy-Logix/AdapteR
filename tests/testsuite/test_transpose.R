Renv <- new.env(parent = globalenv())
Renv$a<-matrix(1:30, 5, 6)
FLenv <- as.FL(Renv)
test_that( "Testing transpose ",
{
  result1=eval_expect_equal({test1<-t(a)
  },Renv,FLenv)
  ##  print(result1)
  })



