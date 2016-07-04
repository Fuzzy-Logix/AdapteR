#Asana: https://app.asana.com/0/143316600934101/148450351472395
Renv <- new.env(parent = globalenv())
Renv$a<-matrix(1:30, 5, 6)
FLenv <- as.FL(Renv)
test_that( "Testing transpose ",
{
  result1=eval_expect_equal({test1<-t(a)
  #for(j in seq(ncol(a)))
  #if(! all(a[, j] == test1[j, ])) stop("wrong transpose")
  },Renv,FLenv)
  ##  print(result1)
  })



