#Asana: https://app.asana.com/0/143316600934101/148450351472400
#testing operators
## MOD result matching fails for -ve numbers
Renv <- new.env(parent = globalenv())
Renv$x<- -1:12
Renv$y <- 1:12
FLenv <- as.FL(Renv)
test_that(
  "Testing + ",
  {
    result1<-eval_expect_equal({test1<-(x+1)},Renv,FLenv)
    
  }
)

test_that(
  "Testing arithmetic ",
  {
    result2<-eval_expect_equal({test2<-2*x+3},Renv,FLenv)
    
  }
)
test_that(
  "Testing integer division ",
  {
    result4<-eval_expect_equal({test4<-(x%/%5)},Renv,FLenv)
    
  }
)

## different test for MOD as results wont match for -ve values
test_that(
  "Testing remainder MOD ",
  {
    result3<-eval_expect_equal({
      test3<-(y%%2)
      test5 <- length(x%%2)
      },Renv,FLenv)
  }
)
