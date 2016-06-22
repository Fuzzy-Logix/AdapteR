#No Factorial function in FL currently
Renv <- new.env(parent = globalenv())
Renv$a <-c(1:7)
Renv$b<-100
FLenv <- as.FL(Renv)
test_that(
  "Testing factorial",
  {
    result1<-eval_expect_equal({test1<-factorial(a)},Renv,FLenv)
    print(result1)
    
  }
)
test_that(
  "Testing factorial",
  {
    result2<-eval_expect_equal({test2<-factorial(b)},Renv,FLenv)
    print(result2)
    
  }
)




