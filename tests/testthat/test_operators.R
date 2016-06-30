#Asana: https://app.asana.com/0/143316600934101/148450351472400
#testing operators
Renv <- new.env(parent = globalenv())
Renv$x<- -1:12
FLenv <- as.FL(Renv)
test_that(
  "Testing",
  {
    result1<-eval_expect_equal({test1<-(x+1)},Renv,FLenv)
    print(result1)
    
  }
)

test_that(
  "Testing",
  {
    result2<-eval_expect_equal({test2<-2*x+3},Renv,FLenv)
    print(result2)
    
  }
)

test_that(
  "Testing",
  {
    result3<-eval_expect_equal({test3<-(x%%2)},Renv,FLenv)
    print(result3)
    
  }
)
test_that(
  "Testing",
  {
    result4<-eval_expect_equal({test4<-(x%/%5)},Renv,FLenv)
    print(result4)
    
  }
)






