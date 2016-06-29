#testing product function
Renv <- new.env(parent = globalenv())
Renv$a <-1:7
FLenv <- as.FL(Renv)
test_that(
  "Testing prod",
  {
    result1=eval_expect_equal({test1<-prod(a)},Renv,FLenv)
    print(result1)
    
  }
)



