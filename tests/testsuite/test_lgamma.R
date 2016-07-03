#testing special functions- lgamma
Renv <- new.env(parent = globalenv())
Renv$a <-1:10
FLenv <- as.FL(Renv)
test_that(
  "Testing lgamma",
  {
    result1=eval_expect_equal({test1<-lgamma(Renv$a)},Renv,FLenv)
    print(result1)
    
  }
)