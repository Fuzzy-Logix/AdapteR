Renv <- new.env(parent = globalenv())
Renv$a <-1:25
Renv$b<-runif(25)
FLenv <- as.FL(Renv)
##https://app.asana.com/0/143316600934101/151568642664574
test_that("Testing weighted variance",
{
    result1<-eval_expect_equal({test1<-wt.var(a,b)},Renv,FLenv)
    ##    print(result1)
    
})


