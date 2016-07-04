Renv <- new.env(parent = globalenv())
Renv$a <-1:25
Renv$b<-runif(25)
FLenv <- as.FL(Renv)
test_that("Testing weighted variance",
{
    result1<-eval_expect_equal({test1<-wt.var(a,b)},Renv,FLenv)
    ##    print(result1)
    
})


