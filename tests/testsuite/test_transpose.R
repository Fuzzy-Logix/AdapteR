Renv <- new.env(parent = globalenv())
Renv$a<-matrix(1:30, 5, 6)
FLenv <- as.FL(Renv)
test_that( "Testing transpose ",
{
    result1=eval_expect_equal({
        ta<-t(a)
    },Renv,FLenv)
    ##  print(result1)
})



test_that("Testing matrix multiplication with transpose ",
{
    eval_expect_equal({
        taXa <- ta %*% a
    },Renv,FLenv)
})



