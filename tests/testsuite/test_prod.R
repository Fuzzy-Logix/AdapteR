#testing product function
Renv <- new.env(parent = globalenv())
Renv$a <-1:7
Renv$b <- matrix(rnorm(9),3)
FLenv <- as.FL(Renv)
test_that("Testing prod", {
    result1=eval_expect_equal({
        test1<-prod(a)
        test2 <- prod(b)
        test3 <- prod(b,a)
        },Renv,FLenv)
    ##print(result1)
  })
