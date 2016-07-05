Renv = new.env(parent = globalenv())
Renv$mat1 = matrix(c(5,1,1,3),2,2)

FLenv <- as.FL(Renv)

test_that("chol of positive definite matrix ",{
    result = eval_expect_equal({
        test1 = chol(mat1)
    }, Renv,FLenv,
    ##verbose=T,
    expectation = "test1")
})
 

