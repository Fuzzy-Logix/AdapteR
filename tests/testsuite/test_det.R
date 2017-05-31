Renv = new.env(parent = globalenv())
Renv$mat1 = cbind(1, 1:3, c(2,0,1))
Renv$mat2 =  matrix(1:4, ncol = 2)

FLenv <- as.FL(Renv)

##phani: Det of mat1 fails in Aster because
## of no sparse matrix support in DBLytixAster
test_that("Check for determinant function ",{
    result = eval_expect_equal({
        e2 <- det(mat1)
        e3 <- det(mat2)
    }, 	Renv,FLenv,
    	expectation= c("e2","e3"),
    	platforms=c("TD","Hadoop"))
})
