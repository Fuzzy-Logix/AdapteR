Renv = new.env(parent = globalenv())
Renv$mat1 = cbind(1, 1:3, c(2,0,1))
Renv$mat2 =  matrix(1:4, ncol = 2)


## In order for examples to work in a chain, FLenv is needed
FLenv <- as.FL(Renv)

test_that("Check for determinant function ",{
<<<<<<< HEAD
    result = eval_expect_equal({test1 = det(mat1)
                                tets2 = det(mat2)},Renv)
=======
    result = eval_expect_equal({
        e1 <- eigen(mat1)
        e2 <- det(mat1)
        e3 <- det(mat2)},
        Renv,FLenv)
>>>>>>> 7a9a63f498bdbdb6bec3f35e1cf2f3d5b120c538
    print(result)
    })
