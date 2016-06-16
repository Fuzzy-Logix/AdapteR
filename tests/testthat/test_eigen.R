Renv = new.env(parent = globalenv())
Renv$mat1 = cbind(c(1,-1), c(-1,1))
Renv$mat2 = cbind(1, c(1,-1))
Renv$mat3a = cbind(c(0, 1i), c(-1i, 0))
Renv$mat3 = cbind(-1, 2:1)
Renv$mat4 <- cbind( 1, 3:1, 1:3)
Renv$mat5 <- cbind(-1, c(1:2,0), 0:2)

## In order for examples to work in a chain, FLenv is needed
FLenv <- as.FL(Renv)

test_that("eigen1",{
    result = eval_expect_equal({
        e1 <- eigen(mat1)
        e12 <- eigen(mat1, symmetric = FALSE)
                                        # same (different algorithm).
    }, Renv, FLenv)
    print(result)
})

test_that("eigen only.values",{
    result = eval_expect_equal({
        e2 <- eigen(mat2, only.values = TRUE)
    print(result)
})

<<<<<<< HEAD
test_that("Check for eigen function",{
    result = eval_expect_equal({test1 =eigen(mat1)
                     test2 = eigen(mat1,symmetric = FALSE)
                     test3 = eigen(mat2,only.values = TRUE)
                     test4 = eigen(mat4)
                     tets5 = eigen(mat5)
                     test6 = eigen(mat6)},Renv)
=======
test_that("eigen",{
    result = eval_expect_equal({
        e4 <- eigen(mat4)
    }, Renv, FLenv)
>>>>>>> 7a9a63f498bdbdb6bec3f35e1cf2f3d5b120c538
    print(result)
})

<<<<<<< HEAD
# Hermittian matrix is not formed due to bug in FLMatrixArithematic.default
test_that("Check for eigen function with Hermittian matrix",{
    result = eval_expect_equal({test7 = eigen(mat3)
                     },Renv)
=======
test_that("eigen supports complex values",{
    result = eval_expect_equal({
        e2a <- eigen(print(mat2a)) # Hermite ==> real Eigenvalues
        ## 3 x 3:
        e3 <- eigen(mat3) # complex values
        e5 <- eigen(mat5) # complex values
    }, Renv, FLenv)
>>>>>>> 7a9a63f498bdbdb6bec3f35e1cf2f3d5b120c538
    print(result)
})
