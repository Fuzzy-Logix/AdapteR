Renv = new.env(parent = globalenv())
Renv$mat1 = cbind(c(1,-1), c(-1,1))
Renv$mat3 = cbind(-1, 2:1)
Renv$mat3a = cbind(c(0, 1i), c(-1i, 0))
Renv$mat5 <- cbind(-1, c(1:2,0), 0:2)

FLenv <- as.FL(Renv)

## complex eigen values not supported
# Hermittian matrix is not formed due to bug in FLMatrixArithematic.default
test_that("eigen: support of complex values",{
    result = eval_expect_equal({
        e3 <- eigen(mat3) # complex values
        e5 <- eigen(mat5) # complex values
    }, Renv, FLenv)
    ##print(result)
})

test_that("eigen: support of complex values",{
    result = eval_expect_equal({
        e1val <- eigen(mat1)$values
        e1vec <- eigen(mat1)$vectors
    }, Renv,FLenv)
})
