
Renv = new.env(parent = globalenv())
Renv$mat1 = cbind(c(1,-1), c(-1,1))
Renv$mat2 = cbind(1, c(1,-1))
# Hermittian matrix
#Not forming
Renv$mat3 = cbind(c(0, 1i), c(-1i, 0)) 
# Complex values
Renv$mat4 = cbind(-1, 2:1)

Renv$mat5 = cbind( 1, 3:1, 1:3)

Renv$mat6 = cbind(-1, c(1:2,0), 0:2)

test_that("Check for eigen function",{
    result = eval_expect_equal({eigen(mat1)
                     eigen(mat1,symmetric = FALSE)
                     eigen(mat2,only.values = TRUE)
                     eigen(mat4)
                     eigen(mat5)
                     eigen(mat6)},Renv)
    print(result)
    })

# Hermittian matrix is not formed due to bug in FLMatrixArithematic.default
test_that("Check for eigen function with Hermittian matrix",{
    result = eval_expect_equal({eigen(mat3)
                     },Renv)
    print(result)
    })
