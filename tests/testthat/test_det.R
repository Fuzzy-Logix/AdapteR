Renv = new.env(parent = globalenv())
Renv$mat1 = cbind(1, 1:3, c(2,0,1))
Renv$mat2 =  matrix(1:4, ncol = 2)


test_that("Check for determinant function ",{
    result = eval_expect_equal({eigen(mat1)
                     det(mat1)
                     det(mat2)},Renv)
    print(result)
    })
