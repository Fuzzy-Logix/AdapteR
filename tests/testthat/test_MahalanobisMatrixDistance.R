Renv = new.env(parent = globalenv())

Renv$mat1 = cbind(1:6, 1:3)
Renv$mat2 = var(Renv$mat1)
Renv$mat3 = matrix(rnorm(100*3), ncol = 3)
Renv$mat4 = cov(Renv$mat3)
D2 <- mahalanobis(x, colMeans(x), Sx)

FLenv = as.FL(Renv)

#Not in AdapteR.
test_that("Check for mahalanobis matrix distance method",{
         result = eval_expect_equal({test1 = mahalanobis(c(0, 0), 1:2, mat2) 
                                    },Renv,FLenv)
    })

test_that("Check for mahalanobis matrix distance method",{
         result = eval_expect_equal({test2 = mahalanobis(mat3, colMeans(mat3), mat4)
                                    },Renv,FLenv)
    })