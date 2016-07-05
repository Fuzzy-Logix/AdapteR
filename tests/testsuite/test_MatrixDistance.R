Renv = new.env(parent = globalenv())

Renv$mat1 = matrix(rnorm(100), nrow = 5)
Renv$var2 =  c(0, 0, 1, 1, 1, 1)
Renv$var3 =  c(1, 0, 1, 1, 0, 1)

FLenv = as.FL(Renv)


test_that("Check for matrix distance with different arguments",{
           result = eval_expect_equal({ test1 = dist(mat1)
                               test2 = dist(mat1, diag = TRUE)
                               test3 = dist(mat1, upper = TRUE)
                               test10 = dist(mat1, method="manhattan")
                               test11 = dist(mat1,diag=T,upper=T,method="euclidean")
                               },Renv,FLenv,check.attributes=FALSE)
           ##           print(result)
    })

