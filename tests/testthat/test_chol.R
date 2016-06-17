
x <- matrix(c(1:5, (1:5)^2), 5, 2)
x <- cbind(x, x[, 1] + 3*x[, 2])
colnames(x) <- letters[20:22]
m <- crossprod(x,x)

Renv = new.env(parent = globalenv())
Renv$mat1 = matrix(c(5,1,1,3),2,2)
Renv$mat2 = m
Renv$mat3 = matrix(c(5,-5,-5,3), 2, 2)

FLenv <- as.FL(Renv)

#In FLTestLib.R error comes in coercing non string type to paste function.
test_that("Check for Cholesky function ",{
    result = eval_expect_equal({
        test1 = chol(mat1)},
        Renv,FLenv)
    print(result)
    })
 
#Test failing for R , FL is giving its output..
test_that("Check for Cholesky function ",{
    result = eval_expect_equal({
        test2 = chol(mat2)},
        Renv,FLenv)
    print(result)
    })

#Test Failed
#Matrix not positive or semi definite.
#Asana Ticket - https://app.asana.com/0/143316600934101/145335789954341s
test_that("Check for Cholesky function ",{
    result = eval_expect_equal({
        test3 = chol(mat3,pivot = TRUE)},
        Renv,FLenv)
    print(result)
    })

