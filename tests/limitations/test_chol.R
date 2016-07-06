Renv = new.env(parent = globalenv())
Renv$x <- matrix(c(1:5, (1:5)^2), 5, 2)
Renv$x <- cbind(Renv$x, Renv$x[, 1] + 3*Renv$x[, 2])
colnames(Renv$x) <- letters[20:22]
Renv$mat1 = matrix(c(5,1,1,3),2,2)
Renv$mat3 = matrix(c(5,-5,-5,3), 2, 2)

FLenv <- as.FL(Renv)

#FL Works, R works only if pivot=TRUE, in which
# case FL and R outputs dont match
test_that("chol of positive semi-definite ",{
    result = eval_expect_equal({
        test2 = chol(mat2)},
        Renv,FLenv)
    ##print(result)
    })

#FL fails as expected. R works only if pivot=TRUE
test_that("chol of non-positive-definite https://app.asana.com/0/143316600934101/145335789954341s",{
    result = eval_expect_equal({
        test3 = chol(mat3,pivot = TRUE)},
        Renv,FLenv)
    ##print(result)
})
