#Initialisation of data.
Renv = new.env(parent = globalenv())
N = 100
upper = 100 # For upper = 100 , results were different.
lower = -100 # For lower = 100 , results were different.
Renv$mat1 = matrix(runif(N^2,lower,upper),nrow = N,ncol = N,byrow = TRUE)
FLenv = as.FL(Renv)

#Default value is set to be 1 i.e. will run for 1 time.
test_that("diag on large matrix with large values: https://app.asana.com/0/143316600934101/150232474441202", {
    result1=eval_expect_equal({
        test1=det(mat1)}
       ,Renv,FLenv,
        expectation = "test1",
        tolerance=1e-7, scale=det(Renv$mat1),
        check.attributes = FALSE)
})


###########################3333
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
    }, Renv,FLenv,platforms=c("TDAster"))
})
