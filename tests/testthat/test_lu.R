Renv <- new.env(parent = globalenv())
Renv$x <- Matrix(rnorm(9),3,3)
library(Matrix)
Renv$pm <- as(readMM(system.file("external/pores_1.mtx",
                            package = "Matrix")),
                "CsparseMatrix")

FLenv <- as.FL(Renv)

test_that("LU on dense square matrix running",
eval_expect_equal({
  result1 <- lu(x)
},Renv,FLenv,check.attributes=FALSE))

## fails.. different results in R and FL
test_that("exapnd LU on dense square matrix ",
eval_expect_equal({
  result2 <- expand(result1)
  print(result2)
},Renv,FLenv,check.attributes=FALSE))

## No round available yet
test_that("LU on dense non-square matrix",
          eval_expect_equal({
            result3 <- round(10 * x[,-3])
            result4 <- dim(result3)
            result5 <- lu(result3)
            result6 <- expand(result5)
          },Renv,FLenv,check.attributes=FALSE))

## No drop0 function available yet
test_that("Sparse LU",
        eval_expect_equal({
            pmLU <- lu(pm)
            str(pmLU)
            ppm <- pm[pmLU@p + 1L, pmLU@q + 1L]
            pLU <- drop0(pmLU@L %*% pmLU@U) # L %*% U -- dropping extra zeros
            #pLU <- pmLU@L %*% pmLU@U # L %*% U -- dropping extra zeros
            result7 <- ppm[1:14, 1:5]
            result8 <- pLU[1:14, 1:5]
        },Renv,FLenv,check.attributes=FALSE))
