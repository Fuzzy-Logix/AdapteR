Renv <- new.env(parent = globalenv())
Renv$x <- Matrix(rnorm(9),3,3)
Renv$result3 <- round(10 * Renv$x[,-3])
library(Matrix)
Renv$pm <- as(readMM(system.file("external/pores_1.mtx",
                                 package = "Matrix")),
              "CsparseMatrix")

FLenv <- as.FL(Renv)

Renv$luresult <- expand(lu(Renv$x))
FLenv$luresult <- expand(lu(FLenv$x))

test_that("LU on dense square matrix running",
          eval_expect_equal({
            result1 <- lu(x)
          },Renv,FLenv,check.attributes=FALSE))

## fails in R environment.. 
## R's default cross-prod not implemented for p-matrix
## so testing only for FLenv
test_that("Check PM=LU ",{
  FLexpect_equal((FLenv$luresult$P %*% FLenv$x)
                ,(FLenv$luresult$L %*% FLenv$luresult$U))
})

test_that("Check L,U triangulairty ",{
  result = eval_expect_equal({
    vupper <- matrix(as.matrix(luresult$U),dim(luresult$U))
    vtemp1 <- vupper[lower.tri(vupper)]
    FLexpect_equal(rep(0,length(vtemp1))
                  ,vtemp1)
    vlower <- matrix(as.matrix(luresult$L),dim(luresult$L))
    vtemp2 <- vlower[upper.tri(vlower)]
    FLexpect_equal(rep(0,length(vtemp2))
                  ,vtemp2)
    rm(vtemp1)
    rm(vtemp2)
    rm(vupper)
    rm(vlower)
  },Renv,FLenv)
  ##print(result)
})

# ## fails.. different results in R and FL
# test_that("exapnd LU on dense square matrix https://app.asana.com/0/143316600934101/145318689357916 ",
# eval_expect_equal({
#   result2 <- expand(result1)
#   print(result2)
# },Renv,FLenv,check.attributes=FALSE))

# ## fails..No round available yet
# test_that("LU on dense non-square matrix",
#           eval_expect_equal({
#             result3 <- round(10 * x[,-3])
#             result4 <- dim(result3)
#             result5 <- lu(result3)
#             result6 <- expand(result5)
#           },Renv,FLenv,check.attributes=FALSE))

# ## fails..No drop0 function available yet
# test_that("Sparse LU",
#         eval_expect_equal({
#             pmLU <- lu(pm)
#             str(pmLU)
#             ppm <- pm[pmLU@p + 1L, pmLU@q + 1L]
#             pLU <- drop0(pmLU@L %*% pmLU@U) # L %*% U -- dropping extra zeros
#             #pLU <- pmLU@L %*% pmLU@U # L %*% U -- dropping extra zeros
#             result7 <- ppm[1:14, 1:5]
#             result8 <- pLU[1:14, 1:5]
#         },Renv,FLenv,check.attributes=FALSE))
