Renv <- new.env(parent = globalenv())
Renv$x <- Matrix(rnorm(9),3,3)
Renv$result3 <- round(10 * Renv$x[,-3])
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



# # Testing FLLUDecomp
# test_that("check LU Decomposition",
# {
#   m <- initF.FLMatrix(n=5)
#   expect_equal(AdapteR::expand(AdapteR::lu(m$FL)),
#                Matrix::expand(Matrix::lu(m$R)),check.attributes=FALSE)
# })
