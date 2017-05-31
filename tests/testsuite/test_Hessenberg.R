#####################
#No equivalent function for hessenberg Decomposition in R.

## initF based tests
test_that("check non-symmetric matrices are decomposed as P*H*PTranspose, FLMatrix generated using initF",
{
	flm <- initF.FLMatrix(n=5,isSquare=TRUE)$FL
    resultList <- FLHessen(flm)
    FLPMatrix <- resultList$P
    FLHMatrix <- resultList$H
    FLPTransMatrix <- t(FLPMatrix)
    e <- FLPMatrix %*% FLHMatrix %*% FLPTransMatrix
    FLexpect_equal(flm, e, tolerance= 0.0001)
    PMatrix<- as.matrix(resultList$P)
    HMatrix<- as.matrix(resultList$H)
})

