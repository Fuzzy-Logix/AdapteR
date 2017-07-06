#####################
## DBLytix Example
## test fails because for symmetric matrix e= PTranspose*H*P 
## is giving the desired matrix
test_that("check symmetric matrices are decomposed as P*H*PTranspose, FLMatrix from DBLYtix FLHessenbergDecomUdt example",
{
	flmatrix <- FLMatrix(getTestTableName("tblMatrixMulti"), 5,
							"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL",
							dims= c(5,5))
	resultList <- FLHessen(flmatrix)
	FLPMatrix <- resultList$P
    FLHMatrix <- resultList$H
    FLPTransMatrix <- t(FLPMatrix)
    e <- FLPMatrix %*% FLHMatrix %*% FLPTransMatrix
    FLexpect_equal(flmatrix, e, tolerance= 0.0001)
    PMatrix<- as.matrix(resultList$P)
    HMatrix<- as.matrix(resultList$H)
})
