###########################
## DBLytix check correctness of decomposition, flm= P*J*PInv
flm<- FLMatrix(getTestTableName("tblMatrixMulti"), 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")

test_that("FLJordan(flm): FLJordan(flm)$P %*% diag(FLJordan(flm)$J) %*% FLJordan(flm)$PInv == flm",{
	FLResultList <- FLJordan(flm)
	e= FLResultList$P %*% diag(FLResultList$J) %*% FLResultList$PInv
	FLexpect_equal(flm,e)
})


############################
# init tests



############################
## DBLytix Example
test_that("Testing if basic DBLytix FLJordanDecompUdt-FLJordan Example runs from AdapteR",{
	flmatrix <- FLMatrix(getTestTableName("tblMatrixMulti"),5,
						 "MATRIX_ID",
						 "ROW_ID",
						 "COL_ID",
						 "CELL_VAL")
	resultList <- FLJordan(flmatrix)
	resultList$J
	resultList$P
	resultList$PInv
})
