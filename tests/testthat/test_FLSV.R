
flmatrix <- FLMatrix("fuzzylogix","tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")

test_that("computes the singular values for FLMatrix objects(FLSV)", {
  resultFLMatrix <- FLSV(flmatrix,3)
  })
  

