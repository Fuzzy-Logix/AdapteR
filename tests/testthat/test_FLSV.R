
flmatrix <- FLMatrix("fuzzylogix","tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")

## gk @anany: this needs to be svd function tests, please delete and move
test_that("computes the singular values for FLMatrix objects(FLSV)", {
  resultFLMatrix <- FLSV(flmatrix,3)
  })
  

