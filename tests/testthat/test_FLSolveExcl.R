flmatrix <- FLMatrix("fuzzylogix","tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")

test_that("FLSolveExcl", {
  resultFLMatrix <- FLSolveExcl(flmatrix,3)
  })
