flmatrix <- FLMatrix(getOption("ResultDatabaseFL"),
                    "tblMatrixMulti",
                     5,
                     "MATRIX_ID",
                     "ROW_ID",
                     "COL_ID",
                     "CELL_VAL")

## gk @anany: this needs to be solve function tests, please delete and move and comment differences to solve
test_that("FLSolveExcl", {
  resultFLMatrix <- FLSolveExcl(flmatrix,3)
  })
