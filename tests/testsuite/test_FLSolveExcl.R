flmatrix <- FLMatrix(getTestTableName("tblMatrixMulti"),
                     5,
                     "MATRIX_ID",
                     "ROW_ID",
                     "COL_ID",
                     "CELL_VAL")

## gk: this needs to be solve function tests, please delete and move and comment differences to solve
test_that("FLSolveExcl", {
  resultFLMatrix <- FLSolveExcl(flmatrix,3)
  })


## Testing FLSolveExcl
#The Following test fails on Aster. ApI difference from TD
test_that("check FLSolveExcl",
{
  M <- as.FLMatrix(matrix(rnorm(25),5))
  FLexpect_equal(dim(FLSolveExcl(M,4)),dim(M)-1)
  FLexpect_equal(dim(FLSolveExcl(M,6)),dim(M))
})


