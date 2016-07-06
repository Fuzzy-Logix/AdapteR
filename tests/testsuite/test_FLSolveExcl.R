flmatrix <- FLMatrix(getOption("ResultDatabaseFL"),
                    "tblMatrixMulti",
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
test_that("check FLSolveExcl",
{
  M <- initF.FLMatrix(n=5,isSquare=TRUE)$FL
    FLexpect_equal(dim(FLSolveExcl(M,3)),dim(M)-1)
    FLexpect_equal(dim(FLSolveExcl(M,6)),dim(M))
})
