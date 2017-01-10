
## gk @anany: this needs to be svd function tests, please delete and move
test_that("computes the singular values for FLMatrix objects(FLSV)", {
    flmatrix <- FLMatrix(getTestTableName("tblMatrixMulti"),
                         5,
                         "MATRIX_ID",
                         "ROW_ID",
                         "COL_ID",
                         "CELL_VAL")
    resultFLMatrix <- FLSV(flmatrix,3)
})


## Testing FLSV
test_that("check FLSV working",
{
  M <- initF.FLMatrix(n=3,isSquare=TRUE)$FL
    FLexpect_equal(
              length(FLSV(M)),
              nrow(M)
          )
})


