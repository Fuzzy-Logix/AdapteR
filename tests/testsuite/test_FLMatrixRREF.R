#Function not in R
#FLMatrixRREF
test_that( "testing the example written in FLMatrixRREF",{
    flmatrix <- FLMatrix(getOption("ResultDatabaseFL"),
                         "tblMatrixMulti",
                          5,
                          "MATRIX_ID",
                          "ROW_ID",
                          "COL_ID",
                          "CELL_VAL")
    resultFLMatrix <- FLMatrixRREF(flmatrix)
    ##    print(resultFLMatrix)
 })

## Testing FLMatrixRREF
test_that("check FLMatrixRREF working",
{
  M <- initF.FLMatrix(n=5,isSquare=TRUE)$FL
  FLexpect_equal(
      dim(FLMatrixRREF(M)),
      dim(M)
  )
  FLexpect_equal(
      dimnames(FLMatrixRREF(M)),
      dimnames(M)
  )
})

