#Function not in R
#FLMatrixREF

test_that("testing the example written in FLMatrixREF",{
  flmatrix <- FLMatrix(getOption("ResultDatabaseFL"), 
                        "tblMatrixMulti", 
                        5,
                        "MATRIX_ID",
                        "ROW_ID",
                        "COL_ID",
                        "CELL_VAL")
  resultFLMatrix <- FLMatrixREF(flmatrix)
  ##  print(resultFLMatrix)
})


## Testing FLMatrixREF
test_that("check FLMatrixREF",
{
    FLMatrixREF(initF.FLMatrix(n=5,isSquare=TRUE)$FL)
})
