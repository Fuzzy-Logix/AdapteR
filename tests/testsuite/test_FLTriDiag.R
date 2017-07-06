
#Outputs the class of result and result.
test_that("Check for FLTriDiag function which calculates Hessenberg upper diagonal matrix",{
    ## Initialized a matrix using tblMatrixMulti table of database.
    testmatrix <- FLMatrix(getTestTableName("tblMatrixMulti"), 5,
    					   "MATRIX_ID","ROW_ID","COL_ID","CELL_VAL",
    					   	dims= c(5,5))
    resultFLMatrix <- FLTriDiag(testmatrix)
    resultMatrix <- as.matrix(resultFLMatrix)
    ##    print(result)
    ##print(paste0("Class of result is :",class(result)))
})

## Testing FLTriDiag
test_that("check FLTriDiag",
{
    resultMatrix <- as.matrix(FLTriDiag(initF.FLMatrix(n=5,isSquare=TRUE)$FL))
})
