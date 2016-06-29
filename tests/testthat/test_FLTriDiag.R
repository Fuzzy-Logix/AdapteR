# Initialized a matrix using tblMatrixMulti table of database.
testmatrix <- FLMatrix(getOption("ResultDatabaseFL"), 
"tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")

#Computes Hessenberg matrix
#Outputs the class of result and result.
test_that("Check for FLTriDiag function which calculates Hessenberg upper diagnol matrix",{
    result <- FLTriDiag(testmatrix)
    print(result)
    print(paste0("Class of result is :",class(result)))

    })
