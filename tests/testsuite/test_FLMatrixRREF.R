#Function not in R
#FLMatrixRREF
##options(debugSQL=T)
test_that( "testing the example written in FLMatrixRREF",{
    flmatrix <- FLMatrix("FL_DEMO", "tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
    resultFLMatrix <- FLMatrixRREF(flmatrix)
    ##    print(resultFLMatrix)
    
 })
