#Function not in R
#FLMatrixREF

test_that("testing the example written in FLMatrixREF",{
  flmatrix <- FLMatrix("FL_DEMO", "tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
  resultFLMatrix <- FLMatrixREF(flmatrix)
  print(resultFLMatrix)
  
 })




  