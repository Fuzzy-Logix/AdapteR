
## Testing M_Remainder
test_that("check result for M_Remainder",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix(getOption("ResultDatabaseFL"),"tblmatrixMulti",5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,10))
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,10))
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)
  ##
  FLexpect_equal((M1$FL%%M2),M1$R%%M2R,check.attributes=FALSE)
  FLexpect_equal((V1%%V2),V1R%%V2R,check.attributes=FALSE)
  FLexpect_equal((P1$FL%%P1$FL),P1$R%%P1$R,check.attributes=FALSE)
  FLexpect_equal((V1%%P1$FL),V1R%%P1$R,check.attributes=FALSE)
  FLexpect_equal((P1$FL%%V2),P1$R%%V2R,check.attributes=FALSE)
  FLexpect_equal((M1$FL%%V2),M1$R%%V2R,check.attributes=FALSE)
  FLexpect_equal((M1$FL%%P1$FL),M1$R%%P1$R,check.attributes=FALSE)
  FLexpect_equal((V1%%M2),V1R%%M2R,check.attributes=FALSE)
  FLexpect_equal((P1$FL%%M2),P1$R%%M2R,check.attributes=FALSE)
})


## Matrix Multiplication with dimnames
test_that("Matrix Multiplication with dimnames",{
  FLMatrixObj <- FLMatrix(getTestTableName("tblmatrixMulti"),5,
                          "matrix_id","row_id","col_id")
  FLMatrixObj2 <- as.FL(matrix(rnorm(25),5,dimnames=list(1:5,letters[1:5])))
  ResultRMatrixObj <- as.matrix(FLMatrixObj %*% FLMatrixObj2)
  })
