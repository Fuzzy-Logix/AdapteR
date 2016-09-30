
## gk: these need fixing!!!!
## Testing M_Subtraction
test_that("-: vector and matrix subtraction, integer and double",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix("tblmatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,10))
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,10))
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)
  FLexpect_equal((V1-M2),V1R-M2R,check.attributes=FALSE)
  FLexpect_equal((V1/V2), V1R/V2R, check.attributes=FALSE)
})
