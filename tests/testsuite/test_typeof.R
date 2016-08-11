

## Testing M_Multiplication
test_that("typeof: matrix, vector and expressions",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix("tblmatrixMulti",5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
  expect_equal(typeof(M2),"double")
  expect_equal(typeof(M1$FL),"double")
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,10))
  expect_equal(typeof(V2),"integer")
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,10))
  expect_equal(typeof(V2),"integer")
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)
  expect_equal(typeof(P1$FL),"double")

  expect_equal(typeof(P1$FL*P1$FL),"double")
  expect_equal(typeof(V1*V2*M2*P1$FL*M1$FL),"double")
  expect_equal(typeof(P1$FL*P1$FL*V1*V2*M2*P1$FL*M1$FL),"double")
  ## gk: please add more tests systematically
})
