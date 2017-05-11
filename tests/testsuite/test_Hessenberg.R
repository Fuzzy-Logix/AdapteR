#No equivalent R function for Jordan Decomposition.
test_that("check Jordan Decomposition",
{
  M <- FLMatrix(getTestTableName("tblmatrixMulti"),5,
                "Matrix_id","ROW_ID","COL_ID","CELL_VAL")
  FLJordan(M)
})
#No equivalent function for hessenberg Decomposition.
test_that("check Hessenberg Decomposition",
{
    FLHessen(initF.FLMatrix(n=5,isSquare=TRUE)$FL)
})
