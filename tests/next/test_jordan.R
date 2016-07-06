
##Testing FLJordon
### works only with matrices with non-complex
### eigen values. So input taken from DbLytix manual.
test_that("check Jordan Decomposition",
{
  M <- FLMatrix(getOption("ResultDatabaseFL"),"tblmatrixMulti",5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
    FLJordan(M)
})


