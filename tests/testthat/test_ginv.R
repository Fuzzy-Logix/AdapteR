#R example is just a function. Hence, this example is used as a test case
Renv <- new.env(parent= globalenv())

Renv$Rmatrix <- as.matrix( FLMatrix("fuzzylogix","tblMatrixMulti", 6,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL"))
FLenv <- as.FL(Renv)

test_that(" Moore-Penrose generalized inverse (ginv)",{
  eval_expect_equal({
      resultMatrix <-  ginv(Rmatrix)
  },Renv,FLenv)
})