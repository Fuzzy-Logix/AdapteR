#R example is just a function. Hence, this example is used as a test case
Renv <- new.env(parent= globalenv())

Renv$m <- as.matrix( FLMatrix("fuzzylogix","tblMatrixMulti", 6,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL"))
FLenv <- as.FL(Renv)

test_that("ginv: Moore-Penrose generalized inverse",{
  eval_expect_equal({
      resultMatrix <-  ginv(m)
  },Renv,FLenv)
})



############################################################
## initF based tests
test_that("ginv: Moore-Penrose generalized inverse",
{
    expect_eval_equal(initF.FLMatrix,
                      AdapteR::ginv,
                      MASS::ginv,
                      n=5)
})
