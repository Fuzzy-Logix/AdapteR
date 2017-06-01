#R example is just a function. Hence, this example is used as a test case
Renv <- new.env(parent= globalenv())

Renv$m <- as.matrix( FLMatrix(getTestTableName("tblMatrixMulti"), 
                    6,
                    "MATRIX_ID",
                    "ROW_ID",
                    "COL_ID",
                    "CELL_VAL",
                    dims= c(10,10)))
FLenv <- as.FL(Renv)

test_that("ginv: Moore-Penrose generalized inverse",{
  eval_expect_equal({
      resultMatrix <-  ginv(m)
  },Renv,FLenv,
    tolerance= 0.0001)
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

#####################################################
## Testcase checking conditions for pseudo-inverse
test_that("check Moore-Penrose Inverse conditions",
{
    flmatrix <- as.FL(matrix(rnorm(25),5,5))
    flmatrixginv <- ginv(flmatrix)

    #If A is the given matrix and C is its pseudo inverse, then
    # (i) ACA= A
    # (ii) CAC= C
    # (iii) transpose(AC)= AC
    # (iv) transpose(CA)= CA
    FLexpect_equal(flmatrix %*% flmatrixginv %*% flmatrix, flmatrix, tolerance= 0.0001)
    FLexpect_equal(flmatrixginv %*% flmatrix %*% flmatrixginv, flmatrixginv, tolerance= 0.0001)
    FLexpect_equal(t(flmatrix %*% flmatrixginv), flmatrix %*% flmatrixginv, tolerance= 0.0001)
    FLexpect_equal(t(flmatrixginv %*% flmatrix), flmatrixginv %*% flmatrix, tolerance= 0.0001)
})
