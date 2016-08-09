Renv = new.env(parent = globalenv())
Renv$mat1 = matrix(c(5,1,1,3),2,2)

FLenv <- as.FL(Renv)

test_that("chol of positive definite matrix ",{
    result = eval_expect_equal({
        test1 = chol(mat1)
    }, Renv,FLenv,
    ##verbose=T,
    expectation = "test1")
})

## Testing FLCholskeyDecomp
## needs a hermitian positive definite matrix as input
test_that("check FLCholskeyDecomp",
{
    m4 <- FLMatrix(table="tblmatrixMulti",5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
    expect_equal(as.matrix(chol(m4)), Matrix::chol(as.matrix(m4)))
})
