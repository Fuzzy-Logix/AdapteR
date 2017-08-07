Renv <- new.env(parent = globalenv())
Renv$a<-matrix(1:30, 5, 6)
FLenv <- as.FL(Renv)
test_that( "Testing transpose ",
{
    result1=eval_expect_equal({
        ta<-t(a)
    },
    Renv,
    FLenv,
    expectation= c("ta") )
})


test_that("comparing transpose of transpose between FLenv and Renv",
{
	
    eval_expect_equal({
      tta <- t(t(a))
    },Renv,
      FLenv,
      expectation= c("tta"))

    ##transpose of transpose of matrix
    FLexpect_equal(FLenv$a,t(t(FLenv$a)))
})

####################
## DBLytix Example
test_that("Testing if  Example from AdapteR documentation runs",{
	FLMatrixObj <- FLMatrix(getTestTableName("tblmatrixmulti"),5,
							"matrix_id",
							"row_id",
							"col_id",
							"cell_val",
							dims= c(5,5))
	ResultFLMatrix <- t(FLMatrixObj)
  ResultMatrix <- as.matrix(ResultFLMatrix)
})

