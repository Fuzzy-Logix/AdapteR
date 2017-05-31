##Testing FLTrace
Renv = new.env(parent = globalenv())
Renv$mat1 = cbind(1, 1:3, c(2,1,3))
FLenv <- as.FL(Renv)

test_that("Check for trace function ",{
    result = eval_expect_equal({
        e <- tr(mat1)
    }, 	Renv,FLenv,
    	expectation= c("e"))
})

## Testing FLTrace, initF based test
test_that("check FLTrace",
{
    expect_eval_equal(initF.FLMatrix,
                    AdapteR::tr,
                    psych::tr,
                    n=5,
                    isSquare=TRUE)
})

####################
## DBLytix Example
test_that("Testing if basic DBLytix FLMatrixTraceUdt-solve Example runs from AdapteR",{
	FLMatrixObj <- FLMatrix(getTestTableName("tblmatrixmulti"),5,
							"matrix_id",
							"row_id",
							"col_id",
							"cell_val",
							dims= c(5,5))
	ResultFLVector <- tr(FLMatrixObj)
})
