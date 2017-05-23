############################################################
## R base::solve example based tests

hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }

Renv <- new.env(parent = globalenv())
Renv$A <- hilbert(4)

FLenv <- as.FL(Renv)

test_that("Result comparision : solve with ax=b equation solving ",{
	eval_expect_equal({
		X <- solve(A,A)
		},
		Renv,
		FLenv,
		expectation=c("X"))
	})

test_that("solve(m): m %*% solve(m) == I", {
    eval_expect_equal({
        aI <- A %*% solve(A)
    }, Renv, FLenv,
    expectation=c("aI"))
})


## Fails as singular matrices exist in Hadoop's ARTestMatrixResult Table
## (The big table from where smaller matrices are subsetted in initF)
############################################################
## initF based tests
test_that("solve(m), matrices with small numbers", {
    expect_eval_equal(initF.FLMatrix,
                      AdapteR::solve,
                      base::solve,
                      n=5,
                      isSquare=TRUE)
})

####################
## DBLytix Example
test_that("Testing if basic DBLytix FLMatrixInvUdt-solve Example runs from AdapteR",{
	FLMatrixObj <- FLMatrix(getTest

		sTableName("tblmatrixmulti"),5,
							"matrix_id",
							"row_id",
							"col_id",
							"cell_val")
	ResultFLMatrixObj <- solve(FLMatrixObj)
})



