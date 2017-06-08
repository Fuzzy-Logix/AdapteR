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
		expectation=c("X"),
		tolerance= 0.0001)
	})

test_that("solve(m): m %*% solve(m) == I", {
    eval_expect_equal({
        aI <- A %*% solve(A)
    }, Renv, FLenv,
    expectation=c("aI"),
    tolerance= 0.0001)
})


####################
## DBLytix Example
test_that("Testing if basic DBLytix FLMatrixInvUdt-solve Example runs from AdapteR",{
	FLMatrixObj <- FLMatrix(getTestTableName("tblmatrixmulti"),5,
							"matrix_id",
							"row_id",
							"col_id",
							"cell_val",
							dims= c(5,5))
	ResultFLMatrixObj <- solve(FLMatrixObj)
	
})



