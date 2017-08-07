############################################################
## R base::svd example based tests
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }

Renv <- new.env(parent = globalenv())
Renv$A <- hilbert(6)

FLenv <- as.FL(Renv)

test_that("Result comparision : svd ",{
	eval_expect_equal({
		s <- svd(A)
		D <- s$d 	#diagonal matrix of both environment must be same
		},
		Renv,
		FLenv,
		expectation=c("D"),
		noexpectation= c("s"),
		tolerance= 0.0001)
	})

### https://app.asana.com/0/136555696724838/354164600450647
test_that("Check whether U & V matrices are unitary",{
	flm <- FLenv$A
	FLsvd <- svd(flm)
	U <- FLsvd$u
	V <- FLsvd$v
	I <- as.FL(diag(6))

	FLexpect_equal(U %*% t(U), I, check.attributes= FALSE)
	FLexpect_equal(V %*% t(V), I, check.attributes= FALSE)
	})


###########################
## DBLytix check correctness of decomposition, flm= U*D*transpose(V)
flm<- FLMatrix(getTestTableName("tblMatrixMulti"), 5,
				"MATRIX_ID",
				"ROW_ID",
				"COL_ID",
				"CELL_VAL", 
				dims= c(5,5))

test_that("svd(flm): svd(flm)$u %*% diag(svd(flm)$d) %*% t(svd(flm)$v) == flm",{
	FLResultList <- svd(flm)
	e= FLResultList$u %*% diag(FLResultList$d) %*% t(FLResultList$v)
	FLexpect_equal(flm,e, check.attributes= FALSE)
})


####################
## DBLytix Example
test_that("Testing if basic DBLytix FLMatrixSVDUdt-svd Example runs from AdapteR",{
	flmatrix <- FLMatrix(getTestTableName("tblmatrixmulti"),5,
							"matrix_id",
							"row_id",
							"col_id",
							"cell_val",
							dims= c(5,5))
	
	resultList <- svd(flmatrix)
	resultvectorD <-  as.vector(resultList$d)
	resultMatrixU <- as.matrix(resultList$u)
	resultMatrixV <- as.matrix(resultList$v)
	
})