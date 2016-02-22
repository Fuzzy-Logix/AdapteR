#' @include utilities.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

#' Cholesky Decomposition.
#'
#' \code{chol} computes the Cholesky factorization of FLMatrix object.\cr
#' The Cholesky decomposition is a decomposition of a positive definite matrix 
#' into the product of a lower triangular matrix and its conjugate transpose.
#'
#' The wrapper overloads chol and implicitly calls FLCholeskyDecompUdt.
#' @param x is of class FLMatrix
#' @section Constraints:
#' Input can only be a Hermitian, positive definite square matrix (n x n)
#' with maximum dimension limitations of (1000 x 1000)
#' @return \code{chol} returns FLMatrix which is the upper triangular factor of the Cholesky decomposition
#' @examples
#' connection<-odbcConnect("Gandalf")
#' flmatrix<-FLMatrix(connection, "FL_DEMO", "tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLMatrix <- chol(flmatrix)
#' @export
chol <- function (x, ...){
  UseMethod("chol", x)
}

# chol.default <- base::chol

chol.FLMatrix<-function(object)
{
	connection<-getConnection(object)
	flag1Check(connection)
	
	# checkSquare(object,"chol")
	# checkHermitianPositiveDefinite(object)

	sqlstr<-paste0(
					viewSelectMatrix(object,"a",withName="z"),
					outputSelectMatrix("FLCholeskyDecompUdt",viewName="z",
							localName="a",includeMID=TRUE,vconnection=connection)
                   )

	tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = connection,
                        variables=list(
                            rowIdColumn="OutputColNum",
                            colIdColumn="OutputRowNum",
                            valueColumn="OutputVal"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

	flm <- new("FLMatrix",
	            select= tblfunqueryobj,
	            dimnames=dimnames(object))

	vResult <- (ensureQuerySize(pResult=flm,
            pInput=list(object),
            pOperator="chol",
            pStoreResult=TRUE))
	vResult <- store(vResult)

	return(t(vResult))
}
