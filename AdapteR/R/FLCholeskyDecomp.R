#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

chol <- function (x, ...){
  UseMethod("chol", x)
}
#' Cholesky Decomposition.
#'
#' \code{chol} computes the Cholesky factorization of FLMatrix object.\cr
#' The Cholesky decomposition is a decomposition of a positive definite matrix 
#' into the product of a lower triangular matrix and its conjugate transpose.
#'
#' The wrapper overloads chol and implicitly calls FLCholeskyDecompUdt.
#' @param object is of class FLMatrix
#' @section Constraints:
#' Input can only be a Hermitian, positive definite square matrix (n x n)
#' with maximum dimension limitations of (1000 x 1000)
#' @return \code{chol} returns FLMatrix which is the upper triangular factor of the Cholesky decomposition
#' @examples
#' connection<-odbcConnect("Gandalf")
#' flmatrix<-FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLMatrix <- chol(flmatrix)
#' @export

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
                        odbc_connection = connection,
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

	return(t(store(object=flm)))
}
