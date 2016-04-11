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
#' @param object is of class FLMatrix
#' @param ... any additional arguments
#' @section Constraints:
#' Input can only be a Hermitian, positive definite square matrix (n x n)
#' with maximum dimension limitations of (1000 x 1000)
#' @return \code{chol} returns FLMatrix which is the 
#' upper triangular factor of the Cholesky decomposition
#' @examples
#' connection<-RODBC::odbcConnect("Gandalf")
#' flmatrix<-FLMatrix("FL_DEMO", 
#' "tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLMatrix <- chol(flmatrix)
#' @export
chol <- function (object, ...){
  UseMethod("chol", object)
}

# chol.default <- base::chol
#' @export
chol.FLMatrix<-function(object,...)
{
	connection<-getConnection(object)
	flag1Check(connection)

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
