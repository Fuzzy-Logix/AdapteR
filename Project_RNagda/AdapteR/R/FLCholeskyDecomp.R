#' @include utilities.R
#' @include FLMatrix.R
NULL
chol <- function (x, ...){
  UseMethod("chol", x)
}
#' Cholesky Decomposition.
#'
#' \code{chol} computes the Cholesky factorization of FLMatrix object.\cr
#' The Cholesky decomposition is a decomposition of a positive definite matrix into the product of a lower triangular matrix
#' and its conjugate transpose.
#'
#' The wrapper overloads chol and implicitly calls FLCholeskyDecompUdt.
#' @param table an object of class FLMatrix
#' @section Constraints:
#' Input can only be a Hermitian, positive definite square matrix (n x n)
#' with maximum dimension limitations of (1000 x 1000)
#' @return \code{chol} returns the upper triangular factor of the Cholesky decomposition
#' @examples
#' connection<-odbcConnect("Gandalf")
#' table<-FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' chol(table)
#' @export
chol.FLMatrix<-function(object){
	connection<-object@odbc_connection
	sqlstr<-paste0("WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) AS (SELECT a.",object@matrix_id_colname,", a.",object@row_id_colname,", a.",object@col_id_colname,", a.",object@cell_val_colname," FROM  ",object@matrix_table," a WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") SELECT a.* FROM TABLE (FLCholeskyDecompUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) HASH BY z.Matrix_ID LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a ORDER BY 1,2,3;")
	retobj<-sqlQuery(connection,sqlstr)
	nrow<-max(retobj$OutputRowNum)
	ncol<-max(retobj$OutputColNum)
	cellval_chol<-(retobj$OutputVal)
	data_chol<-matrix(cellval_chol,nrow,ncol)
	print(data_chol)
}
