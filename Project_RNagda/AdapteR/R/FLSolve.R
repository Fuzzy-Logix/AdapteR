#' @include FLMatrix.R
NULL
#' Inverse of a Matrix.
#'
#' \code{solve} computes the inverse for FLMatrix objects.
#'
#' The wrapper overloads solve and implicitly calls FLMatrixInvUdt.
solve <- function (x, ...){
	UseMethod("solve", x)
}
#' @section Constraints:
#' Input can only be a square matrix (n x n) with maximum dimension limitations
#' of (1000 x 1000).
#' @param Input an object of class FLMatrix
#' @return \code{solve} returns inverse which replicates the equivalent R output.
#' @examples
#' \dontrun{
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' table <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' solve(table)
#' }
#' @export
solve.FLMatrix<-function(object){
	connection<-object@odbc_connection
	sqlstr<-paste0("WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) AS (SELECT a.",object@matrix_id_colname,", a.",object@row_id_colname,", a.",object@col_id_colname,", a.",object@cell_val_colname," FROM  ",object@matrix_table," a WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") SELECT a.* FROM TABLE (FLMatrixInvUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) HASH BY z.Matrix_ID LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a ORDER BY 1,2,3;")
	retobj<-sqlQuery(connection,sqlstr)
	data<-c()
	i<-1
	while(i<=length(retobj$OutputVal))
	{
		data<-append(data,retobj$OutputVal[i])
		i<-i+1
	}
	options(scipen=999)
	return <- round(data, digits=8)
	inverse<-matrix(return, nrow=sqrt(length(retobj$OutputVal)), ncol=sqrt(length(retobj$OutputVal)), byrow=TRUE)
	inverse
}
