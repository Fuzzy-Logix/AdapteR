#' @include utilities.R
#' @include FLMatrix.R
NULL
det <- function (x, ...){
  UseMethod("det", x)
}
#' Determinant of a Matrix.
#'
#' \code{det} computes the determinant of FLMatrix objects.
#'
#' The wrapper overloads det and implicitly calls FLMatrixDetUdt.
#' @param table an object of class FLMatrix
#' @section Constraints:
#' Input can only be a square matrix (n x n) with maximum dimension limitations
#' of (1000 x 1000).
#' @return \code{det} returns determinant which replicates the equivalent R output.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' table <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' det(table)
#' @export
det.FLMatrix<-function(object){
	connection<-object@odbc_connection
	sqlstr<-paste0("WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) AS (SELECT a.",object@matrix_id_colname,", a.",object@row_id_colname,", a.",object@col_id_colname,", a.",object@cell_val_colname," FROM  ",object@matrix_table," a WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") SELECT a.* FROM TABLE (FLMatrixDetUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) HASH BY z.Matrix_ID LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a ORDER BY 1;")
	retobj<-sqlQuery(connection,sqlstr)
	result<-retobj$OutputDetVal
	result
}
