#' @include utilities.R
#' @include FLMatrix.R
NULL
t<-function(x, ...){
	UseMethod("t", x)
}
#' Matrix Transpose.
#'
#' \code{t} returns the transpose of FLMatrix objects.
#'
#' The wrapper overloads t such that given a matrix or data frame of class FLMatrix, t returns the transpose of that object
#' @param table an object of class FLMatrix
#' @section Constraints:
#' Input can be a matrix of dimensions (m x n) where m > n, m < n or m = n.
#' @return \code{t} returns transpose which replicates the equivalent R output.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' table <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' t(table)
#' @export
t.FLMatrix<-function(object)
{
	sqlQuery(object@odbc_connection,paste0("DATABASE ",object@db_name,";"," SET ROLE ALL;"))
	
	matrix_id_val <- sqlQuery(object@odbc_connection, paste0(" SELECT max(",object@matrix_id_colname,") FROM ",object@matrix_table))[1,1]+1
	
	sqlQuery(object@odbc_connection, paste0(" INSERT INTO ",object@matrix_table," SELECT ",matrix_id_val,",a.",object@col_id_colname," AS ",
			object@row_id_colname,",a.",object@row_id_colname," AS ",object@col_id_colname,",a.",object@cell_val_colname," AS ",object@cell_val_colname,
			" FROM ",object@matrix_table," a where a.",object@matrix_id_colname,"=",object@matrix_id_value))

	new("FLMatrix", odbc_connection = object@odbc_connection, db_name = object@db_name, matrix_table = object@matrix_table, 
		matrix_id_value = matrix_id_val, matrix_id_colname = object@matrix_id_colname, row_id_colname = object@row_id_colname, 
		col_id_colname = object@col_id_colname, cell_val_colname = object@cell_val_colname)
	
}
