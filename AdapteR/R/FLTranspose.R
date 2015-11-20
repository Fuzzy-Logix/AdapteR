#' @include utilities.R
#' @include FLMatrix.R
#' @include FLIs.R
#' @include FLDims.R
#' @include FLPrint.R
NULL

t<-function(x, ...){
	UseMethod("t", x)
}
#' Matrix Transpose.
#'
#' \code{t} returns the transpose of FLMatrix objects.
#'
#' The wrapper overloads t such that given a matrix of class FLMatrix, t returns the transpose of that object
#' @param object is of class FLMatrix
#' @section Constraints:
#' Input can be a matrix of dimensions (m x n) where m > n, m < n or m = n.
#' @return \code{t} returns a FLMatrix object which is the transpose of input FLMatrix object
#' and replicates the equivalent R output.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' resultFLMatrix <- t(flmatrix)
#' @export
t.FLMatrix<-function(object)
{
	
	flag1Check(object@odbc_connection)
	
	sqlSendUpdate(object@odbc_connection, paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
		                                    " SELECT ",max_matrix_id_value,
		                                             ",a.",
			                                          object@col_id_colname," AS ",object@row_id_colname,
			                                          ",a.",object@row_id_colname," AS ",object@col_id_colname,
			                                          ",a.",object@cell_val_colname," AS ",object@cell_val_colname,
			                                " FROM ",remoteTable(object)," a 
			                                where a.",object@matrix_id_colname,"=",object@matrix_id_value))

	max_matrix_id_value <<- max_matrix_id_value + 1

	new("FLMatrix", 
		odbc_connection = object@odbc_connection, 
		db_name = result_db_name, 
		matrix_table = result_matrix_table, 
		matrix_id_value = max_matrix_id_value - 1, 
		matrix_id_colname = "MATRIX_ID", 
		row_id_colname = "ROW_ID", 
		col_id_colname = "COL_ID", 
		cell_val_colname = "CELL_VAL", 
		nrow = object@ncol, 
		ncol = object@nrow, 
		dimnames = list(object@dimnames[[2]],object@dimnames[[1]]))
	
}
