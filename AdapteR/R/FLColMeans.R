#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

colMeans <- function (x, ...){
  UseMethod("colMeans", x)
}


colMeans.default <- base::colMeans

#' column means of a FLMatrix.
#'
#' \code{colMeans} computes the column-wise average of FLMatrix objects.
#'
#' The wrapper overloads colMeans and extends it to FLMatrix objects.
#' @param object is of class FLMatrix.
#' @return \code{colMeans} returns a FLVector object representing the column-wise Means.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLVector <- colMeans(flmatrix)
#' @export

colMeans.FLMatrix<-function(object)
{
	connection<-object@odbc_connection
	flag3Check(connection)

	sqlstr<-paste0("INSERT INTO ",result_db_name,".",result_vector_table, 
					" SELECT ",max_vector_id_value,
					         ",a.",object@col_id_colname,
					         ", CAST(AVG(a.",object@cell_val_colname,") AS NUMBER) 
					FROM ",remoteTable(object)," a 
					WHERE a.",object@matrix_id_colname,"=",object@matrix_id_value,
					" GROUP BY a.",object@col_id_colname)

	sqlSendUpdate(connection,sqlstr)
	
	max_vector_id_value <<- max_vector_id_value + 1
	
	table <- FLTable(connection,
		             result_db_name,
		             result_vector_table,
		             "VECTOR_ID",
		             "VECTOR_INDEX",
		             "VECTOR_VALUE")

	new("FLVector", 
		table = table, 
		col_name = table@cell_val_colname, 
		vector_id_value = max_vector_id_value-1, 
		size = ncol(object))
}
