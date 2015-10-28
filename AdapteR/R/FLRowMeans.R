#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

rowMeans <- function (x, ...){
  UseMethod("rowMeans", x)
}

rowMeans.default <- base::rowMeans


#' row means of a FLMatrix.
#'
#' \code{rowMeans} computes the row-wise average of FLMatrix objects.
#'
#' The wrapper overloads rowMeans and extends it to FLMatrix objects.
#' @param object is of class FLMatrix.
#' @return \code{rowMeans} returns a FLVector object representing the row-wise Means.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLVector <- rowMeans(flmatrix)
#' @export

rowMeans.FLMatrix<-function(object)
{
	connection<-object@odbc_connection
	flag3Check(connection)

	sqlstr<-paste0("INSERT INTO ",result_db_name,".",result_vector_table, 
					" SELECT ",max_vector_id_value,
					         ",a.",object@row_id_colname,
					         ", CAST(AVG(a.",object@cell_val_colname,") AS NUMBER) 
					FROM ",object@db_name,".",object@matrix_table," a 
					WHERE a.",object@matrix_id_colname,"=",object@matrix_id_value,
					" GROUP BY a.",object@row_id_colname)

	sqlQuery(connection,sqlstr)
	
	max_vector_id_value <<- max_vector_id_value + 1
	
	table <- FLTable(connection,
		             result_db_name,
		             result_vector_table,
		             "VECTOR_ID",
		             "VECTOR_INDEX",
		             "VECTOR_VALUE")

	new("FLVector", 
		table = table, 
		col_name = table@num_val_name, 
		vector_id_value = max_vector_id_value-1, 
		size = object@nrow)
}
