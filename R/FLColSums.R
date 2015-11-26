#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

colSums <- function (x, ...){
  UseMethod("colSums", x)
}

colSums.default <- base::colSums

#' column sums of a FLMatrix.
#'
#' \code{colSums} computes the column-wise sums of FLMatrix objects.
#'
#' The wrapper overloads colSums and extends it to FLMatrix objects.
#' @param object is of class FLMatrix.
#' @return \code{colSums} returns a FLVector object representing the col-wise sums.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLVector <- colSums(flmatrix)
#' @export

colSums.FLMatrix<-function(object)
{
	connection<-object@odbc_connection
	flag3Check(connection)

	sqlstr<-paste0("INSERT INTO ",result_db_name,".",result_vector_table, 
					" SELECT ",max_vector_id_value,
					         ",a.",object@col_id_colname,
					         ", CAST(SUM(a.",object@cell_val_colname,") AS NUMBER) 
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
		col_name = table@num_val_name, 
		vector_id_value = max_vector_id_value-1, 
		size = ncol(object))
}
