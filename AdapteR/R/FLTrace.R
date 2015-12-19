#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

#' Matrix Trace.
#'
#' \code{tr} computes the trace of FLMatrix objects.
#'
#' \code{tr} computes the trace of input FLMatrix object, stores the result
#' in-database and returns FLVector object
#' @param x an object of class FLMatrix
#' @return \code{tr} returns FLVector object of size 1 which replicates the equivalent R output.
#' @section Constraints:
#' Input can only be with maximum dimension limitations
#' of (1000 x 1000).
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' resultFLVector <- tr(flmatrix)
#' @export

tr<-function(x, ...){
	UseMethod("tr", x)
}

tr.FLMatrix<-function(object){
	connection<-getConnection(object)
	
	flag3Check(connection)

	sqlstr<-paste0(     " INSERT INTO ",result_db_name,".",result_vector_table,
						" SELECT ",max_vector_id_value,
						         ",1, 
						         CAST(FLMatrixTrace(a.",object@row_id_colname,", a.",object@col_id_colname,
						              ", a.",object@cell_val_colname,") AS NUMBER) ",
					    " FROM ",remoteTable(object)," a",
					    " GROUP BY a.",object@matrix_id_colname,
					    " WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,
					    " AND a.", object@row_id_colname," <= ",min(nrow(object),ncol(object)),
					    " AND a.",object@col_id_colname, " <= ", min(nrow(object),ncol(object)))
	
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
		size = 1)
}
