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
	connection<-getConnection(object)
	flag3Check(connection)

	sqlstr<-paste0("INSERT INTO ",
					getRemoteTableName(result_db_name,result_vector_table),
					" SELECT ",max_vector_id_value,
					         ",a.",object@variables$rowId,
					         ",AVG(a.",object@variables$value,")  
					FROM ",remoteTable(object)," a ",
					constructWhere(constraintsSQL(object,"a")),
					" GROUP BY a.",object@variables$rowId)

	sqlSendUpdate(connection,sqlstr)
	
	max_vector_id_value <<- max_vector_id_value + 1
	
	table <- FLTable(connection,
		             result_db_name,
		             result_vector_table,
		             "VECTOR_INDEX",
		             whereconditions=paste0("VECTOR_ID = ",max_vector_id_value-1)
		             )

	return(table[,"VECTOR_VALUE"])

	# table <- FLTable(connection,
	# 	             result_db_name,
	# 	             result_vector_table,
	# 	             "VECTOR_ID",
	# 	             "VECTOR_INDEX",
	# 	             "VECTOR_VALUE")

	# new("FLVector", 
	# 	table = table, 
	# 	col_name = table@variables$value, 
	# 	vector_id_value = max_vector_id_value-1, 
	# 	size = nrow(object))
}
