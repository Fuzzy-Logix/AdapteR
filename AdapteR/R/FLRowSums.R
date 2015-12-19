#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

rowSums <- function (x, ...){
  UseMethod("rowSums", x)
}


rowSums.default <- base::rowSums

#' row sums of a FLMatrix.
#'
#' \code{rowSums} computes the row-wise sums of FLMatrix objects.
#'
#' The wrapper overloads rowSums and extends it to FLMatrix objects.
#' @param object is of class FLMatrix.
#' @return \code{rowSums} returns a FLVector object representing the row-wise sums.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLVector <- rowSums(flmatrix)
#' @export

rowSums.FLMatrix<-function(object)
{
	connection<-getConnection(object)
	flag3Check(connection)

	sqlstr<-paste0("INSERT INTO ",
					getRemoteTableName(result_db_name,result_vector_table),
					" SELECT ",max_vector_id_value,
					         ",a.",object@row_id_colname,
					         ",SUM(a.",object@cell_val_colname,") 
					FROM ",remoteTable(object)," a ",
					constructWhere(constraintsSQL(object,"a")),
					" GROUP BY a.",object@row_id_colname)

	sqlSendUpdate(connection,sqlstr)
	
	max_vector_id_value <<- max_vector_id_value + 1
	
	table <- FLTable(connection,
		             result_db_name,
		             result_vector_table,
		             "VECTOR_INDEX",
		             whereconditions=paste0("VECTOR_ID = ",max_vector_id_value-1)
		             )

	return(table[,"VECTOR_VALUE"])
}
