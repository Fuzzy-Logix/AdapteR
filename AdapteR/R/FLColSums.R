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
	connection<-getConnection(object)
	flag3Check(connection)

	sqlstr<-paste0("INSERT INTO ",
					getRemoteTableName(result_db_name,result_vector_table),
					" SELECT ",max_vector_id_value,
					         ",a.",object@variables$colId,
					         ",SUM(a.",object@variables$value,") 
					FROM ",remoteTable(object)," a ",
					constructWhere(constraintsSQL(object,"a")),
					" GROUP BY a.",object@variables$colId)

	sqlSendUpdate(connection,sqlstr)
	
	max_vector_id_value <<- max_vector_id_value + 1
	
	table <- FLTable(connection,
		             result_db_name,
		             result_vector_table,
		             "VECTOR_INDEX",
		             whereconditions=paste0(result_db_name,".",result_vector_table,".VECTOR_ID = ",max_vector_id_value-1)
		             )

	return(table[,"VECTOR_VALUE"])
}
