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
	connection<-getConnection(object)
	flag3Check(connection)

	sqlstr<-paste0("INSERT INTO ",
					getRemoteTableName(result_db_name,result_vector_table),
					" SELECT ",max_vector_id_value,
					         ",a.",getVariables(object)$colIdColumn,
					         ", AVG(a.",getVariables(object)$valueColumn,") 
					FROM ",remoteTable(object)," a ",
                       constructWhere(constraintsSQL(object,"a")),
					" GROUP BY a.",getVariables(object)$colIdColumn)

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
