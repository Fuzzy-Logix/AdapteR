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

tr.default <- psych::tr

tr.FLMatrix<-function(object){
	connection<-getConnection(object)
	
	flag3Check(connection)

	sqlstr<-paste0( " INSERT INTO ",
					getRemoteTableName(result_db_name,result_vector_table),
					" SELECT ",max_vector_id_value,
					         ",1, 
					         FLMatrixTrace(",getVariables(object)$rowId,
					         			   ",",getVariables(object)$colId,
					              		   ",",getVariables(object)$value,")",
				    " FROM ",remoteTable(object)," a",
				    " GROUP BY ",getVariables(object)$matrixId,
				    constructWhere(c(constraintsSQL(object),
				    	paste0(getVariables(object)$rowId," <= ",min(nrow(object),ncol(object))),
				    	paste0(getVariables(object)$colId, " <= ", min(nrow(object),ncol(object))))))
	
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
