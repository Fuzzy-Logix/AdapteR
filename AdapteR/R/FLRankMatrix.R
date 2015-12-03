#' @include utilities.R
#' @include FLMatrix.R
#' @include FLVector.R
NULL

rankMatrix<-function(x, ...){
	UseMethod("rankMatrix", x)
}

rankMatrix.default <- Matrix::rankMatrix

#' Matrix Rank.
#'
#' \code{rankMatrix} computes the rank of FLMatrix objects.
#'
#' \code{rankMatrix} computes the rank of input FLMatrix object, stores the result
#' in-database and returns FLVector object
#' @param object is of class FLMatrix
#' @return \code{rankMatrix} returns FLVector object of size 1 which replicates the equivalent R output.
#' @section Constraints:
#' Input can have maximum dimension limitations of (1000 x 1000).
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' resultFLVector <- rankMatrix(flmatrix)
#' @export

rankMatrix.FLMatrix<-function(object)
{
	connection<-object@odbc_connection


	flag3Check(connection)

	sqlstr<-paste0(viewSelectMatrix(object,"a"),
                       outputSelectMatrix("FLMatrixRankUdt",includeMID=FALSE,outColNames=list("OutputMtxRank"))
                       )
	
	return(sqlQuery(connection,sqlstr)$"OutputMtxRank"[1])
	##browser()
### Phani-- below lines are absolete
### gk: delete the cruft. ;-)
	# table <- FLTable(connection,
	# 	             result_db_name,
	# 	             result_vector_table,
	# 	             "VECTOR_ID",
	# 	             "VECTOR_INDEX",
	# 	             "VECTOR_VALUE"
	# 	             )
	# new("FLVector", 
	# 	table = table, 
	# 	col_name = table@num_val_name, 
	# 	vector_id_value = max_vector_id_value-1, 
	# 	size = 1)	
}
