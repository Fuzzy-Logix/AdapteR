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

	sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_vector_table,
					" WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) AS 
					  (
					  SELECT a.",object@matrix_id_colname,",
					         a.",object@row_id_colname,",
					         a.",object@col_id_colname,",
					         a.",object@cell_val_colname,"
					  FROM   ",object@matrix_table," a
					  WHERE  a.",object@matrix_id_colname," = ",object@matrix_id_value,"
					  )
					  SELECT ",max_vector_id_value,",1,a.OutputMtxRank
					  FROM   TABLE (
					             FLMatrixRankUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val)
					             HASH BY z.Matrix_ID
					             LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID
					             ) AS a ")
	
	sqlQuery(connection,sqlstr0)
	
	max_vector_id_value <<- max_vector_id_value + 1
	
	table <- FLTable(connection,
		             result_db_name,
		             result_vector_table,
		             "VECTOR_ID",
		             "VECTOR_INDEX",
		             "VECTOR_VALUE"
		             )
	new("FLVector", 
		table = table, 
		col_name = table@num_val_name, 
		vector_id_value = max_vector_id_value-1, 
		size = 1)
	
}
