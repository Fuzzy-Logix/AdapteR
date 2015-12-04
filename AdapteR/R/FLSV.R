#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

FLSV <- function (x, ...){
  UseMethod("FLSV", x)
}

#' Singular Values of a FLMatrix.
#'
#' \code{FLSV} computes the singular values for FLMatrix objects.
#'
#' The wrapper overloads FLSV and implicitly calls FLSVUdt.
#' @param object is of class FLMatrix
#' @section Constraints:
#' Input can only be a square matrix (n x n) with maximum dimension limitations
#' of (700 x 700).
#' @return \code{FLSV} returns a FLVector object representing the singular values.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLVector <- FLSV(flmatrix)
#' @export

FLSV.FLMatrix<-function(object)
{
	if(nrow(object) != ncol(object))
	{
		stop("not a square matrix")
	}
	connection<-object@odbc_connection
	flag3Check(connection)

	sqlstr<-paste0("INSERT INTO ",result_db_name,".",result_vector_table,
				   " WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
					AS (SELECT a.",object@matrix_id_colname,", 
							   a.",object@row_id_colname,", 
							   a.",object@col_id_colname,",
							   a.",object@cell_val_colname," 
						FROM  ",remoteTable(object)," a 
						WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") 
					SELECT ",max_vector_id_value,
					       ",a.OutputID,
					       CAST(a.OutputSV AS NUMBER) 
					FROM TABLE (FLSVUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
								HASH BY z.Matrix_ID 
								LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) 
					AS a;")

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
		size = nrow(object))
}
