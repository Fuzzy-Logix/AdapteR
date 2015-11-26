#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

ginv <- function (x, ...){
	UseMethod("ginv", x)
}

ginv.default <- MASS::ginv

#' Generalized Inverse of a Matrix.
#'
#' \code{ginv} computes the pseudo-inverse for FLMatrix objects.
#'
#' The wrapper overloads ginv and implicitly calls FLMatrixPseudoInvUdt.
#' @param object is of class FLMatrix
#' @section Constraints:
#' Input can only be with maximum dimension limitations of (500 x 500).
#' @return \code{ginv} returns a FLMatrix object which is the pseudo-inverse 
#' of input FLMatrix object and replicates the equivalent R output.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 1)
#' resultFLMatrix <- ginv(flmatrix)
#' @export

ginv.FLMatrix<-function(object)
{

	connection<-object@odbc_connection
	flag1Check(connection)

	sqlstr<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
				   " WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
						AS (SELECT a.",object@matrix_id_colname,", 
								   a.",object@row_id_colname,", 
								   a.",object@col_id_colname,", 
								   a.",object@cell_val_colname,
							" FROM  ",remoteTable(object)," a 
							WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") 
					SELECT ",max_matrix_id_value,",
					       a.OutputRowNum,
					       a.OutputColNum,
					       a.OutputVal 
					FROM TABLE (FLMatrixPseudoInvUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
						HASH BY z.Matrix_ID 
						LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a;")
	
	sqlSendUpdate(connection,sqlstr)

	max_matrix_id_value <<- max_matrix_id_value + 1

	return(FLMatrix( 
		       connection = connection, 
		       database = result_db_name, 
		       matrix_table = result_matrix_table, 
			   matrix_id_value = max_matrix_id_value-1,
			   matrix_id_colname = "MATRIX_ID", 
			   row_id_colname = "ROW_ID", 
			   col_id_colname = "COL_ID", 
			   cell_val_colname = "CELL_VAL",
			   nrow = ncol(object), 
			   ncol = nrow(object), 
			   dimnames = list(c(),c())))
}
