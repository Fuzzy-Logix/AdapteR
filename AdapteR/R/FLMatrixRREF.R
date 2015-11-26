#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

FLMatrixRREF <- function (x, ...){
	UseMethod("FLMatrixRREF", x)
}

#' Reduced Row Echelon form of a Matrix.
#'
#' \code{FLMatrixRREF} gives the Reduced Row Echelon form of FLMatrix objects.
#'
#' The wrapper overloads FLMatrixRREF and implicitly calls FLMatrixRREFUdt.
#' @param object is of class FLMatrix
#' @section Constraints:
#' Input can only be a square FLMatrix with maximum dimension limitations of (1000 x 1000).
#' @return \code{FLMatrixRREF} returns a FLMatrix object which is the Reduced Row Echelon form of input FLMatrix.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLMatrix <- FLMatrixRREF(flmatrix)
#' @export

FLMatrixRREF.FLMatrix<-function(object)
{

	if(nrow(object) != ncol(object)) 
	{ 
		stop("FLMatrixRREF function is applicable on square matrix only") 
	}

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
					SELECT ",max_matrix_id_value,
					       ",a.OutputRowNum,
					        a.OutputColNum,
					        a.OutputVal 
					FROM TABLE (FLMatrixRREFUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
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
			   nrow = nrow(object), 
			   ncol = ncol(object), 
			   dimnames = list(c(),c()))
	      )
}
