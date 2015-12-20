#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

FLMatrixREF <- function (x, ...){
	UseMethod("FLMatrixREF", x)
}

#' Row Echelon form of a Matrix.
#'
#' \code{FLMatrixREF} gives the Row Echelon form of FLMatrix objects.
#'
#' The wrapper overloads FLMatrixREF and implicitly calls FLMatrixREFUdt.
#' @param object is of class FLMatrix
#' @section Constraints:
#' Input can only be a square FLMatrix with maximum dimension limitations of (1000 x 1000).
#' @return \code{FLMatrixREF} returns a FLMatrix object which is the Row Echelon form of input FLMatrix.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLMatrix <- FLMatrixREF(flmatrix)
#' @export


FLMatrixREF.FLMatrix<-function(object)
{

	if(nrow(object) != ncol(object)) 
	{ 
		stop("FLMatrixREF function is applicable on square matrix only") 
	}

	connection<-getConnection(object)
	flag1Check(connection)

	sqlstr<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
				   " WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
						AS (SELECT a.",object@matrix_id_colname,", 
								   a.",object@variables$rowId,", 
								   a.",object@variables$colId,", 
								   a.",object@variables$value,
							" FROM  ",remoteTable(object)," a 
							WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") 
					SELECT ",max_matrix_id_value,
					       ",a.OutputRowNum,
					        a.OutputColNum,
					        a.OutputVal 
					FROM TABLE (FLMatrixREFUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
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
