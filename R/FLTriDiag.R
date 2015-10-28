#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

FLTriDiag <- function (x, ...){
	UseMethod("FLTriDiag", x)
}

#' TriDiagonal or Upper Hessenberg matrix of a FLMatrix.
#'
#' \code{FLTriDiag} computes the TriDiagonal or Upper Hessenberg matrix of FLMatrix object.
#'
#' The wrapper overloads FLTriDiag and implicitly calls FLTriDiagUdt.
#' @param object is of class FLMatrix
#' @section Constraints:
#' Input can only be a square matrix (n x n) with maximum dimension limitations
#' of (700 x 700).
#' @return \code{FLTriDiag} returns a FLMatrix object representing the upper Hessenberg or TriDiagonal matrix.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLMatrix <- FLTriDiag(flmatrix)
#' @export

FLTriDiag.FLMatrix<-function(object)
{

	if(object@nrow != object@ncol) 
	{ 
		stop("FLTriDiag function is applicable on square matrix only") 
	}

	connection<-object@odbc_connection
	flag1Check(connection)

	sqlstr<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
				   " WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
						AS (SELECT a.",object@matrix_id_colname,", 
								   a.",object@row_id_colname,", 
								   a.",object@col_id_colname,", 
								   a.",object@cell_val_colname,
							" FROM  ",object@matrix_table," a 
							WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") 
					SELECT ",max_matrix_id_value,
					       ",a.OutputRowNum,
					        a.OutputColNum,
					        a.OutputVal 
					FROM TABLE (FLTridiagUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
						 HASH BY z.Matrix_ID 
						 LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a;")
	
	sqlQuery(connection,sqlstr)
	
	max_matrix_id_value <<- max_matrix_id_value + 1

	return(new("FLMatrix", 
		       odbc_connection = connection, 
		       db_name = result_db_name, 
		       matrix_table = result_matrix_table, 
			   matrix_id_value = max_matrix_id_value-1,
			   matrix_id_colname = "MATRIX_ID", 
			   row_id_colname = "ROW_ID", 
			   col_id_colname = "COL_ID", 
			   cell_val_colname = "CELL_VAL",
			   nrow = object@nrow, 
			   ncol = object@ncol, 
			   dimnames = list(c(),c()))
	      )
}
