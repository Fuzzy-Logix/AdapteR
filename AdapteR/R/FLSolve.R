#' @include utilities.R
#' @include FLMatrix.R
#' @include FLIs.R
#' @include FLDims.R
#' @include FLPrint.R
NULL

solve <- function (x, ...){
	UseMethod("solve", x)
}

#' Inverse of a Matrix.
#'
#' \code{solve} computes the inverse for FLMatrix objects.
#'
#' The wrapper overloads solve and implicitly calls FLMatrixInvUdt.
#' @param object is of class FLMatrix
#' @section Constraints:
#' Input can only be a square matrix (n x n) with maximum dimension limitations
#' of (1000 x 1000).
#' @return \code{solve} returns a FLMatrix object which is the inverse of input FLMatrix object
#' and replicates the equivalent R output.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' resultFLMatrix <- solve(flmatrix)
#' @export

#solve.default <- base::solve
# do not define solve.default in this package as it is already defined in base::solve.default.
# It might lead to stack overflow.

solve.FLMatrix<-function(object)
{

	if(object@nrow != object@ncol) 
	{ 
		stop("solve function is applicable on square matrix only") 
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
					FROM TABLE (FLMatrixInvUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
						HASH BY z.Matrix_ID 
						LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a;")
	
	t<-sqlQuery(connection,sqlstr)

	if(length(t) > 0) 
	{ 
		stop(" Error Inverting Matrix - Matrix might be exactly singular ") 
	}
	
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
