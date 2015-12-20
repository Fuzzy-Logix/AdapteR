#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

solveExcl <- function (x, ...){
	UseMethod("solveExcl", x)
}

#' Inverse of a Matrix excluding a dimension.
#'
#' \code{solveExcl} computes the inverse for FLMatrix objects by excluding 
#' the specified row and column from the matrix.
#'
#' The wrapper overloads solveExcl and implicitly calls FLMatrixInvExclUdt.
#' @param object is of class FLMatrix
#' @param ExclIdx is a positive integer specifying row or column id to be excluded.
#' @section Constraints:
#' Input can only be a square matrix (n x n) with maximum dimension limitations
#' of (1000 x 1000).
#' @return \code{solveExcl} returns a FLMatrix object which is the inverse of input FLMatrix object
#' after excluding given dimension.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLMatrix <- solveExcl(flmatrix,3)
#' @export

solveExcl.FLMatrix<-function(object,ExclIdx)
{

	if(nrow(object) != ncol(object)) 
	{ 
		stop("solveExcl function is applicable on square matrix only") 
	}

	connection<-getConnection(object)
	flag1Check(connection)

	sqlstr<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
				   " WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val, ExclIdx) 
						AS (SELECT a.",object@matrix_id_colname,", 
								   a.",object@variables$rowId,", 
								   a.",object@variables$colId,", 
								   a.",object@variables$value,",",
								   ExclIdx, 
							" FROM  ",remoteTable(object)," a 
							WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") 
					SELECT ",max_matrix_id_value,
					       ",a.OutputRowNum,
					        a.OutputColNum,
					        a.OutputVal 
					FROM TABLE (FLMatrixInvExclUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val, z.ExclIdx) 
						HASH BY z.Matrix_ID 
						LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a;")
	
	t<-sqlQuery(connection,sqlstr)

	if(length(t) > 0) 
	{ 
		stop(" Error Inverting Matrix - Matrix might be exactly singular ") 
	}
	
	max_matrix_id_value <<- max_matrix_id_value + 1

	if(ExclIdx > nrow(object))
	{
		nr <- nrow(object)
	}
	else
	{
		nr <- nrow(object) - 1
	}
	return(FLMatrix( 
		       connection = connection, 
		       database = result_db_name, 
		       matrix_table = result_matrix_table, 
			   matrix_id_value = max_matrix_id_value-1,
			   matrix_id_colname = "MATRIX_ID", 
			   row_id_colname = "ROW_ID", 
			   col_id_colname = "COL_ID", 
			   cell_val_colname = "CELL_VAL",
			   nrow = nr, 
			   ncol = nr, 
			   dimnames = list(c(),c())))
}
