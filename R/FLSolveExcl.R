#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

FLSolveExcl <- function (x, ...){
	UseMethod("FLSolveExcl", x)
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

FLSolveExcl.FLMatrix<-function(object,ExclIdx)
{

	connection<-getConnection(object)
	flag1Check(connection)

	  MID <- max_matrix_id_value

	

	sqlstr<-paste0(" INSERT INTO ",
					getRemoteTableName(result_db_name, result_matrix_table),
				   " WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val, ExclIdx) 
						AS (SELECT a.",getVariables(object)$matrixId,", 
								   a.",getVariables(object)$rowId,", 
								   a.",getVariables(object)$colId,", 
								   a.",getVariables(object)$value,",",
								   ExclIdx, 
							" FROM  ",remoteTable(object)," a ",
							constructWhere(c(constraintsSQL(object))),") 
					SELECT ",MID,
					       ",a.OutputRowNum,
					        a.OutputColNum,
					        a.OutputVal 
					FROM TABLE (FLMatrixInvExclUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val, z.ExclIdx) 
						HASH BY z.Matrix_ID 
						LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a;")
	
	sqlSendUpdate(connection,sqlstr)
	max_matrix_id_value <<- max_matrix_id_value + 1

	return(FLMatrix(
            connection = connection, 
            database = result_db_name, 
            matrix_table = result_matrix_table, 
            matrix_id_value = MID,
            matrix_id_colname = "MATRIX_ID", 
            row_id_colname = "ROW_ID", 
            col_id_colname = "COL_ID", 
            cell_val_colname = "CELL_VAL")
           )
}
