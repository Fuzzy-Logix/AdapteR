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

	connection<-getConnection(object)
	flag1Check(connection)

	sqlstr<-paste0(" INSERT INTO ",
                   getRemoteTableName(result_db_name,result_matrix_table),
                   viewSelectMatrix(object,"a",withName="z"),
                   outputSelectMatrix("FLMatrixREFUdt",viewName="z",localName="a",includeMID=TRUE)
                   )
	
	sqlSendUpdate(connection,sqlstr)
	
	MID <- max_matrix_id_value
	max_matrix_id_value <<- max_matrix_id_value + 1

	return(FLMatrix( 
		       connection = connection, 
		       database = result_db_name, 
		       matrix_table = result_matrix_table, 
			   matrix_id_value = MID,
			   matrix_id_colname = "MATRIX_ID", 
			   row_id_colname = "ROW_ID", 
			   col_id_colname = "COL_ID", 
			   cell_val_colname = "CELL_VAL",
			))
}
