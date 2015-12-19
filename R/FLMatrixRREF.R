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

	# checkSquare(object,"FLMatrixRREF")
	connection<-getConnection(object)
	flag1Check(connection)

	sqlstr<-paste0(" INSERT INTO ",
                   getRemoteTableName(result_db_name,result_matrix_table),
                   viewSelectMatrix(object,"a",withName="z"),
                   outputSelectMatrix("FLMatrixRREFUdt",viewName="z",localName="a",includeMID=TRUE)
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
