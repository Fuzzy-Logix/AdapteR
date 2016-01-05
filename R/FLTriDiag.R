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

	connection<-getConnection(object)
	flag1Check(connection)

	sqlstr<-paste0(" INSERT INTO ",
                   getRemoteTableName(result_db_name,result_matrix_table),
                   viewSelectMatrix(object,"a",withName="z"),
                   outputSelectMatrix("FLTridiagUdt",viewName="z",localName="a",includeMID=TRUE)
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
			   row_id_colname = "rowIdColumn", 
			   col_id_colname = "colIdColumn", 
			   cell_val_colname = "valueColumn",
			))
}
