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

	MID <- max_matrix_id_value

	sqlstr<-paste0(" INSERT INTO ",
                   getRemoteTableName(result_db_name,result_matrix_table),
                   viewSelectMatrix(object,"a",viewName="z"),
                   outputSelectMatrix("FLMatrixPseudoInvUdt",viewName="z",localName="a")
                   )
	
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
