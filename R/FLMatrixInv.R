#' @include utilities.R
#' @include FLMatrix.R
NULL

#' Inverse of a matrix
#'
#' \code{FLMatrixInv} computes the inverse of a non-singular square matrix.
#' @section Constraints:
#' Input can only be a square matrix (n x n) with maximum dimension limitations
#' of (1000 x 1000).
#' @param matrix an object of class FLMatrix; \code{matrix} should be and
#' non-singular.
#' @return \code{FLMatrixInv} returns an object of class FLMatrix mapped
#' to the inverse of the  input matrix,which can be pulled to R using
#' \code{FLFetchMatrix}.
#' @examples
#' \dontrun{
#' connection      <- odbcConnect("Gandalf")
#' database        <- "FL_R_WRAP"
#' matrix_table    <- "tblMatrixMulti"
#' matrix_id_value <- 5
#' in_matrix       <- FLMatrix(connection, database, matrix_table, matrix_id_value,
#'                    matrix_id = "Matrix_ID", row_id = "Row_ID", column_id = "Col_ID",
#'                    cell_value = "Cell_Val")
#' out_matrix      <- FLMatrixInv(in_matrix)
#' }
#' @export
FLMatrixInv <- function(matrix)
{
	if(class(matrix) != "FLMatrix")
		stop("Argument Type Mismatch: matrix must be an FLMatrix object")		
	matrixTable   <- matrix@matrix_table
	matrixValue <- toString(matrix@matrix_id_value)
	outTable      <- gen_out_matrix_table("MatrixInv",matrixTable, matrixValue)
	connection    <- matrix@ODBC_connection		
	sqlParameters <- list(	matrixID      = matrix@matrix_id,
							matrixValue = toString(matrix@matrix_id_value),
							rowID         = matrix@row_id,
							columnID      = matrix@column_id,
							cellValue     = matrix@cell_value,
							matrixTable   = matrix@matrix_table,
							outTable      = outTable )
	run_sql(connection, "FLMatrixInv.sql", sqlParameters)

	retData = new(	"FLMatrix", ODBC_connection = connection, database = matrix@database,
					matrix_table = outTable, matrix_id_value = matrix@matrix_id_value,
					matrix_id = "OutputMatrixID", row_id = "OutputRowNum", column_id = "OutputColNum", cell_value = "OutputVal")
	return(retData)
}



