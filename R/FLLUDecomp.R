#' @include utilities.R
#' @include FLMatrix.R
NULL

#' Lower-upper (lu) decomposition of a matrix
#'
#' The LU decomposition involves factorizing a matrix as the product of a lower
#' triangular matrix L and an upper triangular matrix U. Permutation matrix is
#' also provided in the output. If permutation matrix is not used in the decomposition, the output of
#' permutation matrix is an identity matrix.
#'
#' @section Constraints:
#' Input matrix has a maximum dimension limitations of (1000 x 1000).
#' @param matrix an object of class FLMatrix; \code{matrix} should be square.
#' @return \code{FLLUDecomp} returns an object of class FLLUDecomp. An object
#' of class FLLUDecomp is a list containing three components of class FLMatrix
#' which are described as follows.
#' \item{l_matrix}{lower triangular matrix}
#' \item{u_matrix}{upper triangular matrix}
#' \item{perm_matrix}{permutation matrix}
#' All the three matrices of class FLLUDecomp can be pulled in R using
#' \code{FLFetchMatrix}.
#' @examples
#' \dontrun{
#' library(RODBC)
#' connection      <- odbcConnect("Gandalf");
#' database        <- "FL_R_WRAP";
#' matrix_table    <- "tblMatrixMulti";
#' matrix_id_value <- 5;
#' in_matrix       <- FLMatrix(connection, database, matrix_table, matrix_id_value,
#'                    matrix_id = "Matrix_ID", row_id = "Row_ID", column_id = "Col_ID",
#'                    cell_value = "Cell_Val")
#' out_lu          <- FLLUDecomp(in_matrix)
#' }
#' @export
FLLUDecomp <- function(matrix)  
{
		matrixValue <- matrix@matrix_id_value
		matrixTable   <- matrix@matrix_table
		connection    <- matrix@ODBC_connection
		outTable      <- gen_out_matrix_table("LUDecomp",matrixTable, matrixValue)
		
		sqlParameters <- list(	matrixID      = matrix@matrix_id,
								matrixValue = toString(matrixValue),
								rowID         = matrix@row_id,
								columnID      = matrix@column_id,
								cellValue     = matrix@cell_value,
								matrixTable   = matrixTable,
								outTable      = outTable )
		run_sql(connection, "FLLUDecomp.sql", sqlParameters)
		
		lMatrix    	<- FLMatrix(connection,
								database        = matrix@database, 
								matrix_table    = outTable,
								matrix_id_value = matrixValue, 
								matrix_id       = "OutputMatrixID",
								row_id          = "OutputRowNum", 
								column_id       = "OutputColNum", 
								cell_value      = "OutputValL")

		uMatrix    	<- FLMatrix(connection, 
								database        = matrix@database, 
								matrix_table    = outTable, 
								matrix_id_value = matrixValue, 
								matrix_id       = "OutputMatrixID", 
								row_id          = "OutputRowNum", 
								column_id       = "OutputColNum", 
								cell_value      = "OutputValU")
		
		permMatrix 	<- FLMatrix(connection, 
								database        = matrix@database, 
								matrix_table    = outTable, 
								matrix_id_value = matrixValue, 
								matrix_id       = "OutputMatrixID", 
								row_id          = "OutputRowNum", 
								column_id       = "OutputColNum", 
								cell_value      = "OutputPermut")

		retData = new("FLLUDecomp", l_matrix = lMatrix, u_matrix = uMatrix, perm_matrix = permMatrix)
		return(retData)
}