#' Eigendecomposition of a matrix
#'
#' Calculates the eigenvalues and eigenvectors of a square matrix (n x n).
#' @section Constraints:
#' Input can only be a square matrix (n x n) with maximum dimension limitations
#' of (700 x 700). The current FLEigen can only calculate real eigenvalues. If
#' the input matrix is not symmetric, it is suggested to verify the output
#' values.
#' @param matrix an object of class FLMatrix.
#' @return \code{FLEigen} returns a list. The list has two components of class
#' \code{FLMatrix}: \code{vectors} and \code{values}. \code{"vectors"} is mapped
#' to a matrix containing the eigen vectors of the input matrix. \code{"values"}
#' is mapped to a diagonal matrix whose diagonal elements are the eigen values
#' of the input matrix.
#' @examples
#' \dontrun{
#' connection      <- odbcConnect("Gandalf")
#' database        <- "FL_R_WRAP"
#' matrix_table    <- "tblMatrixMulti"
#' matrix_id_value <- 5
#' in_matrix       <- FLMatrix(connection, database, matrix_table, matrix_id_value,
#'                    matrix_id = "Matrix_ID", row_id = "Row_ID", column_id = "Col_ID",
#'                    cell_value = "Cell_Val")
#' out_eigen       <- FLEigen(in_matrix)
#' }
#' @export
FLEigen <- function(matrix)  
{	
	matrixTable   <- matrix@matrix_table
	matrixValue <- toString(matrix@matrix_id_value)
	connection    <- matrix@ODBC_connection
	outTable      <- gen_out_matrix_table("FLEigenVectors",matrixTable, matrixValue)
	sqlParameters <- list(	matrixID      = matrix@matrix_id,
							matrixValue   = matrixValue,
							rowID         = matrix@row_id,
							columnID      = matrix@column_id,
							cellValue     = matrix@cell_value,
							matrixTable   = matrixTable,
							outTable      = outTable )
	run_sql(connection, "FLEigenVectorUdt.sql", sqlParameters)
	retData <- list()
	retData$vectors = new(	"FLMatrix", 
							ODBC_connection = connection,
							database        = matrix@database, 
							matrix_table    = outTable, 
							matrix_id_value = matrix@matrix_id_value, 
							matrix_id       = "OutputMatrixID", 
							row_id          = "OutputRowNum", 
							column_id       = "OutputColNum", 
							cell_value      = "OutputVal")

	outTable 		<- gen_out_matrix_table("FLEigenValues",matrixTable, matrixValue)		
	sqlParameters$outTable <- outTable
	run_sql(connection, "FLEigenValueUdt.sql", sqlParameters)
	retData$values = new(	"FLMatrix", 
							ODBC_connection = connection,
							database        = matrix@database, 
							matrix_table    = outTable, 
							matrix_id_value = matrix@matrix_id_value, 
							matrix_id       = "OutputMatrixID", 
							row_id          = "OutputRowNum", 
							column_id       = "OutputColNum", 
							cell_value      = "OutputVal")
		
	return(retData)
}	