#' @include utilities.R
#' @include FLMatrix.R
NULL

#' Singular Value Decomposition of a matrix
#'
#' Formally, the singular value decomposition of an m x n matrix M is a
#' factorization of the form \deqn{M = U \sum V'}
#' where U is an m x m unitary matrix, \eqn{\sum} is an m x n rectangular diagonal
#' matrix with non-negative real numbers on the diagonal, and \eqn{V'} (the
#' conjugate transpose of V) is an n x n real or complex unitary matrix.
#' The diagonal entries \eqn{\sum _{i,i}} of \eqn{\sum} are known as the singular
#' values of M. The m columns of U and the n columns of V are called the
#' left-singular vectors and right-singular vectors of M, respectively.
#'
#' @section Constraints:
#' Input matrix has a maximum dimension limitations of (550 x 550).

#' @param matrix an object of class FLMatrix.

#' @return \code{FLLUDecomp} returns an object of class FLSVD. An object
#' of class FLSVD is a list containing three components of class FLMatrix
#' which are described as follows.
#' \item{u_matrix}{the U matrix}
#' \item{s_matrix}{the \eqn{\sum} matrix}
#' \item{v_matrix}{the V matrix}
#' All the three matrices of class FLSVD can be pulled in R using
#' \code{FLFetchMatrix}.

#' @examples
#' \dontrun{
#' connection      <- odbcConnect("Gandalf")
#' database        <- "FL_R_WRAP"
#' matrix_table    <- "tblMatrixMulti"
#' matrix_id_value <- 3
#' in_matrix       <- FLMatrix(connection, database, matrix_table, matrix_id_value,
#'                    matrix_id = "Matrix_ID", row_id = "Row_ID", column_id = "Col_ID",
#'                    cell_value = "Cell_Val")
#' out_svd         <- FLSVD(in_matrix)
#' }
#' @export
FLSVD <- function(matrix)
{
	matrixTable   <- matrix@matrix_table
	matrixValue	  <- matrix@matrix_id_value
	outTable      <- gen_out_matrix_table("SVD",matrixTable, toString(matrixIDValue))
	connection    <- matrix@ODBC_connection
	sqlParameters <- list(	matrixID      = matrix@matrix_id,
							matrixValue = toString(matrixIDValue),
							rowID         = matrix@row_id,
							columnID      = matrix@column_id,
							cellValue     = matrix@cell_value,
							matrixTable   = matrix@matrix_table,
							outTable      = outTable )
	run_sql(connection, "FLSVDUdt.sql", sqlParameters)

	uMatrix <- FLMatrix(connection,
						database        = matrix@database,
						matrix_table    = outTable,
						matrix_id_value = matrixValue,
						matrix_id       = "OutputMatrixID",
						row_id          = "OutputRowNum",
						column_id       = "OutputColNum",
						cell_value      = "OutUVal")

	sMatrix <- FLMatrix(connection,
						database        = matrix@database,
						matrix_table    = outTable,
						matrix_id_value = matrixValue,
						matrix_id       = "OutputMatrixID",
						row_id          = "OutputRowNum",
						column_id       = "OutputColNum",
						cell_value      = "OutSVal")

	vMatrix <- FLMatrix(connection,
						database        = matrix@database,
						matrix_table    = outTable,
						matrix_id_value = matrixValue,
						matrix_id       = "OutputMatrixID",
						row_id          = "OutputRowNum",
						column_id       = "OutputColNum",
						cell_value      = "OutVVal")

	retData = new("FLSVD", u_matrix = uMatrix, s_matrix = sMatrix, v_matrix = vMatrix)
	return(retData)
}
