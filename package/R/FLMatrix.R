#
# Fuzzy Logix Matrix Object
# @param  {RODBC}	Connection		  [description]
# @param  {character} database  [description]
# @param  {character} matrix_table	  [description]
# @param  {numeric} matrix_id_value		  [description]

setOldClass("RODBC")
#' @export
setClass("FLMatrix",
		slots = list(	ODBC_connection = "RODBC",
						database        = "character",
						matrix_table    = "character",
						matrix_id_value = "numeric",
						matrix_id       = "character",
						row_id          = "character",
						column_id       = "character",
						cell_value      = "character"))
#' @export
setClass("FLLUDecomp",
		slots = list(	l_matrix = "FLMatrix",
						u_matrix = "FLMatrix",
						perm_matrix = "FLMatrix"))
#' @export
setClass("FLSVD",
		slots = list(	u_matrix = "FLMatrix",
						s_matrix = "FLMatrix",
						v_matrix = "FLMatrix"))

#' Constructor function for matrix
#'
#' \code{FLMatrix} constructs an object of class \code{FLMatrix}. This object
#'      is used as the input for all the matrix operation functions.
#'
#' @param connection ODBC connection handle as returned by \code{\link[RODBC]{odbcConnect}}
#' @param database name of the database in \code{Teradata} which has the matrix
#' table
#' @param matrix_table name of the matrix table
#' @param matrix_id_value identifier for the input matrix
#' @param matrix_id,row_id,column_id,cell_value  column names in \code{matrix_table}
#' which give the matrix_id_value, row number, column number and value of the
#' matrix element
#' @return \code{FLMatrix} returns an object of class FLMatrix mapped
#' to a matrix, which can be pulled to R using \code{FLFetchMatrix}.
#' @examples
#' \dontrun{
#' connection      <- odbcConnect("Gandalf")
#' database        <- "FL_R_WRAP"
#' matrix_table    <- "tblMatrixMulti"
#' matrix_id_value <- 5
#' in_matrix       <- FLMatrix(connection, database, matrix_table, matrix_id_value,
#'                    matrix_id = "Matrix_ID", row_id = "Row_ID", column_id = "Col_ID",
#'                    cell_value = "Cell_Val")
#' }
#' @export
FLMatrix <- function(connection, database, matrix_table, matrix_id_value,
					 matrix_id = "Matrix_ID", row_id = "Row_ID", column_id = "Col_ID",
					 cell_value = "Cell_Val")
{

	#Type validation
	matrix_id_value <- ifelse(	is_number(matrix_id_value),
						as.integer(matrix_id_value),
						stop("matrix_id_value should be an integer"))

	argList  <- as.list(environment())
	typeList <- list(	connection      = "integer",
						database        = "character",
						matrix_table    = "character",
						matrix_id_value = "integer",
						matrix_id       = "character",
						row_id          = "character",
						column_id       = "character",
						cell_value      = "character" )
	validate_args(argList, typeList)

	sqlQuery(connection, paste("DATABASE", database));
	sqlQuery(connection, "SET ROLE ALL");

	new("FLMatrix", ODBC_connection = connection, database = database, matrix_table = matrix_table, matrix_id_value = matrix_id_value, matrix_id = matrix_id, row_id = row_id, column_id = column_id, cell_value = cell_value)
}

# FLFetchMatrix
#' Pulls matrix into R
#'
#' \code{FLFetchMatrix} retrieves a matrix to which an FLMatrix object is
#' mapped to in Teradata.
#' @param matrix an object of class FLMatrix.
#' @return \code{FLFetchMatrix} returns a data frame with three components which
#' are as follows.
#' \item{ROW_ID}{Row number of the matrix elements}
#' \item{COL_ID}{Column number of the matrix elements}
#' \item{CELL_VAL}{Value of the matrix elements}
#' @examples
#' \dontrun{
#' connection      <- odbcConnect("Gandalf")
#' database        <- "FL_R_WRAP"
#' matrix_table    <- "tblMatrixMulti"
#' matrix_id_value <- 5
#' in_matrix       <- FLMatrix(connection, database, matrix_table, matrix_id_value,
#'                    matrix_id = "Matrix_ID", row_id = "Row_ID", column_id = "Col_ID",
#'                    cell_value = "Cell_Val")
#' FLFetchMatrix(in_matrix)
#' }
#' @export
FLFetchMatrix <- function(matrix)
{

	connection <- matrix@ODBC_connection
	sqlParameters <- list(	row_id          = matrix@row_id,
							column_id       = matrix@column_id,
							cell_value      = matrix@cell_value,
							matrix_table    = matrix@matrix_table,
							matrix_id       = matrix@matrix_id,
							id_value 		= matrix@matrix_id_value)
	outputMatrix  <- run_sql(connection, "FLFetchMatrix.sql", sqlParameters)

	return(outputMatrix)

}
