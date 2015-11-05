#' @include utilities.R
NULL
setOldClass("RODBC")
#' An S4 class to represent FLMatrix
#'
#' @slot odbc_connection ODBC connectivity for R
#' @slot db_name character
#' @slot matrix_table character
#' @slot matrix_id_value numeric id value
#' @slot matrix_id_colname character
#' @slot row_id_colname character
#' @slot col_id_colname character
#' @slot cell_val_colname character
#' @slot nrow numeric number of rows of FLMatrix object
#' @slot ncol numeric number of cols of FLMatrix object
#' @slot dimnames list dimension names of FLMatrix object
#'

# Creating FLMatrix datatype
setClass(
	"FLMatrix",
	slots = list(
		odbc_connection = "RODBC",
		db_name = "character",
		matrix_table = "character",
		matrix_id_value	= "numeric",
		matrix_id_colname = "character",
		row_id_colname = "character",
		col_id_colname = "character",
		cell_val_colname = "character",
		nrow = "numeric",
		ncol = "numeric",
		dimnames = "list"
	)
)

#' Constructor function for FLMatrix.
#'
#' \code{FLMatrix} constructs an object of class \code{FLMatrix}.
#'
#' \code{FLMatrix} object is an in-database equivalent to matrix object.
#' This object is used as input for matrix operation functions.
#' @param connection ODBC connection handle as returned by \code{\link[RODBC]{odbcConnect}}
#' @param database name of the database
#' @param matrix_table name of the matrix table
#' @param matrix_id_value identifier for the input matrix
#' @param matrix_id_colname matrix id value in \code{matrix_table}
#' @param row_id_colname column name in \code{matrix_table} where row numbers are stored
#' @param col_id_colname column name in \code{matrix_table} where column numbers are stored
#' @param cell_val_colname column name in \code{matrix_table} where matrix elements are stored
#' @return \code{FLMatrix} returns an object of class FLMatrix mapped
#' to an in-database matrix.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' @export

FLMatrix <- function(connection, 
					 database, 
					 matrix_table, 
					 matrix_id_value,
					 matrix_id_colname = "MATRIX_ID", 
					 row_id_colname = "ROW_ID", 
					 col_id_colname = "COL_ID", 
					 cell_val_colname = "CELL_VAL", 
					 nrow = c(), 
					 ncol = c(), 
					 dimnames = list(c(),c()))
{
	# Select and set access to the entire database and its functions
	sqlQuery(connection,
			 paste("DATABASE", database,";
					SET ROLE ALL;"))
	
	nrow <- sqlQuery(connection, 
					 paste0("SELECT max(",row_id_colname,") 
							 FROM ",matrix_table," 
							 WHERE ",matrix_id_colname,"=",matrix_id_value))[1,1]

	ncol <- sqlQuery(connection,
					 paste0("SELECT max(",col_id_colname,")
					 		 FROM ",matrix_table," 
					 		 WHERE ",matrix_id_colname,"=",matrix_id_value))[1,1]
	

	if(length(dimnames)!=0 && ((length(dimnames[[1]])!=0 && length(dimnames[[1]])!=nrow) ||
	  (length(dimnames[[2]])!=0 && length(dimnames[[2]])!=nrow)))
	{
		stop(" ERROR in dimnames: length of dimnames not equal to array extent ")
	}
    else
    {
		new("FLMatrix", 
			odbc_connection = connection, 
			db_name = database, 
			matrix_table = matrix_table, 
			matrix_id_value = matrix_id_value, 
			matrix_id_colname = matrix_id_colname, 
			row_id_colname = row_id_colname, 
			col_id_colname = col_id_colname, 
			cell_val_colname = cell_val_colname,
			nrow = nrow, 
			ncol = ncol, 
			dimnames = dimnames)
    }
}

max_matrix_id_value <- 0
max_matrix_id_value <- max_matrix_id_value + 1
result_db_name <- "FL_TRAIN"
result_matrix_table <- gen_unique_table_name("tblMatrixMultiResult")
# result_matrix_table <- "tblMatrixMultiResult"
flag1 <- 0

print.FLMatrix <- function(object)
{
	sqlQuery(object@odbc_connection, 
			 paste0("DATABASE", object@db_name,"; 
			 		 SET ROLE ALL;"))
	nrows <- object@nrow
	valuedf <- sqlQuery(object@odbc_connection, 
						paste0("SELECT * 
								FROM ",object@matrix_table," 
								WHERE ",object@matrix_id_colname,"=",object@matrix_id_value," 
								ORDER BY 1,2,3"))
	print(matrix(valuedf[,object@cell_val_colname],
		  nrows,
		  length(valuedf[,object@cell_val_colname])/nrows,
		  byrow=T,
		  dimnames = object@dimnames))
}


setMethod("show","FLMatrix",print.FLMatrix)

