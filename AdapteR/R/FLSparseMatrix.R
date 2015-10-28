#' @include utilities.R
NULL
setOldClass("RODBC")
#' An S4 class to represent FLSparseMatrix
#'
#' @slot odbc_connection ODBC connectivity for R
#' @slot db_name character
#' @slot matrix_table character
#' @slot matrix_id_value numeric id value
#' @slot matrix_id_colname character
#' @slot row_id_colname character
#' @slot col_id_colname character
#' @slot cell_val_colname character
#' @slot nrow numeric number of rows of FLSparseMatrix object
#' @slot ncol numeric number of cols of FLSparseMatrix object
#' @slot dimnames list dimension names of FLSparseMatrix object
#'
setClass(
	"FLSparseMatrix",
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
#' Constructor function for FLSparseMatrix.
#'
#' \code{FLSparseMatrix} constructs an object of class \code{FLSparseMatrix}.
#'
#' \code{FLSparseMatrix} object is an in-database equivalent to sparseMatrix object.
#' This object is used as input for matrix operation functions.
#' @param connection ODBC connection handle as returned by \code{\link[RODBC]{odbcConnect}}
#' @param database name of the database
#' @param matrix_table name of the SparseMatrix table
#' @param matrix_id_value identifier for the input matrix
#' @param matrix_id_colname matrix id value in \code{matrix_table}
#' @param row_id_colname column name in \code{matrix_table} where row numbers are stored
#' @param col_id_colname column name in \code{matrix_table} where column numbers are stored
#' @param cell_val_colname column name in \code{matrix_table} where matrix elements are stored
#' @param dimnames dimension names of \code{FLSparseMatrix}
#' @return \code{FLSparseMatrix} returns an object of class FLSparseMatrix mapped
#' to a sparseMatrix.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flsparsematrix <- FLSparseMatrix(connection, "FL_TRAIN", "tblMatrixMultiSparse", 2)
#' @export
FLSparseMatrix <- function(connection,
						   database, 
						   matrix_table, 
						   matrix_id_value,
						   matrix_id_colname = "MATRIX_ID", 
						   row_id_colname = "ROW_ID", 
						   col_id_colname = "COL_ID", 
						   cell_val_colname = "CELL_VAL", 
						   dimnames = list(c(),c()))
{
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
		new("FLSparseMatrix",
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

pToj <- function(p) 
{
	dp <- diff(p)
  	rep(seq_along(dp), dp)
}

max_Sparsematrix_id_value <- 0
max_Sparsematrix_id_value <- max_Sparsematrix_id_value + 1
result_db_name <- "FL_TRAIN"
result_Sparsematrix_table <- gen_unique_table_name("tblMatrixMultiResultSparse")
# result_Sparsematrix_table <- "tblMatrixMultiResultSparse"
flag2 <- 0

# Prints FLSparseMatrix object
print.FLSparseMatrix <- function(object)
{
	sqlQuery(object@odbc_connection, paste0("DATABASE", object@db_name,"; SET ROLE ALL;"))
	nrow <- object@nrow
	valuedf <- sqlQuery(object@odbc_connection, paste0("SELECT * FROM ",object@matrix_table," WHERE ",object@matrix_id_colname,"=",
						object@matrix_id_value," ORDER BY 1,2,3"))
	sparseMatrix(i=valuedf[,object@row_id_colname],j=valuedf[,object@col_id_colname],x=valuedf[,object@cell_val_colname],dimnames = object@dimnames)
}

setMethod("show","FLSparseMatrix",print.FLSparseMatrix)

