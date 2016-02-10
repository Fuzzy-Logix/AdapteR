#' @include utilities.R
NULL
setOldClass("RODBC")
#' An S4 class to represent FLSparseMatrix
#'
#' @slot connection ODBC connectivity for R
#' @slot database character
#' @slot table_name character
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
		connection = "ANY",
		database = "character",
		table_name = "character",
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
#' @param table_name name of the SparseMatrix table
#' @param matrix_id_value identifier for the input matrix
#' @param matrix_id_colname matrix id value in \code{table_name}
#' @param row_id_colname column name in \code{table_name} where row numbers are stored
#' @param col_id_colname column name in \code{table_name} where column numbers are stored
#' @param cell_val_colname column name in \code{table_name} where matrix elements are stored
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
						   table_name, 
						   matrix_id_value,
						   matrix_id_colname = "MATRIX_ID", 
						   row_id_colname = "rowIdColumn", 
						   col_id_colname = "colIdColumn", 
						   cell_val_colname = "valueColumn", 
						   dimnames = list(c(),c()))
    FLMatrix(connection,
             database, 
             table_name, 
             matrix_id_value,
             matrix_id_colname, 
             row_id_colname, 
             col_id_colname, 
             cell_val_colname, 
             dimnames)

## FLSparseMatrix <- function(connection,
## 						   database, 
## 						   table_name, 
## 						   matrix_id_value,
## 						   matrix_id_colname = "MATRIX_ID", 
## 						   row_id_colname = "rowIdColumn", 
## 						   col_id_colname = "colIdColumn", 
## 						   cell_val_colname = "valueColumn", 
## 						   dimnames = list(c(),c()))
## {
## 	nrow <- sqlQuery(connection,
## 					 paste0("SELECT max(",row_id_colname,")
## 					 		 FROM ",table_name," 
## 					 		 WHERE ",matrix_id_colname,"=",matrix_id_value))[1,1]
## 	ncol <- sqlQuery(connection,
## 					 paste0("SELECT max(",col_id_colname,")
## 					 		 FROM ",table_name," 
## 					 		 WHERE ",matrix_id_colname,"=",matrix_id_value))[1,1]
## 	if(length(dimnames)!=0 && ((length(dimnames[[1]])!=0 && length(dimnames[[1]])!=nrow) || 
## 	  (length(dimnames[[2]])!=0 && length(dimnames[[2]])!=nrow)))
## 	{
##       	stop(" ERROR in dimnames: length of dimnames not equal to array extent ")
## 	} 
##     else
##     {
## 		new("FLSparseMatrix",
## 			 connection = connection,
## 			 database = database, 
## 			 table_name = table_name, 
## 			 matrix_id_value = matrix_id_value, 
## 			 matrix_id_colname = matrix_id_colname, 
## 			 row_id_colname = row_id_colname, 
## 			 col_id_colname = col_id_colname, 
## 			 cell_val_colname = cell_val_colname,
## 	         nrow = nrow, 
## 	         ncol = ncol, 
## 	         dimnames = dimnames)    	
##     }
## }

pToj <- function(p) 
{
	dp <- diff(p)
  	rep(seq_along(dp), dp)
}
