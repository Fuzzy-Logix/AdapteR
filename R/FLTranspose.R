#' @include utilities.R
#' @include FLMatrix.R
#' @include FLIs.R
#' @include FLDims.R
#' @include FLPrint.R
NULL

t<-function(x, ...){
	UseMethod("t", x)
}
#' Matrix Transpose.
#'
#' \code{t} returns the transpose of FLMatrix objects.
#'
#' The wrapper overloads t such that given a matrix of class FLMatrix, t returns the transpose of that object
#' @param object is of class FLMatrix
#' @section Constraints:
#' Input can be a matrix of dimensions (m x n) where m > n, m < n or m = n.
#' @return \code{t} returns a FLMatrix object which is the transpose of input FLMatrix object
#' and replicates the equivalent R output.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' resultFLMatrix <- t(flmatrix)
#' @export
t.FLMatrix<-function(object)
{
	new("FLMatrix", 
		odbc_connection = object@odbc_connection, 
		db_name = object@db_name, 
		matrix_table = object@matrix_table, 
		matrix_id_value = object@matrix_id_value,
		matrix_id_colname = object@matrix_id_colname, 
		row_id_colname = object@col_id_colname, 
		col_id_colname = object@row_id_colname, 
		cell_val_colname = object@cell_val_colname, 
		dimnames = list(object@dimnames[[2]],object@dimnames[[1]]),
        whereconditions=object@whereconditions)
	
}
