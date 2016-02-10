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
t.FLMatrix<-function(object){
    if(class(object@select)=="FLTableFunctionQuery")
    object <- store(object)
	return(FLMatrix( 
            connection = getConnection(object), 
            database = object@select@database, 
            table_name = object@select@table_name, 
            matrix_id_value = "",
            matrix_id_colname = getVariables(object)$matrixId, 
            row_id_colname = getVariables(object)$colIdColumn, 
            col_id_colname = getVariables(object)$rowIdColumn, 
            cell_val_colname = getVariables(object)$valueColumn, 
            dimnames = list(object@dimnames[[2]],object@dimnames[[1]]),
            whereconditions=object@select@whereconditions)
            )
}

t.FLMatrixBind<-function(object){
    ## gk: todo: design deep row/column index swap
    return(t(store(object)))
}
