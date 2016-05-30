#' @include utilities.R
#' @include FLMatrix.R
#' @include FLIs.R
#' @include FLDims.R
#' @include FLPrint.R
NULL

#' Matrix Transpose.
#'
#' \code{t} returns the transpose of FLMatrix objects.
#'
#' @param object is of class FLMatrix
#' @param ... any additional arguments
#' @section Constraints:
#' Input can be a matrix of dimensions (m x n) where m > n, m < n or m = n.
#' @return \code{t} returns a FLMatrix object which is the transpose of input FLMatrix object
#' and replicates the equivalent R output.
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' flmatrix <- FLMatrix("FL_DEMO", 
#' "tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLMatrix <- t(flmatrix)
#' @export
t<-function(object, ...){
	UseMethod("t", object)
}

#' @export
t.FLMatrix<-function(object,...){
    if(class(object@select)=="FLTableFunctionQuery")
        object <- store(object)
    swapRowCol <- function(select){
        newrc <- select@variables$rowIdColumn
        select@variables$rowIdColumn <- select@variables$colIdColumn
        select@variables$colIdColumn <- newrc
        return(select)
    }
    object@select <- swapRowCol(object@select)
    object@mapSelect <- swapRowCol(object@mapSelect)
    object@dim <- rev(object@dim)
    object@dimnames <- rev(object@dimnames)
    return(object)
}

#' @export
t.FLMatrixBind<-function(object,...){
    ## gk: todo: design deep row/column index swap
    return(t(store(object)))
}
