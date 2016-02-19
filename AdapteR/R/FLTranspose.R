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
    swapRowCol <- function(select){
        newrc <- select@variables$rowIdColumn
        select@variables$rowIdColumn <- select@variables$colIdColumn
        select@variables$colIdColumn <- newrc
        return(select)
    }
    object@select <- swapRowCol(object@select)
    object@mapSelect <- swapRowCol(object@select)
    object@dim <- rev(object@dim)
    object@dimnames <- rev(object@dimnames)
    return(object)
}

t.FLMatrixBind<-function(object){
    ## gk: todo: design deep row/column index swap
    return(t(store(object)))
}
