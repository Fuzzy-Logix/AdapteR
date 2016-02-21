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
#' @param x is of class FLMatrix
#' @section Constraints:
#' Input can be a matrix of dimensions (m x n) where m > n, m < n or m = n.
#' @return \code{t} returns a FLMatrix object which is the transpose of input FLMatrix object
#' and replicates the equivalent R output.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_DEMO", "tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLMatrix <- t(flmatrix)
##' @author  Gregor Kappler <g.kappler@@gmx.net>, phani srikar <phanisrikar93ume@gmail.com>
#' @export
t<-function(x, ...){
	UseMethod("t", x)
}

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
