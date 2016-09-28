#' @include FLMatrix.R
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
#' flmatrix <- FLMatrix("tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLMatrix <- t(flmatrix)
#' @export
t<-function(object, ...){
	UseMethod("t", object)
}

#' @export
t.FLMatrix<-function(object,...){
    object@dimColumns[1:2] <- rev(object@dimColumns[1:2])
    object@dim <- rev(object@dims)
    if(!is.null(object@Dimnames))
        object@Dimnames <- rev(object@Dimnames)
    return(object)
}

#' @export
t.FLMatrixBind<-function(object,...){
    ## gk: todo: design deep row/column index swap
    return(t(store(object)))
}
