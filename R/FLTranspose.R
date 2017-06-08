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
#' flmatrix <- FLMatrix(getTestTableName("tblMatrixMulti"), 5,"MATRIX_ID","ROW_ID",
#' 						"COL_ID","CELL_VAL",dims= c(5,5))
#' resultFLMatrix <- t(flmatrix)
#' @seealso \code{\link[base]{t}} for corresponding R function reference
#' @export
t<-function(object, ...){
	UseMethod("t", object)
}

#' @export
t.FLMatrix<-function(object,...){
    object@dimColumns[2:3] <- rev(object@dimColumns[2:3])
    object@select@variables[2:3] <- rev(object@select@variables[2:3])
    object@dims <- rev(object@dims)
    if(!is.null(object@Dimnames))
        object@Dimnames <- rev(object@Dimnames)
    return(object)
}

#' @export
t.FLMatrixBind<-function(object,...){
    ## gk: todo: design deep row/column index swap
    return(t(store(object)))
}
