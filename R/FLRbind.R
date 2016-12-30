#' @include FLMatrix.R
NULL

#' @export
rbind.default <- base::rbind

#' Combine objects by rows.
#'
#' \code{rbind} combines input objects by rows and forms a FLMatrix.
#'
#' \code{rbind} takes a sequence of vector, FLVector, matrix, FLMatrix or data frames arguments,
#' combines them by rows and makes a FLMatrix.
#' @param x... can be a sequence of vector, FLVector, matrix, FLMatrix or data frames
#' @section Constraints:
#' Input matrices, FLMatrices and data frames should have same number of columns.
#' @return \code{rbind} returns a FLMatrix object which is the row wise combination of input arguments.
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 5)
#' resultFLMatrix <- rbind(flmatrix,1:5,flmatrix)
#' @export
rbind.FLMatrix<-function(x,...) FLbind(list(x,...),1)

#' @export
rbind.FLMatrixBind <- rbind.FLMatrix
