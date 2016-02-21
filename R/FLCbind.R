#' @include utilities.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL


         
#' Combine objects by columns.
#'
#' \code{cbind} combines input objects by columns and forms a FLMatrix.
#'
#' \code{cbind} takes a sequence of vector, FLVector, matrix, FLMatrix or data frames arguments,
#' combines them by columns and makes a FLMatrix.
#' @param x... can be a sequence of vector, FLVector, matrix, FLMatrix or data frames
#' @section Constraints:
#' Input matrices, FLMatrices and data frames should have same number of rows.
#' @return \code{cbind} returns a FLMatrix object which is the column-wise combination of input arguments.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLMatrix <- cbind(flmatrix,1:5,flmatrix)
#' @export
cbind <- function (x, ...){
  UseMethod("cbind", x)
}


cbind.default <- function(object,...){
    cbind(as.FLMatrix(object,object@connection),...)
}

#' Combine objects by columns.
#'
#' \code{cbind} combines input objects by columns and forms a FLMatrix.
#'
#' \code{cbind} takes a sequence of vector, FLVector, matrix, FLMatrix or data frames arguments,
#' combines them by columns and makes a FLMatrix.
#' @param x... can be a sequence of vector, FLVector, matrix, FLMatrix or data frames
#' @section Constraints:
#' Input matrices, FLMatrices and data frames should have same number of rows.
#' @return \code{cbind} returns a FLMatrix object which is the column-wise combination of input arguments.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLMatrix <- cbind(flmatrix,1:5,flmatrix)
#' @export
cbind.FLMatrix<-function(object,...)
{
    objectList<-list(object,...)
    if(all(sapply(objectList,is.FLMatrix))){
        return(FLMatrixBind(parts=objectList,by=2))
    }
    stop()
}
cbind.FLMatrixBind <- cbind.FLMatrix

