#' @include FLMatrix.R
NULL

#' Generalized Inverse of a Matrix.
#'
#' \code{ginv} computes the pseudo-inverse for FLMatrix objects.
#'
#' @param object is of class FLMatrix
#' @param ... any additional arguments
#' @section Constraints:
#' Input can only be with maximum dimension limitations of (500 x 500).
#' @return \code{ginv} returns a FLMatrix object which is the pseudo-inverse 
#' of input FLMatrix object and replicates the equivalent R output.
#' @examples
#' flmatrix <- FLMatrix(getTestTableName("tblMatrixMulti"), 1,"MATRIX_ID",
#'                      "ROW_ID","COL_ID","CELL_VAL",dims= c(2,3))
#' resultFLMatrix <- ginv(flmatrix)
#' @seealso \code{\link[MASS]{ginv}} for corresponding R function reference
#' @export
ginv <- function (object, ...){
	UseMethod("ginv", object)
}

#' @export
ginv.default <- function(object,...){
    if (!requireNamespace("MASS", quietly = TRUE)){
            stop("MASS package needed for ginv. Please install it.",
            call. = FALSE)
            }
    else return(MASS::ginv(object,...))
}

#' @export
ginv.FLMatrix<-function(object,...)
{
    
    flm <- constructMatrixUDTSQL(pObject=object,
                                 pFuncName="FLMatrixPseudoInvUdt",
                                 pdims=rev(getDimsSlot(object)),
                                 pdimnames=rev(dimnames(object))
                                 )
    
    return(ensureQuerySize(pResult=flm,
                            pInput=list(object,...),
                            pOperator="ginv",
                            pStoreResult=TRUE))

}
