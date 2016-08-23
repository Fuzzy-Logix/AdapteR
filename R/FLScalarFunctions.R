#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLTable.R
#' @include utilities.R
NULL

#' Rounding of Numbers
#'
#' Scalar function which caps an input value at a ceiling or upper boundary.
#'
#' \code{cap} takes a single numeric argument x and returns 
#' a numeric vector containing the smallest integers not 
#' less than the corresponding elements of x.
#' @param x can be an in-database object like FLMatrix,FLVector,FLTable or
#' a normal R object
#' @param ub value at which x is to be capped
#' @return \code{cap} returns an object of same class as input
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' flvector <- as.FLVector(1:5)
#' Result <- cap(flmatrix,1)
#' Result <- cap(flvector,3)
#' @export
setGeneric("cap",function(x,ub)
    standardGeneric("cap"))

setMethod("cap",signature(x="FLAbstractColumn"),
    function(x,ub){
    return(paste0("FLCap(",
            paste0(x@columnName,collapse=",")
            ,",",ub,")"))
})

setMethod("cap",signature(x="FLMatrix"),
    function(x,ub){
    return(constructScalarSQL(pObject=x,
                            pFunc=cap,
                            ub))
})

setMethod("cap",signature(x="FLVector"),
    function(x,ub){
    return(constructScalarSQL(pObject=x,
                            pFunc=cap,
                            ub))
})

setMethod("cap",signature(x="FLTable"),
    function(x,ub){
    return(constructScalarSQL(pObject=x,
                            pFunc=cap,
                            ub))
})
