#' @include utilities.R
#' @include FLIs.R
#' @include FLCastFunctions.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLSparseMatrix.R
#' @include FLTable.R
#' @include FLDims.R
#' @include FLPrint.R
#' @include FLIdentical.R
NULL

#' Equality of in-database objects.
#'
#' \code{==} checks the equality of in-database objects.
#'
#' The equality of in-database objects mimics the normal addition of R data types.
#' One can check equality of FLMatrices, FLMatrix - R matrices, FLVectors and
#' FLVector - RVector.
#' @param x can be an in-database object like FLMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param y can be an in-database object like FLMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{==} returns a logical TRUE or FALSE.
#' @section Constraints:
#' Currently only \code{dgCMatrix},\code{dgeMatrix},\code{dsCMatrix},
#' \code{dgTMatrix},\code{matrix},\code{Matrix},\code{vector} R types
#' are supported.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 1)
#' Rvector <- 1:5
#' Result <- flmatrix == flmatrix
#' Result <- Rvector == as.FLVector(Rvector,connection)
#' @export

# "==" <- function(pObj1, pObj2)
# {
#     UseMethod("==", pObj1)
# }

# `==.default` <- function(pObj1,pObj2)
# {
# 	op <- .Primitive("==")
# 	op(pObj1,pObj2)
# }

# `==.FLMatrix` <- function(pObj1,pObj2)
# return(identical(pObj1,pObj2))

# `==.FLVector` <- function(pObj1,pObj2)
# return(identical(pObj1,pObj2))

# `==.matrix` <- function(pObj1,pObj2)
# return(identical(pObj1,pObj2))

# `==.dgCMatrix` <- function(pObj1,pObj2)
# return(identical(pObj1,pObj2))

# `==.dgTMatrix` <- function(pObj1,pObj2)
# return(identical(pObj1,pObj2))

# `==.dgeMatrix` <- function(pObj1,pObj2)
# return(identical(pObj1,pObj2))

# `==.dsCMatrix` <- function(pObj1,pObj2)
# return(identical(pObj1,pObj2))

# `==.numeric` <- function(pObj1,pObj2)
# return(identical(pObj1,pObj2))

