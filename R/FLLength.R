#' @include FLDims.R
#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
NULL

length <- function(obj)
{
	UseMethod("length",obj)
}

length.default <- base::length

#' computes the length of FLMatrix object.
#' @param obj is a FLMatrix object.
#' @return \code{length} returns a R Vector giving the length of input object.
length.FLMatrix <- function(obj)
{
	return(dim(obj)[1]*dim(obj)[2])
}

#' computes the length of FLSparseMatrix object.
#' @param obj is a FLSparseMatrix object.
#' @return \code{length} returns a R Vector giving the length of input object.
length.FLSparseMatrix <- function(obj)
{
	return(dim(obj)[1]*dim(obj)[2])
}

#' computes the length of FLVector object.
#' @param obj is a FLVector object.
#' @return \code{length} returns a R Vector giving the length of input object.
length.FLVector <- function(obj)
{
	return(obj@size)
}