#' @include FLMatrix.R
NULL

#' @export
length <- function(obj)
{
	UseMethod("length",obj)
}

#' @export
length.default <- base::length

#' computes the length of FLMatrix object.
#' @param obj is a FLMatrix object.
#' @return \code{length} returns a R Vector giving the length of input object.
#' @export
length.FLMatrix <- function(obj)
{
	return(dim(obj)[1]*dim(obj)[2])
}
#' @export
length.FLMatrixBind <- function(obj)
{
	return(dim(obj)[1]*dim(obj)[2])
}


#' computes the length of FLVector object.
#' @param obj is a FLVector object.
#' @return \code{length} returns a R Vector giving the length of input object.
#' @export
length.FLVector <- function(obj)
{
    if(length(obj@dims)>0)
        vlength <- max(obj@dims)
    else vlength <- max(length(dimnames(obj)[[2]]),
                        length(dimnames(obj)[[1]]))
    return(vlength)
}

#' computes the length of FLTable object.
#' @param obj is a FLTable object.
#' @return \code{length} returns a R Vector giving the
#' number of observations or rows in input FLTable object.
#' @export
length.FLTable <- function(obj)
{
	return(ncol(obj))
}
