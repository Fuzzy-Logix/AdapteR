#' @include FLMatrix.R
NULL

## move to file headtail.R
#' Return the First or Last Part of an in-database Object
#'
#' \code{head()} and \code{tail()} functions return the first or last parts of a FLVector, FLMatrix orFLTable.
#'
#' @seealso \code{\link[utils]{head}} , \code{\link[utils]{tail}} for R reference implementation.
#'
#' @param x     an in-database object(FLVector, FLMatrix or FLTable)
#' @param n     a single integer. If positive, size for the resulting object: number of elements for a vector 
#' (including lists), rows for a matrix or data frame or lines for a function. If negative, all but the
#' n last/first number of elements of x.
#' @return returns an object (usually) like x i.e. an in-database object but generally smaller.
#' @section Usage: \code{tail.FLMatrix(x, n=6L, ...)}
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' flmatrix <- FLMatrix("FL_DEMO.tblMatrixMulti", 1,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' headflmatrix <- head(flmatrix, n=6L, ...)
#' print(headflmatrix)
#' @export
head.FLTable <- function(x,n=6L,...){

    stopifnot(length(n) == 1L)
  n <- if (n < 0L) max(nrow(x) + n, 0L) else min(n, nrow(x))
  if(n <= 0) stop("n value in head function is out of bounds")
  x[seq_len(n), ,drop=FALSE]
}

## move to file headtail.R
#' @export
tail.FLTable <- function(x, n = 6L, ...)
{
    stopifnot(length(n) == 1L)
    nrx <- nrow(x)
    n <- if (n < 0L) max(nrx + n, 0L) else min(n, nrx)
    if(n <= 0) stop("n value in head function is out of bounds")
    x[seq.int(to = nrx, length.out = n), , drop = FALSE]
}

## move to file headtail.R
#' @export
head.FLMatrix <- function(x,n=6,...)
return(head.FLTable(x=x,n=n,...))

## move to file headtail.R
#' @export
tail.FLMatrix <- function(x,n=6,...)
return(tail.FLTable(x=x,n=n,...))


## move to file headtail.R
#' @export
head.FLVector <- function(x,n=6,...){

    stopifnot(length(n) == 1L)
    n <- if (n < 0L) max(length(x) + n, 0L) else min(n, length(x))
    if(n <= 0) stop("n value in head function is out of bounds")
    return(x[1:n])
}


## move to file headtail.R
#' @export
tail.FLVector <- function(x,n=6,...){

    stopifnot(length(n) == 1L)
    nrx <- length(x)
    n <- if (n < 0L) max(nrx + n, 0L) else min(n, nrx)
    if(n <= 0) stop("n value in head function is out of bounds")
    return(x[(nrx-n+1):nrx])
}
