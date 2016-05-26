#' @export
head.FLTable <- function(x,n=6,...){
  stopifnot(length(n) == 1L)
  n <- if (n < 0L) max(nrow(x) + n, 0L) else min(n, nrow(x))
  if(n <= 0) stop("n value in head function is out of bounds")
  x[seq_len(n), ,drop=FALSE]
}
#' @export
tail.FLTable <- function(x, n = 6L, ...)
{
    stopifnot(length(n) == 1L)
    nrx <- nrow(x)
    n <- if (n < 0L) max(nrx + n, 0L) else min(n, nrx)
    if(n <= 0) stop("n value in head function is out of bounds")
    x[seq.int(to = nrx, length.out = n), , drop = FALSE]
}

#' @export
head.FLMatrix <- function(x,n=6,...)
return(head.FLTable(x=x,n=n,...))
#' @export
tail.FLMatrix <- function(x,n=6,...)
return(tail.FLTable(x=x,n=n,...))

#' @export
head.FLVector <- function(x,n=6,...){

    stopifnot(length(n) == 1L)
    n <- if (n < 0L) max(length(x) + n, 0L) else min(n, length(x))
    if(n <= 0) stop("n value in head function is out of bounds")
    return(x[1:n])
}

#' @export
tail.FLVector <- function(x,n=6,...){

    stopifnot(length(n) == 1L)
    nrx <- length(x)
    n <- if (n < 0L) max(nrx + n, 0L) else min(n, nrx)
    if(n <= 0) stop("n value in head function is out of bounds")
    return(x[(nrx-n+1):nrx])
}