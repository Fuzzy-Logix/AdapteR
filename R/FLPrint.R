# Print the objects
#' @export
print.FLMatrix <- function(object)
{
    print(as.matrix(object,sparse=TRUE))
}

## Prints FLVector object
#' @export
print.FLSimpleVector <- function(object)
{
    print(as.vector(object))
}
#' @export
setMethod("show","FLSimpleVector",print.FLSimpleVector)

## Prints FLSimpleWideTable object
#' @export
print.FLSimpleWideTable <- function(object)
{
    print(as.data.frame(object))
}
#' @export
setMethod("show","FLSimpleWideTable",print.FLSimpleWideTable)

## Prints FLVector object
#' @export
print.FLVector <- function(object)
{
    print(as.vector(object))
}

## Prints FLVector object
#' @export
print.FLSimpleVector <- function(object)
{
    print(as.vector(object))
}

