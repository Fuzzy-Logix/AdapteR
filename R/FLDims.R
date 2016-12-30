# Gives the dimensions of the objects

## #' @export
## dim.default <- base::dim
#' @export
dim.FLIndexedValues <- function(object)
{
    return(object@dims)
}

# Returns the dimensions of the object
#' @export
dim.FLMatrixBind <- function(object)
{
    return(c(nrow(object),ncol(object)))
}

#' @export
dim.FLTable <- function(object)
{
    return(object@dims)
}



#' @export
dimnames.FLIndexedValues <- function(object)
  return(object@Dimnames)
#' @export
dimnames.FLTable <- function(object)
  return(object@Dimnames)
#' @export
dimnames.FLTableMD <- function(object)
  return(object@Dimnames)
#' @export
dimnames.FLAbstractTable <- function(object)
  return(object@Dimnames)
