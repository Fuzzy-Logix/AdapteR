#' @export
setClass("FLConnection",slots=list())


#' A FLConnection object stores either a JDBC or a ODBC connection
#' as well as the platform that is connected to.
#' 
#' @export
#' @param connection ODBC/JDBC connection class for connectivity for R
#' @param platform character, either TD, TDAster, or Hadoop
FLConnection <- function(connection, platform, name)
    # structure(connection=connection,platform=platform,class="FLConnection")
    structure(list(connection),
              platform=platform,
              name=name,
            class="FLConnection",
            names="connection")

##' @export
setGeneric("getFLConnection", function(object) {
    standardGeneric("getFLConnection")
})
setMethod("getFLConnection", signature(object = "ANY"), function(object) getFLConnection())
setMethod("getFLConnection", signature(object = "missing"), function(object) getOption("FLConnection"))
## setMethod("getConnection", signature(object = "FLMatrix"), function(object) object@select@connection)
## setMethod("getConnection", signature(object = "FLTable"), function(object) object@select@connection)
## setMethod("getConnection", signature(object = "FLTableQuery"), function(object) object@select@connection)
## setMethod("getConnection", signature(object = "FLVector"), function(object) object@select@connection)

getFLConnectionName <- function(...) attr(getFLConnection(...),"name")

##' @export
getFLPlatform <- function(connection=getFLConnection()) return(attr(getFLConnection(),"platform"))
is.TD         <- function() getFLPlatform()=="TD"
is.TDAster    <- function() getFLPlatform()=="TDAster"
is.Hadoop     <- function() getFLPlatform()=="Hadoop"

##' @export
setGeneric("getRConnection", function(object) {
    standardGeneric("getRConnection")
})
setMethod("getRConnection", 
    signature(object = "FLConnection"), 
    function(object) 
    object$connection)
