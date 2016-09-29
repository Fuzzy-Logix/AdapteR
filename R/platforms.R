
#' A FLConnection object stores either a JDBC or a ODBC connection
#' as well as the platform that is connected to.
#' 
#' @export
#' @param connection ODBC/JDBC connection class for connectivity for R
#' @param platform character, either TD, TDAster, or Hadoop
FLConnection <- function(connection, platform)
    # structure(connection=connection,platform=platform,class="FLConnection")
    structure(list(connection),
            platform=platform,
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

##' @export
getFLPlatform <- function(connection=getFLConnection()) return(getFLConnection()$platform)
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
