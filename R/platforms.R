
##' @export
setGeneric("getFLConnection", function(object) {
    standardGeneric("getFLConnection")
})
setMethod("getFLConnection", signature(object = "ANY"), function(object) getFLConnection())
setMethod("getFLConnection", signature(object = "missing"), function(object) getOption("connectionFL"))
## setMethod("getConnection", signature(object = "FLMatrix"), function(object) object@select@connection)
## setMethod("getConnection", signature(object = "FLTable"), function(object) object@select@connection)
## setMethod("getConnection", signature(object = "FLTableQuery"), function(object) object@select@connection)
## setMethod("getConnection", signature(object = "FLVector"), function(object) object@select@connection)

##' @export
getFLPlatform <- function(connection=getFLConnection()) return(getOption("FLPlatform"))
is.TD         <- function() getFLPlatform()=="TD"
is.TDAster    <- function() getFLPlatform()=="TDAster"
is.Hadoop     <- function() getFLPlatform()=="Hadoop"


