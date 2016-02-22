#' @include FLconstructSQL.R

#' An S4 class to represent FLMatrix
#'
#' @slot connection ODBC connectivity for R
#' @slot by character either rows or cols
setClass("FLMatrixBind",
         slots = list(parts = "list",
                      by = "numeric",
                      offsets = "numeric",
                      dimnames = "list"))

namesOrIndex <- function(x){
    if(!is.null(names(x)))
        return(names(x))
    else
        return(1:length(x))
}


orderVariables <- function(P,varNames){
    if("select" %in% slotNames(P))
        P@select <- orderVariables(P@select,varNames)
    if("variables" %in% slotNames(P)){
        ##print(P@variables[[varName]])
        P@variables <- P@variables[varNames]
        ##print(P@variables[[varName]])
    }
    if("parts" %in% slotNames(P))
        P@parts <- llply(P@parts, orderVariables,
                         varNames)
    P
}
updateVariable <- function(P,varName,value){
    ##print(str(P))
    ##browser()
    if("select" %in% slotNames(P))
        P@select <- updateVariable(P@select,varName,value)
    if("variables" %in% slotNames(P)){
        ##print(P@variables[[varName]])
        if(!is.null(P@variables[[varName]]))
            value <- gsub("'\\.'",".", ## gk: improve with better concatenation wout '.'
                          paste0(as.character(value),".",
                                 as.character(P@variables[[varName]])))
        P@variables[[varName]] <- value
        cat(paste0("setting var ",varName,"=",value,"\n"))
        ##print(P@variables[[varName]])
    }
    if("parts" %in% slotNames(P))
        P@parts <- llply(P@parts, updateVariable,
                         varName,value)
    P
}

##' Bind a matrix/array by an index. Currently limited to matrices
##' with character dimnames
##' 
##' @param parts 
##' @param by the numeric index by which binding takes place
##' @return returns a remote matrix object defining the deep table sql for the *bound result.
FLMatrixBind <- function(parts,by){
    dims <- ldply(parts, function(p) dim(p))
    ##print(dims) ## todo: adjust
    ##browser()
    if(length(unique(dims[[by]]))>2)
        stop("Binding of arrays with different dimensions not implemented")
    offsets <- apply(dims,2,cumsum)
    ##browser()
    offsName <- c("rowIdOffset",
                  "colIdOffset")[[by]]
    offsets[,by] <-  offsets[,by]-offsets[1,by]
    if(by==2)
        dimnames <- list(rownames(parts[[1]]),
                         unlist(llply(parts,colnames)))
    else if(by==1)
        dimnames <- list(unlist(llply(parts,rownames)),
                         colnames(parts[[2]]))
    new("FLMatrixBind",
        parts = llply(
            1:length(parts),
            function(n){
                ## this adds constant for identifying the binding
                ## structure and offsets from a union select.
                ## statement
                #browser()
                P <- parts[[n]]
                P <- updateVariable(
                    P,
                    "boundBy",
                    paste0("'",by,"'"))
                P <- updateVariable(
                    P,
                    "bindId",
                    paste0("'",n,"'"))
                P <- updateVariable(P,offsName,
                                    offsets[n,by])
                P
            }),
        dimnames=dimnames,
        by=by)
}

setMethod("suffixAlias",
          signature(object = "FLMatrixBind",suffix="character"),
          function(object,suffix){
              object@parts <- llply(1:length(object@parts),
                                    function(n)
                                        suffixAlias(object@parts[[n]],letters[n]))
              return(object)
          })


## gk,partha,phani: discuss if variable names could differ
setMethod("getVariables",
          signature(object = "FLMatrixBind"),
          function(object) object@parts[[1]]@variables)

setMethod("constructSelect",
          signature(object = "FLMatrixBind"),
          function(object,joinNames=TRUE) {
              constructNselect <- function(n){
                  ##browser()
                  flm <- object@parts[[n]]
                  suffix <- letters[n]
                  constructSelect(
                      suffixAlias(flm,suffix),
                      joinNames=joinNames)
              }
              paste0(unlist(
                  llply(1:length(object@parts),
                        constructNselect)),
                  collapse=" UNION ALL ")
          })

setMethod("getConnection",
          signature(object = "FLMatrixBind"),
          function(object) getConnection(object@parts[[1]]))

setMethod("store",
          signature(object = "FLMatrixBind",returnType="missing",connection="missing"),
          function(object) store.FLMatrix(object))






print.FLMatrixBind <- function(object)
{
    ##gk: todo: implement caching
    print(as.matrix(object,sparse=TRUE))
}

setMethod("show","FLMatrixBind",print.FLMatrixBind)

FLbind <- function(objectList,by){
    if(all(sapply(objectList,function(x) is.FLMatrix(x) &
                                         !any(sapply(dimnames(x),is.null))))){
        return(FLMatrixBind(parts=objectList,by=by))
    }
    stop("Binding currently only supported for FLMatrix objects with dimnames.")
}
