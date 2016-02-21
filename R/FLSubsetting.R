#' @include utilities.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLTable.R
#' @include FLIs.R
#' @include FLDims.R
#' @include FLPrint.R
NULL

#' Extract part of FLMatrix object.
#'
#' \code{[]} acts on FLMatrix objects and extracts parts of them.
#'
#'
#' @param object is a FLMatrix object
#' @param rows is a vector input corresponding to rows to be extracted
#' @param cols is a vector input corresponding to columns to be extracted
#' @return \code{[]} returns FLMatrix object after extraction
#' which replicates the equivalent R extraction.
#' @section Constraints:
#' Applying UDT functions on subsetted matrices with discontinuous row and col ids' 
#' may result in error
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_DEMO", "tblMatrixMulti", 2,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLmatrix <- flmatrix[1,]
##' @author  Gregor Kappler <g.kappler@@gmx.net>
#' @export
`[.FLMatrix`<-function(object,rows=1,cols=1, drop=TRUE)
{
    ##browser()
	connection<-getConnection(object)
	## if(nargs()==2 && missing(rows)) { return(object[,]) }
	## if(nargs()==2)
	## {
    ##     stop("FLVector, drop?")
	## 	if(rows>nrow(object)*ncol(object)) { stop("subscript_out_of_bounds") }
	## 	return(sqlQuery(connection,paste0(" SELECT ",getVariables(object)$valueColumn,
    ##                                       " FROM ",remoteTable(object),
    ##                                       constructWhere(
    ##                                           constraintsSQL(object)),
    ##                                       " ORDER BY ",object@matrix_id_colname,",",getVariables(object)$colIdColumn,",",getVariables(object)$rowIdColumn))[[1]][rows])
	## }


    newrownames <- rows
    newcolnames <- cols

    if(missing(cols))
    {
        if (missing(rows)) return(object)
        if(length(unique(rows))!=length(rows))
            stop("Duplicate use of indices not supported so far")
        return(restrictFLMatrix(
                 object = object,
                 whereconditions = object@select@whereconditions,
                 dimnames = list(newrownames,
                                 object@dimnames[[2]]),
                 conditionDims=c(TRUE,FALSE)))
    }
    else { ## !missing(cols)
        if(missing(rows)) {
            if(length(unique(cols))!=length(cols))
                stop("Duplicate use of indices not supported so far")
            return(restrictFLMatrix(
                object = object,
                whereconditions = object@select@whereconditions,
                dimnames = list(object@dimnames[[1]],
                                newcolnames),
                conditionDims=c(FALSE,TRUE)))
        } else {  ## !missing(cols) and !missing(rows)
            if(length(unique(rows))!=length(rows) | length(unique(cols))!=length(cols))
                stop("Duplicate use of indices not supported so far")
            return(restrictFLMatrix(
                object = object,
                whereconditions = object@select@whereconditions,
                dimnames = list(newrownames,
                                newcolnames),
                conditionDims=c(TRUE,TRUE)))
        }
    }
}

#' Extract part of FLTable object.
#'
#' \code{[]} acts on FLMatrix objects and extracts parts of them.
#'
#'
#' @param object is a FLTable object
#' @param rows is a vector input corresponding to rows to be extracted
#' @param cols is a vector input corresponding to columns to be extracted
#' @return \code{[]} returns FLMatrix object after extraction
#' which replicates the equivalent R extraction.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' fltable <- FLTable(connection, "FL_DEMO", "tblAbaloneWide", "ObsID")
#' resultFLtable <- fltable[1:10,4:6]
##' @author  Gregor Kappler <g.kappler@@gmx.net>
#' @export
`[.FLTable`<-function(object,rows=1,cols=1,drop=TRUE)
{
	connection<-getConnection(object)
    if(is.numeric(rows))
        newrownames <- object@dimnames[[1]][rows]
    else
        newrownames <- rows

    if(is.numeric(cols))
        newcolnames <- object@dimnames[[2]][cols]
    else
        newcolnames <- cols

    ##browser()
    if(missing(cols))
    {
        if (!missing(rows)) {
            if(!setequal(object@dimnames[[1]],
                         newrownames))
                object@select@whereconditions <- c(object@select@whereconditions,
                                            inCondition(paste0(object@select@database,".",
                                                               object@select@table_name,".",
                                                               getVariables(object)$obs_id_colname),
                                                        newrownames))
            object@dimnames <- list(newrownames,
                                   object@dimnames[[2]])
        }
    } else if(missing(rows)) { ## !missing(cols)
        ifelse(any(is.na(as.numeric(object@dimnames[[1]]))),
               newrownames <- sort(object@dimnames[[1]]),
               newrownames <- sort(as.numeric(object@dimnames[[1]])))
        object@dimnames <- list(newrownames,
                                newcolnames)
        if(object@isDeep){
            object@select@whereconditions <-
                c(object@select@whereconditions,
                  inCondition(paste0(object@select@select@database,".",
                                     object@select@table_name,".",
                                     getVariables(object)$var_id_colname),
                              object@dimnames[[2]]))
        }
    } else {  ## !missing(cols) and !missing(rows)
        ##browser()
        if(!setequal(object@dimnames[[1]], newrownames))
            object@select@whereconditions <-
            c(object@select@whereconditions,
              inCondition(paste0(object@select@database,".",
                                 object@select@table_name,".",
                                 getVariables(object)$obs_id_colname),
                          newrownames))
        if(object@isDeep & !setequal(object@dimnames[[2]], newcolnames)){
            object@select@whereconditions <-
                c(object@select@whereconditions,
                  inCondition(paste0(object@select@database,".",
                                     object@select@table_name,".",
                                     getVariables(object)$var_id_colname),
                              newcolnames))
        }
        object@dimnames = list(newrownames, newcolnames)
    }
    if(drop & (ncol(object)==1 | nrow(object) == 1))
        return(new("FLVector",
                    select=object@select,
                    dimnames=object@dimnames,
                    isDeep=object@isDeep))
    else return(object)
}


#' Extract part of FLVector object.
#'
#' \code{[]} acts on FLVector objects and extracts parts of them.
#'
#'
#' @param object is a FLVector object
#' @param pSet is a vector representing the indices of elements to extract
#' @return \code{[]} returns FLVector object after extraction
#' which replicates the equivalent R extraction.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' WideTable <- FLTable(connection, "FL_DEMO", "tblAbaloneWide","ObsID")
#' flvector <- FLVector[,"Diameter"]
#' resultFLVector <- flvector[10:1]
##' @author phani srikar <phanisrikar93ume@gmail.com>
#' @export

`[.FLVector` <- function(object,pSet=1:length(object))
{
    if(any(pSet>length(object))) stop("index out of bounds")
    if(ncol(object)==1)
    {
        newrownames <- object@dimnames[[1]][pSet]
        names(object@select@table_name) <- NULL
        if(!setequal(object@dimnames[[1]], newrownames))
            object@select@whereconditions <-
            c(object@select@whereconditions,
              inCondition(paste0(remoteTable(object@select),".",getVariables(object)$obs_id_colname),
                          newrownames))
        object@dimnames[[1]] <- newrownames
    }
    else if(nrow(object)==1)
    {
        newcolnames <- object@dimnames[[2]][pSet]
        object@dimnames[[2]] <- newcolnames
    }

    return(object)
}
