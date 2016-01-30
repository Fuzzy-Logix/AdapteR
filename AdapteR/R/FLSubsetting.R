
#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
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
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' resultFLmatrix <- flmatrix[1,]
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
    if(is.numeric(rows))
    ##     newrownames <- match(object@dimnames[[1]][rows],object@dimnames[[1]])
    ## else
        newrownames <- object@dimnames[[1]][rows] ## gk: reverted to my version
    else
        newrownames <- rows
    if(any(is.na(newrownames)))
        stop("subscript_out_of_bounds")

    ##browser()
    if(is.numeric(cols))
    ##     newcolnames <- match(object@dimnames[[2]][cols],object@dimnames[[2]])
    ## else
        newcolnames <- object@dimnames[[2]][cols] ## gk: reverted to my version
    else
        newcolnames <- cols
    if(any(is.na(newcolnames)))
        stop("subscript_out_of_bounds")

    if(missing(cols)) 
    {
        if (missing(rows)) return(object)
        else return(restrictFLMatrix(
                 object = object,
                 whereconditions = object@select@whereconditions,
                 dimnames = list(newrownames,
                                 object@dimnames[[2]]),
                 conditionDims=c(TRUE,FALSE)))
    }
    else { ## !missing(cols)
        if(missing(rows)) {
            return(restrictFLMatrix(
                object = object,
                whereconditions = object@select@whereconditions,
                dimnames = list(object@dimnames[[1]],
                                newcolnames),
                conditionDims=c(FALSE,TRUE)))
        } else {  ## !missing(cols) and !missing(rows)
            return(restrictFLMatrix(
                object = object,
                whereconditions = object@select@whereconditions,
                dimnames = list(newrownames,
                                newcolnames),
                conditionDims=c(TRUE,TRUE)))
        }
    }
}

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
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' resultFLmatrix <- flmatrix[1,]
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
                                            inCondition(paste0(object@select@db_name,".",
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
                  inCondition(paste0(object@select@select@db_name,".",
                                     object@select@table_name,".",
                                     getVariables(object)$var_id_colname),
                              object@dimnames[[2]]))
        } 
    } else {  ## !missing(cols) and !missing(rows)
        ##browser()
        if(!setequal(object@dimnames[[1]], newrownames))
            object@select@whereconditions <-
            c(object@select@whereconditions,
              inCondition(paste0(object@select@db_name,".",
                                 object@select@table_name,".",
                                 getVariables(object)$obs_id_colname),
                          newrownames))
        if(object@isDeep & !setequal(object@dimnames[[2]], newcolnames)){
            object@select@whereconditions <-
                c(object@select@whereconditions,
                  inCondition(paste0(object@select@db_name,".",
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
#' @param pObj is a FLVector object
#' @param pSet is a vector representing the indices of elements to extract
#' @return \code{[]} returns FLVector object after extraction
#' which replicates the equivalent R extraction.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' WideTable <- FLTable(connection, "FL_TRAIN", "tblVectorWide","vector_key")
#' flvectorWide <- FLVector(WideTable,"vector_value")
#' resultFLVector <- flvectorWide[1:2]
#' DeepTable <- FLTable(connection, "FL_TRAIN", "tblVectorDeep","vector_id","vector_key","vector_value")
#' flvectorDeep <- FLVector(DeepTable,"vector_value",1)
#' resultFLVector <- flvectorDeep[1:2]
#' @export

`[.FLVector` <- function(object,pSet=1:length(object))
{
    if(any(pSet>length(object))) stop("index out of bounds")
    if(ncol(object)==1)
    {
        newrownames <- object@dimnames[[1]][pSet]
        if(!setequal(object@dimnames[[1]], newrownames))
            object@select@whereconditions <-
            c(object@select@whereconditions,
              inCondition(paste0(object@select@db_name,".",
                                 object@select@table_name,".vectorIndexColumn"),
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
                                        #     pObj[pSet,]
                                        # 	if(pObj@isDeep)
                                        # 	{
                                        # 		vTemp <- character(0)
                                        # 		if(length(pSet)>1)
                                        # 		{
                                        #             vTemp <- rep(paste0(",",pSet[2:length(pSet)]),length.out=length(pSet)-1)
                                        #             vTemp<-paste(vTemp,collapse=" ")
                                        # 		}

                                        # 		sqlstr <- paste0("SELECT * FROM ",remoteTable(pObj), 
                                        #                          " WHERE ",pObj@obs_id_colname,"=",pObj@vector_id_value," AND ",pObj@var_id_name," IN(",pSet[1],vTemp,")")

                                        # 		vRetObj<-sqlQuery(getConnection(pObj),sqlstr)
                                        # 		vResultVec <- c()

                                        # 		for(vIter in pSet)
                                        #             vResultVec <- append(vResultVec,vRetObj[(vRetObj[,pObj@var_id_name]==vIter),pObj@col_name])

                                        # 		return(as.FLVector(vResultVec,getConnection(pObj)))
                                        # 	}

                                        # 	if(!pObj@isDeep)
                                        # 	{
                                        # 		vTemp <- character(0)
                                        # 		if(length(pSet)>1)
                                        # 		{
                                        #             vTemp <- rep(paste0(",",pSet[2:length(pSet)]),length.out=length(pSet)-1)
                                        #             vTemp<-paste(vTemp,collapse=" ")
                                        # 		}

                                        # 		sqlstr <- paste0("SELECT * 
                                        # 						 FROM ",remoteTable(pObj), 
                                        #                          " WHERE ",pObj@obs_id_colname," IN(",pSet[1],vTemp,")")

                                        # 		vRetObj<-sqlQuery(getConnection(pObj),sqlstr)
                                        # 		vResultVec <- c()

                                        # 		for(vIter in pSet)
                                        #             vResultVec <- append(vResultVec,vRetObj[(vRetObj[,pObj@obs_id_colname]==vIter),pObj@col_name])

                                        # 		return(as.FLVector(vResultVec,getConnection(pObj)))
                                        # 	}
                                        # }
