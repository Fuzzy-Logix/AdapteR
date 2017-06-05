#' @include FLMatrix.R
NULL

#' Extract part of FLMatrix object.
#'
#' \code{[]} acts on FLMatrix objects and extracts parts of them.
#'
#'
#' @param object is a FLMatrix object
#' @param rows is a vector input corresponding to rows to be extracted
#' @param cols is a vector input corresponding to columns to be extracted
#' @param drop logical if dimnames to be dropped
#' @return \code{[]} returns FLMatrix object after extraction
#' which replicates the equivalent R extraction.
#' @section Constraints:
#' Applying UDT functions on subsetted matrices with discontinuous row and col ids' 
#' may result in error
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 2,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLmatrix <- flmatrix[1,]
#' @export
`[.FLMatrix`<-function(object,rows=1,cols=1, drop=TRUE)
{
    ##browser()
	connection<-getFLConnection(object)


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
                                 object@Dimnames[[2]]),
                 conditionDims=c(TRUE,FALSE)))
    }
    else { ## !missing(cols)
        if(missing(rows)) {
            if(length(unique(cols))!=length(cols))
                stop("Duplicate use of indices not supported so far")
            return(restrictFLMatrix(
                object = object,
                whereconditions = object@select@whereconditions,
                dimnames = list(object@Dimnames[[1]],
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
#' @param drop logical if dimnames to be dropped
#' @return \code{[]} returns FLMatrix object after extraction
#' which replicates the equivalent R extraction.
#' @examples
#' fltable <- FLTable("tblAbaloneWide", "ObsID")
#' resultFLtable <- fltable[1:10,4:6]
#' @export
`[.FLTable`<-function(object,rows=1,cols=1,drop=TRUE)
{
    vtype <- typeof(object)
    if(class(object@select)=="FLTableFunctionQuery")
      object <- store(object)
	  connection<-getFLConnection(object)

    object <- populateDimnames(object)
    if(is.FLVector(rows)) rows <- as.vector(rows)
    if(is.FLVector(cols)) cols <- as.vector(cols)
    ##@phani: todo: implement subsetting
    ## based on in-database indices as arguments
    if(is.FLMatrix(rows) || is.FLMatrix(cols)){
        return(subsetFLIndices(object,rows,cols))
    }
    if(is.numeric(rows))
        newrownames <- object@Dimnames[[1]][rows]
    else
        newrownames <- rows

    if(is.numeric(cols))
        newcolnames <- object@Dimnames[[2]][cols]
    else
        newcolnames <- cols

    if(!is.null(newrownames))
        if(any(is.na(newrownames)))
            stop("index out of bounds")
    if(!is.null(newcolnames))
        if(any(is.na(newcolnames)))
            stop("index out of bounds")
    ##browser()
    if(missing(cols))
    {
        if (!missing(rows)) {
            if(!setequal(object@Dimnames[[1]],
                         newrownames))
                object@select@whereconditions <- c(object@select@whereconditions,
                                            inCondition(paste0(getVariables(object)$obs_id_colname),
                                                        newrownames))
            object@Dimnames <- list(newrownames,
                                   object@Dimnames[[2]])
            object@dims[[1]] <- length(newrownames)
        }
    } else if(missing(rows)) { ## !missing(cols)
        ifelse(any(is.na(as.numeric(object@Dimnames[[1]]))),
               newrownames <- sort(object@Dimnames[[1]]),
               newrownames <- sort(as.numeric(object@Dimnames[[1]])))
        object@Dimnames <- list(newrownames,
                                newcolnames)
        object@dims[[2]] <- length(newcolnames)
        if(isDeep(object)){
            object@select@whereconditions <-
                c(object@select@whereconditions,
                  inCondition(paste0(getVariables(object)$var_id_colname),
                              object@Dimnames[[2]]))
        }
    } else {  ## !missing(cols) and !missing(rows)
        ##browser()
        if(!setequal(object@Dimnames[[1]], newrownames))
            object@select@whereconditions <-
            c(object@select@whereconditions,
              inCondition(paste0(getVariables(object)$obs_id_colname),
                          newrownames))
        if(isDeep(object) & !setequal(object@Dimnames[[2]], newcolnames)){
            object@select@whereconditions <-
                c(object@select@whereconditions,
                  inCondition(paste0(getVariables(object)$var_id_colname),
                              newcolnames))
        }
        object@Dimnames = list(newrownames, newcolnames)
        object@dims[[1]] <- length(newrownames)
        object@dims[[2]] <- length(newcolnames)
    }
    if(drop & (ncol(object)==1 | nrow(object) == 1))
    {
      vcolnames <- object@Dimnames[[2]]
      vrownames <- object@Dimnames[[1]]
      newnames <- NULL
      if(ncol(object)==1 && 
        (!all(vrownames==(1:length(vrownames)))))
      {
        MID <- getMaxValue(vtable=getOption("NameMapTableFL"),
                vcolName="MATRIX_ID",
                vconnection=connection)+1
        newrownames <- storeVarnameMapping(connection=getFLConnection(),
                        tablename=getTableNameSlot(object),
                        matrixId=MID,
                        dimId= 1,
                        mynames=vrownames
                        )
        newrownames <- names(newrownames)
        newcolnames <- vcolnames
        newnames <- newrownames
        vobsidcolumn <- "obs_id_colname"
      }
      else if(isDeep(object) && nrow(object)==1 &&
        (!all(vcolnames==(1:ncol(object)))))
      {
        MID <- getMaxValue(vtable=getOption("NameMapTableFL"),
                vcolName="MATRIX_ID",
                vconnection=connection)+1
        newcolnames <- storeVarnameMapping(connection=getFLConnection(),
                        tablename=getTableNameSlot(object),
                        matrixId=MID,
                        dimId= 1,
                        mynames=vcolnames
                        )
        newcolnames <- names(newcolnames)
        newrownames <- vrownames
        newnames <- newcolnames
        vobsidcolumn <- "var_id_colname"
      }
      if(!is.null(newnames))
      {
        if(!isAliasSet(object))
        object <- setAlias(object,"flt")
        mapselect <- new("FLSelectFrom",
                         connectionName = getFLConnectionName(), 
                         table_name = c(nameflt=getOption("NameMapTableFL")),
                         variables = list(),
                         whereconditions=c(paste0("nameflt.MATRIX_ID=",MID),
                                           paste0("nameflt.DIM_ID=1"),
                                           paste0("nameflt.NAME = ",castToVARCHAR(object,vobsidcolumn))),
                         order = "")
        object@select@variables[[vobsidcolumn]] <- "nameflt.Num_ID"


        # vtableref <- paste0(getOption("ResultDatabaseFL"),".",
        #               getOption("NameMapTableFL"))
        # select <- new(
        #     "FLSelectFrom",
        #     connection = connection, 
        #     database = getOption("ResultDatabaseFL"), 
        #     table_name = c(nameflta=getOption("NameMapTableFL")),
        #     variables = list(
        #             numIdColname = "Num_ID",
        #             nameColname = "NAME"),
        #     whereconditions=c(paste0(vtableref,".MATRIX_ID=",MID),
        #           paste0(vtableref,".DIM_ID=1")),
        #     order = "")
        
        vres <- newFLVector(
                  select=object@select,
                  Dimnames=list(newrownames,newcolnames),
                  isDeep=isDeep(object),
                  mapSelect=mapselect)
      }
      else
          vres <- newFLVector(
                      select=object@select,
                      Dimnames=list(vrownames,vcolnames),
                      isDeep=isDeep(object))
      vvaluecolumn <- getValueColumn(vres)
      vvaluecolumn <- changeAlias(vvaluecolumn,"","")
      vtype1 <- vtype[vvaluecolumn]
      if(is.null(vtype1))
        vtype1 <- vtype[1]
      names(vtype1) <- NULL
      vres@type <- vtype1
      return(vres)
    }
    else{
      vvaluecolumn <- getValueColumn(object)
      vvaluecolumn <- changeAlias(vvaluecolumn,"","")
      vtype1 <- vtype[vvaluecolumn]
      if(is.null(vtype1))
        vtype1 <- vtype[1]
      object@type <- vtype1
      return(object)
    }
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
#' WideTable <- FLTable("tblAbaloneWide","ObsID")
#' flvector <- FLVector[,"Diameter"]
#' resultFLVector <- flvector[10:1]
#' @export
`[.FLVector` <- function(object,pSet=1:length(object))
{
  ##browser()
  if(FLNamesMappedP(object) 
      || class(object@select)=="FLTableFunctionQuery") 
    object <- store(object)
  if(!isAliasSet(object))
  object <- setAlias(object,"flt")

  vobsidcolumn <- getObsIdColname(object)
  vvaluecolumn <- getValueColumn(object)
  newrownames <- rownames(object)
  newcolnames <- colnames(object)
  namesvector <- names(object)

  if(is.FLVector(pSet) 
    && (is.RowFLVector(pSet) || is.RowFLVector(object)))
  pSet <- as.vector(pSet)

    if(is.FLVector(pSet)){
        ## No index-out-of-bounds check.
        namesvector <- names(object)
        if(!is.null(namesvector)){
        ## Check if vector is named and add join for names.
        ## For named vector,integer subsetting fails.
          if(!is.FLVector(namesvector))
          namesvector <- as.FLVector(namesvector)

          vsqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,\n",
                              "c.vectorIndexColumn AS vectorIndexColumn,\n",
                              "a.vectorValueColumn AS vectorValueColumn \n",
                            " FROM(",constructSelect(object),") AS a,\n",
                              "(",constructSelect(namesvector),") AS b,\n",
                              "(",constructSelect(pSet),") AS c \n",
                            " WHERE a.vectorIndexColumn = b.vectorIndexColumn \n",
                            " AND c.vectorValueColumn = b.vectorValueColumn \n")
          tblfunqueryobj <- new("FLTableFunctionQuery",
                        connectionName = attr(connection,"name"),
                        variables = list(
                      obs_id_colname = "vectorIndexColumn",
                      cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=vsqlstr)
          object@select <- tblfunqueryobj
          mapselect <- NULL
          newrownames <- as.vector(pSet)
          newcolnames <- "vectorValueColumn"
        }
        else{
            if(class(pSet@select)=="FLTableFunctionQuery"
              || FLNamesMappedP(pSet)) pSet <- store(pSet)
            oldalias <- ifelse(length(names(getTableNameSlot(pSet))>0),
                              paste0(names(getTableNameSlot(pSet))),
                              "")
            names(pSet@select@table_name) <- "nameflt"
            nameValueColumn <- getVariables(pSet)[["cell_val_colname"]]
            if(!isDeep(pSet)) nameValueColumn <- colnames(pSet)[1]
            nameIndexColumn <- getVariables(pSet)[["obs_id_colname"]]
            nameValueColumn <- changeAlias(nameValueColumn,"nameflt",oldalias)
            nameIndexColumn <- changeAlias(nameIndexColumn,"nameflt",oldalias)
            mapselect <- new("FLSelectFrom",
                             connectionName = getFLConnectionName(), 
                             table_name = getTableNameSlot(pSet),
                             variables = list(),
                             whereconditions=c(constraintsSQL(pSet),
                                               paste0(nameValueColumn," = ",
                                                      castToVARCHAR(object,vobsidcolumn))),
                             order = "")
            object@select@variables[[vobsidcolumn]]<- nameIndexColumn
            newrownames <- rownames(pSet)
            newcolnames <- changeAlias(getValueColumn(object),"","")
        }
    }
    else{
        pSet <- as.vector(pSet)
      if((is.numeric(pSet) && (any(pSet>length(object))
          || any(pSet<=0)))) stop("index out of bounds")
      
      if(is.RowFLVector(object))
      {
        if(is.numeric(pSet))
        pSet <- object@Dimnames[[2]][pSet]
        if(!all(pSet %in% colnames(object)))
        stop("index out of bounds")
        object@Dimnames[[2]] <- pSet

        if(length(pSet)==1 && object@Dimnames[[1]]!=1)
        {
          MID <- getMaxValue(vtable=getOption("NameMapTableFL"),
              vcolName="MATRIX_ID",
              vconnection=connection)+1

          newnames <- storeVarnameMapping(connection=getFLConnection(),
                        tablename=getOption("ResultVectorTableFL"),
                        matrixId=MID,
                        dimId= 1,
                        mynames=object@Dimnames[[1]]
                        )
          mapselect <- new("FLSelectFrom",
                           connectionName = getFLConnectionName(), 
                           table_name = c(nameflt=getOption("NameMapTableFL")),
                           variables = list(),
                           whereconditions=c(paste0("nameflt.MATRIX_ID=",MID),
                                             paste0("nameflt.DIM_ID=1"),
                                             paste0("nameflt.NAME = ",
                                                castToVARCHAR(object,vobsidcolumn))),
                           order = "")
          object@Dimnames[[1]] <- 1
          object@mapSelect <- mapselect
          object@select@variables[[vobsidcolumn]] <- "nameflt.Num_ID"
        }
        return(object)
      }
        if(!is.null(namesvector)){
            if(is.numeric(pSet) && 
               !all(pSet %in% base::charmatch(namesvector,base::unique(namesvector)))) 
                stop("index out of bounds or duplicates in names of vector")
            if(is.character(pSet) && !all(pSet %in% namesvector))
                stop("index out of bounds")
            if(is.character(pSet) && 
               base::identical(as.character(namesvector),as.character(1:length(object))))
                stop("vector names not assigned or same as indices")
        }
      charpSet <- pSet
      if(is.character(pSet) ||
        !base::identical(as.character(namesvector),as.character(1:length(object))))
      {
        if(is.character(pSet))
        {
          charpSet <- pSet
          pSet <- base::charmatch(pSet,base::unique(namesvector))
        }
        else if(is.numeric(pSet) && is.character(namesvector))
        charpSet <- namesvector[pSet]
        namesvector <- base::charmatch(namesvector,base::unique(namesvector))
      }

      ## If whole vector is subsetted,return object.
      options(warn=-1)
      if(base::identical(as.character(pSet),as.character(namesvector))) return(object)
      options(warn=0)
      
      newpSet <- base::charmatch(pSet,base::unique(namesvector))
      pSet <- as.numeric(pSet)
      

       ## Monotonously increasing/decreasing,use direct subsetting.
      vflag1 <- NULL
      if(length(pSet)==length(base::unique(pSet))){
        if(all(diff(pSet)>0))
        vflag1 <- ""
        if(all(diff(pSet)<0))
        vflag1 <- "DESC"
      }
      
      if(!is.null(vflag1)){
        vmin <- base::min(pSet)
        vmax <- base::max(pSet)
        if(all(diff(pSet)==1)||all(diff(pSet)==-1))
        vwhereconditions <- c(paste0(getVariables(object)[[vobsidcolumn]]," >= ",vmin),
                paste0(getVariables(object)[[vobsidcolumn]]," <= ",vmax))
        else
        vwhereconditions <- inCondition(getVariables(object)[[vobsidcolumn]],
                                        pSet)
        object@select@whereconditions <- c(object@select@whereconditions,
                                            vwhereconditions)
        vsqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,\n",
                            "ROW_NUMBER()OVER(ORDER BY ",
                                getVariables(object)[[vobsidcolumn]],
                                " ",vflag1,") AS vectorIndexColumn,\n",
                            vvaluecolumn," AS vectorValueColumn\n",
                          " FROM ",tableAndAlias(object),
                          constructWhere(constraintsSQL(object)))
        tblfunqueryobj <- new("FLTableFunctionQuery",
                          connectionName = attr(connection,"name"),
                          variables = list(
                        obs_id_colname = "vectorIndexColumn",
                        cell_val_colname = "vectorValueColumn"),
                          whereconditions="",
                          order = "",
                          SQLquery=vsqlstr)
        object@select <- tblfunqueryobj
        mapselect <- NULL
      }
      else{
        MID <- getMaxValue(vtable=getOption("NameMapTableFL"),
              vcolName="MATRIX_ID",
              vconnection=connection)+1

        newnames <- storeVarnameMapping(connection=getFLConnection(),
                        tablename=getOption("ResultVectorTableFL"),
                        matrixId=MID,
                        dimId= 1,
                        mynames=newpSet
                        )

        mapselect <- new("FLSelectFrom",
                         connectionName = getFLConnectionName(), 
                         table_name = c(nameflt=getOption("NameMapTableFL")),
                         variables = list(),
                         whereconditions=c(paste0("nameflt.MATRIX_ID=",MID),
                                           paste0("nameflt.DIM_ID=1"),
                                           paste0("nameflt.NAME = ",
                                            castToVARCHAR(object,vobsidcolumn))),
                         order = "")
        object@select@variables[[vobsidcolumn]] <- "nameflt.Num_ID"
      }

      if(is.character(charpSet)) newnames <- charpSet
      else newnames <- 1:length(charpSet)

      if(ncol(object)==1) newrownames <- newnames
      else newcolnames <- newnames
  }
    select <- object@select

    if(!is.null(mapselect))
    return(newFLVector(
                select=select,
                Dimnames=list(newrownames,newcolnames),
                isDeep=isDeep(object),
                mapSelect=mapselect,
                type=typeof(object)))
    else return(newFLVector(
                select=select,
                Dimnames=list(newrownames,newcolnames),
                isDeep=isDeep(object),
                type=typeof(object)))
}

appendTableName <- function(object,tablename){
  if(length(tablename)>0 && 
      tablename!="" )
  return(sapply(object,function(x){
    if(!grepl(paste0(tablename,"."),x))
    paste0(tablename,".",x)
    else x
    }))
  else object
}

#' @export
changeAlias <- function(object,newalias,oldalias){
  object <- object[object!=""]
  vnames <- names(object)
  if(!length(object)>0) return(object)
  if(length(newalias)>0 && newalias!="") 
  newalias <- paste0(newalias,".")
  else newalias <- ""
  for(i in oldalias){
    if(any(grepl(i,object))){
      if(i=="") i<- "[^ ]*"
      result <- gsub(paste0(i,"\\."),
                    newalias,
                    object)
      break
    }
    else result <- object
  }
  result <- as.vector(sapply(result,function(x){
    if(!grepl(newalias,x,fixed=TRUE))
      paste0(newalias,x)
    else x
    }))
  names(result) <- vnames
  return(result)
}

#' @export
setAlias <- function(object,newalias){
  if(isAliasSet(object))
  oldalias <- names(getTableNameSlot(object))
  else oldalias <- ""
  if(newalias=="" ||is.null(newalias))
  newalias <- NULL
  names(object@select@table_name) <- newalias
  variables <- lapply(getVariables(object),
              function(x)changeAlias(x,newalias,oldalias))
  object@select@variables <- variables
  object@select@whereconditions <- changeAlias(
                              object@select@whereconditions,
                              newalias,oldalias)
  object@select@whereconditions <- constraintsSQL(object)
  return(object)
}

isAliasSet <- function(object){
  if(length(names(getTableNameSlot(object)))>0)
  return(TRUE) else return(FALSE)
}



# flSimple <- FLSerial(1,5)
# subset <- as.FL(3:2)
# flv <- flSimple[subset]
# flv1 <- as.FL(1)
# flv[flv1]
# names(flSimple) <- letters[1:5]
# subset <- as.FL(letters[3:2])
# flv <- flSimple[subset]
# flv[flv1]


## todo: optimize for R index vector
#' @export
`[.FLSimpleVector` <- function(object,pSet=1:length(object))
{
    ## browser()
    if(!is.FLSimpleVector(pSet)){
        if(!is.vector(pSet) && !is.FLVector(pSet)) 
            stop("pSet must be vector, FLVector or FLSimpleVector \n ")
        warning("subset must be FLSimpleVector for efficient subsetting \n ")
        if(missing(pSet))
            pSet <- FLSerial(1,length(object))
        else{
          if(is.vector(pSet)) pSet <- as.FLVector(pSet)
            pSet <- as.FLSimpleVector(pSet)
        }
    }
    obj <- object
    if(!isAliasSet(obj))
        obj <- setAlias(obj,genRandVarName())
    if(!isAliasSet(pSet)){
        pSet <- setAlias(pSet,genRandVarName())
        pSet1 <- setAlias(pSet,genRandVarName())
    }
    
    newnames <- names(object) 
    if(!is.null(names(obj)) &&
        !getTypeSlot(pSet)=="character")
        obj <- setNamesSlot(obj,names(obj)[pSet1])
    if(getTypeSlot(pSet)=="character"){
        if(is.null(names(obj)))
            stop("vector not named.Subscript out of bounds. \n ")
        if(!isAliasSet(vnames))
            vnames <- setAlias(vnames,genRandVarName())
        obj <- setNamesSlot(obj,pSet1)
        pSet <-  new("FLSimpleVector",
                            select=new("FLSelectFrom",
                                       table_name=rmDupTables(c(getTableNameSlot(vnames),
                                                                getTableNameSlot(pSet))),
                                       connectionName=getFLConnectionName(),
                                       variables=list(indexCol=getIndexSQLExpression(pSet),
                                                      valueCol=getIndexSQLExpression(vnames)),
                                       whereconditions=c(getWhereConditionsSlot(vnames),
                                                        getWhereConditionsSlot(pSet),
                                                        paste0(getValueSQLExpression(pSet),"=",
                                                               getValueSQLExpression(vnames))),
                                       order="indexCol"),
                            dimColumns = c("indexCol","valueCol"),
                            Dimnames=list(NULL),
                            dims    = getDimsSlot(pSet),
                            type       = "integer"
                            )
        newnames <- newnames[newnames==as.vector(pSet)] ## todo:  optimize by not fetching!!
    } else{
        if(!is.null(newnames))
            newnames <- newnames[as.vector(pSet)] ## todo:  optimize by not fetching!!
    }
    obj <-  new("FLSimpleVector",
                select=new("FLSelectFrom",
                           table_name=rmDupTables(c(getTableNameSlot(obj),
                                                    getTableNameSlot(pSet))),
                           connectionName=getFLConnectionName(),
                           variables=list(indexCol=getIndexSQLExpression(pSet),
                                          valueCol=getValueSQLExpression(obj)),
                           whereconditions=c(getWhereConditionsSlot(obj),
                                            getWhereConditionsSlot(pSet),
                                            paste0(getValueSQLExpression(pSet),"=",
                                                   getIndexSQLExpression(obj))),
                           order="indexCol"),
                dimColumns = c("indexCol","valueCol"),
                Dimnames=list(newnames),
                dims    = getDimsSlot(pSet),
                type       = getTypeSlot(obj)
                )
    return(obj)
}

rmDupTables <- function(pTableNames){
    vTableNames <- paste0(names(pTableNames),"___",pTableNames)
    return(pTableNames[gsub("___.*$","",vTableNames)])
}
#' Assign names to FLSimpleVectors
#' 
#' @param x FLSimpleVector
#' @param value a vector of length same as
#' FLSimpleVector
#' @return named FLSimpleVector
#' @examples
#' flv <- FLSerial(1,5)
#' names(flv) <- letters[1:5]
#' print(flv)
#' @export
`names<-.FLSimpleVector` <- function(x,value)
{
    dimnames(x) <- list(value)
    x
}

#' Get names of a FLSimpleVector
#'
#' @param x FLSimpleVector
#' @return character vector of names
#' of FLSimpleVector if exists. Else NULL
#' @export
names.FLSimpleVector <- function(x)
{
    dimnames(x)[[1]]
}

#' @export
setGeneric("as.FLSimpleVector", function(pObject,...) {
    standardGeneric("as.FLSimpleVector")
})
setMethod("as.FLSimpleVector", signature(pObject = "FLVector"),
          function(pObject,...)
            as.FLSimpleVector.FLVector(pObject=pObject,...))

as.FLSimpleVector.FLVector <- function(pObject,...){
    if(is.QueryVector(pObject) 
        || is.RowFLVector(pObject)
        || !is.wideFLTable(pObject))
        stop("conversion not supported \n ")
    pObject <- setAlias(pObject,"")
    pObject@select@variables <- list("idColumn"=getVariables(pObject)[["obs_id_colname"]],
                                    "valColumn"=colnames(pObject)[1])
    pObject@select@order <- "idColumn"
    return(new("FLSimpleVector",
                select= getSelectSlot(pObject),
                dimColumns = c("idColumn","valColumn"),
                Dimnames=list(names(pObject)),
                dims    = length(pObject),
                type       = pObject@type
                ))
}


#' Creates FLVector from a column from a database table after applying WHERE conditions.
#'
#' @export
#' @param data FLTable to select from
#' @param column Column to select
#' @param where additional where conditions for restriction
FLVectorForColumnWhere <- function(data,column,where){
    vwideTable <- getTableNameSlot(data)
    sqlstr <- paste0("
SELECT '%insertIDhere%' AS vectorIdColumn,
       ROW_NUMBER() OVER(ORDER BY ",getIndexSQLExpression(data,1),") AS vectorIndexColumn,
       flt.",column," AS vectorValueColumn 
FROM ",vwideTable," flt",
constructWhere(c(where(data),where))
)
    tblfunqueryobj <- new("FLTableFunctionQuery",
                          connectionName = attr(connection,"name"),
                          variables=list(
                              vectorIndexColumn="vectorIndexColumn",
                              vectorValueColumn="vectorValueColumn"),
                          whereconditions="",
                          order = "",
                          SQLquery=sqlstr)
    ## sqlQuery(connection,sqlstr)
    a <- newFLVector(
        select = tblfunqueryobj,
        Dimnames = list(NULL,"vectorValueColumn"),
        isDeep=FALSE,
        type="double")
    a@dims <- c(sqlQuery(connection,paste0("
SELECT COUNT(",getIndexSQLExpression(data,1),") AS n
FROM ",vwideTable," flt",
constructWhere(c(where(data),where))
))[1,1],1)
    a
}


castToVARCHAR <- function(pObject,pObsIdColumn){
    if(is.TDAster()){
        return(paste0(" CAST(",getVariables(pObject)[[pObsIdColumn]],
                    " AS VARCHAR(255)) "))
    }
    else return(paste0(" CAST(",getVariables(pObject)[[pObsIdColumn]],
                    " AS FLOAT) "))
}

