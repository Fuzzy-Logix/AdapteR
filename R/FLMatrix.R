#' @include utilities.R
#' @include FLStore.R
#' @include FLTable.R
#' @include FLVector.R
NULL
setOldClass("RODBC")


## A table query models a select or a table result of a sql statement
#' @slot connection ODBC connectivity for R
setClass("FLTableQuery",
         slots=list(
             connection = "ANY",
             variables="list",
             whereconditions="character",
             order = "character"
         ))


##' A selectFrom models a select from a table
##'
##' @slot database character
##' @slot table_name character
setClass("FLSelectFrom",
         contains="FLTableQuery",
         slots=list(
             database = "character",
             table_name = "character"
         ))

##' A TableFunctionQuery models a select from an arbitrary query
##'
##' @slot SQLquery character
setClass("FLTableFunctionQuery",
         contains="FLTableQuery",
         slots=list(
             SQLquery = "character"
         ))

##' An S4 class to represent FLMatrix.
##' A Matrix can be either based off a query from a deep table (customizable by any where-condition)
##' -- or based off an arbitrary SQL statement returning a deep table.
##'
##' @slot dimnames list of 2 elements with row, column names of FLMatrix object
##' @slot dim list of 2 FLTableQuery instances (or NULL) that map row_ids in the select to row-names in R
setClass(
    "FLMatrix",
    slots = list(
        select = "FLTableQuery",
        mapSelect  = "FLSelectFrom",
        dim = "ANY",
        dimnames = "ANY"
    )
)

#' An S4 class to represent FLTable
#'
#' @slot odbc_connection ODBC connectivity for R
#' @slot db_name character
#' @slot table_name character
#' @slot obs_id_colname character
#' @slot var_id_colnames character 
#' @slot cell_val_colname character
#' @slot isDeep logical
#' @method names FLTable
#' @param object retrieves the column names of FLTable object
setClass(
    "FLTable",
    slots = list(
        select = "FLTableQuery",
        dimnames = "list",
        isDeep = "logical"
    )
)

#' An S4 class to represent FLVector
#'
setClass(
    "FLVector",
    slots = list(
      select = "FLTableQuery",
      dimnames        = "list",
      isDeep= "logical"
    ))


##' stores a matrix in a table.
##' TODO:  define when data is stored (automatic caching, user requests...)
##'
##' @param object
##' @return A FLMatrix based on a stored table of the executed
##' @author  Gregor Kappler <g.kappler@@gmx.net>
setGeneric("store", function(object,returnType,connection,...) {
    standardGeneric("store")
})

setMethod("store",
          signature(object = "FLMatrix",returnType="missing",connection="missing"),
          function(object) store.FLMatrix(object))
setMethod("store",
          signature(object = "FLVector",returnType="missing",connection="missing"),
          function(object) store.FLVector(object))
setMethod("store",
          signature(object = "FLTable",returnType="missing",connection="missing"),
          function(object) store.FLTable(object))
setMethod("store",
          signature(object = "character",returnType="character",connection="RODBC"),
          function(object,returnType,connection) store.character(object,returnType,connection))
setMethod("store",
          signature(object = "character",returnType="character",connection="JDBCConnection"),
          function(object,returnType,connection) store.character(object,returnType,connection))


drop.FLTable <- function(object)
{
    names(object@table_name) <- NULL
    vSqlStr <- paste0(" DROP TABLE ",remoteTable(object))
    sqlSendUpdate(getConnection(object),
                  vSqlStr)
    return(paste0(object@select@table_name," DROPPED"))
}



setGeneric("getVariables", function(object) {
    standardGeneric("getVariables")
})
setMethod("getVariables",
          signature(object = "FLTableQuery"),
          function(object) {
              variables <- object@variables
              if(length(variables)==0) return("*")
              if(is.null(names(variables)))
                  names(variables) <- variables
              else
                  names(variables)[is.na(names(variables))] <- variables[is.na(names(variables))]
              names(variables) <- gsub("\\*","",names(variables))
              return(variables)
          })
setMethod("getVariables",
          signature(object = "FLMatrix"),
          function(object) getVariables(object@select))
setMethod("getVariables",
          signature(object = "FLTable"),
          function(object) getVariables(object@select))
setMethod("getVariables",
          signature(object = "FLVector"),
          function(object) getVariables(object@select))



setGeneric("suffixAlias", function(object,suffix,...) {
    standardGeneric("suffixAlias")
})

setMethod("suffixAlias",
          signature(object = "FLMatrix",suffix="character"),
          function(object,suffix){
              object@select <- suffixAlias(object@select,suffix)
              object@mapSelect <- suffixAlias(object@mapSelect,suffix)
              return(object)
          })
setMethod("suffixAlias",
          signature(object = "FLSelectFrom",suffix="character"),
          function(object,suffix){
              for(talias in names(object@table_name)){
                  object@variables <- llply(
                      object@variables,
                      function(v) gsub(paste0("^",talias,"\\."),
                                       paste0(talias,suffix,"."),
                                       v))
                  object@whereconditions <- gsub(paste0("^",talias,"\\."),
                                       paste0(talias,suffix,"."),
                                       object@whereconditions)
              }
              if(!is.null(names(object@table_name)))
                  names(object@table_name) <- paste0(names(object@table_name),suffix)
              return(object)
          })

setGeneric("constructSelect", function(object,...) {
    standardGeneric("constructSelect")
})

setMethod("constructSelect",
          signature(object = "FLMatrix"),
          function(object,joinNames=TRUE){
              ##browser()
              if(!FLNamesMappedP(object) | !joinNames)
                  return(constructSelect(object@select))
              select <- object@select
              select@variables <- c(select@variables,
                                    object@mapSelect@variables)
              select@table_name <- c(select@table_name,
                                     object@mapSelect@table_name)
              select@whereconditions <- c(select@whereconditions,
                                          object@mapSelect@whereconditions)
              return(constructSelect(select))
          })

setMethod("constructSelect",
          signature(object = "FLTableQuery"),
          function(object) {
              if(is.null(object@database)) return(NULL)
              if(length(object@database)==0) return(NULL)
              return(paste0("SELECT ",
                            paste(colnames(object),collapse=", "),
                            " FROM ",remoteTable(object),
                            constructWhere(c(constraintsSQL(object))),
                            paste(object@order,collapse=", "),
                            "\n"))
          })


setMethod("constructSelect",
          signature(object = "FLTable"),
          function(object) {
            if(class(object@select)=="FLTableFunctionQuery") 
            return(constructSelect(object@select))
              if(!object@isDeep) 
              {
                variables <- getVariables(object)
                if(is.null(names(variables)))
                    names(variables) <- variables
                else
                    names(variables)[is.na(names(variables))] <- variables[is.na(names(variables))]

                ifelse(is.null(variables$obs_id_colname),vobsIDCol <- variables["vectorIndexColumn"],
                   vobsIDCol <- variables["obs_id_colname"])
                colnames <- colnames(object)
                #colnames <- colnames[colnames!=vobsIDCol]
                newColnames <- renameDuplicates(colnames)
                variables <- as.list(c(vobsIDCol[[1]],colnames))
                names(variables) <- c("obs_id_colname",
                                      newColnames)

                return(paste0(
                            "SELECT\n",
                            paste0("     ",
                                   variables," AS ",
                                   names(variables),
                                   collapse = ",\n"),
                            "\nFROM ",remoteTable(object),
                            constructWhere(c(constraintsSQL(object))),
                            "\n"))
              }
              else 
              {
                variables <- getVariables(object)
                if(is.null(names(variables)))
                  names(variables) <- variables
                else
                    names(variables)[is.na(names(variables))] <- variables[is.na(names(variables))]

                variables <- as.list(c(variables[["obs_id_colname"]],variables[["var_id_colname"]],variables[["cell_val_colname"]]))
                names(variables) <- c("obs_id_colname","var_id_colname","cell_val_colname")
                return(paste0(
                          "SELECT\n",
                          paste0("     ",
                                 variables," AS ",
                                 names(variables),
                                 collapse = ",\n"),
                          "\nFROM ",remoteTable(object),
                          constructWhere(c(constraintsSQL(object))),
                          "\n"))
              }
          })

setMethod("constructSelect", signature(object = "FLVector"),
          function(object) {
            if(class(object@select)=="FLTableFunctionQuery") 
            return(constructSelect(object@select))
              if(!object@isDeep) {
                newColnames <- renameDuplicates(colnames(object))
                variables <- getVariables(object)
                if(is.null(names(variables)))
                    names(variables) <- variables
                else
                    names(variables)[is.na(names(variables))] <- variables[is.na(names(variables))]

                ifelse(is.null(variables$obs_id_colname),vobsIDCol <- variables["vectorIndexColumn"],
                   vobsIDCol <- variables["obs_id_colname"])
                variables <- as.list(c("'%insertIDhere%'",vobsIDCol[[1]],colnames(object)))
                names(variables) <- c("vectorIdColumn",
                                      "vectorIndexColumn",
                                      if(length(colnames(object))>1) newColnames else "vectorValueColumn")

                return(paste0(
                            "SELECT\n",
                            paste0("     ",
                                   variables," AS ",
                                   names(variables),
                                   collapse = ",\n"),
                            "\nFROM ",remoteTable(object),
                            constructWhere(c(constraintsSQL(object))),
                            "\n"))
              } else {
                  variables <- getVariables(object)
                  if(is.null(names(variables)))
                    names(variables) <- variables
                  else
                      names(variables)[is.na(names(variables))] <- variables[is.na(names(variables))]

                  if(nrow(object)==1)
                  vobsIDCol <- variables["var_id_colname"]
                  else vobsIDCol <- variables["obs_id_colname"]
                  variables <- as.list(c("'%insertIDhere%'",vobsIDCol[[1]],variables[["cell_val_colname"]]))
                  names(variables) <- c("vectorIdColumn","vectorIndexColumn","vectorValueColumn")
                  return(paste0(
                            "SELECT\n",
                            paste0("     ",
                                   variables," AS ",
                                   names(variables),
                                   collapse = ",\n"),
                            "\nFROM ",remoteTable(object),
                            constructWhere(c(constraintsSQL(object))),
                            "\n"))
              }
          })

renameDuplicates <- function(vec)
{
    for(x in vec)
    {
        id <- which(vec %in% x)
        if(length(id)>1)
            vec[id] <- paste0(vec[id],1:length(id))
    }
    return(vec)
}

setMethod(
    "constructSelect",
    signature(object = "FLSelectFrom"),
    function(object) {
        if(is.null(object@database)) return(NULL)
        if(length(object@database)==0) return(NULL)
        variables <- getVariables(object)
        order <- setdiff(object@order,c(NA,""))
        if(length(order)==0)
            ordering <- ""
        else
            ## ordering <- paste0(" ORDER BY ",paste0(object@obs_id_colname,collapse = ", "))
            ordering <- paste0("\nORDER BY ",
                               paste0(order,
                                      collapse = ", "))
        return(paste0(
            "SELECT\n",
            paste0("     ",
                   variables, " ",
                   names(variables),
                   collapse = ",\n"),
            "\nFROM ",remoteTable(object),
            constructWhere(c(constraintsSQL(object))),
            ordering,
            "\n"))
    })

setMethod("constructSelect",
          signature(object = "FLTableFunctionQuery"),
          function(object) return(object@SQLquery))

setMethod("constructSelect",
          signature(object = "numeric"),
          function(object) return(object))

setMethod("constructSelect",
          signature(object = "character"),
          function(object) return(object))

FLNamesMappedP <- function(m) return(0<length(m@mapSelect@table_name))



getConditionValues <- function(selection,names) FLNameMatch(selection,names)

FLName <- function(i,thenames){
    ##browser()
    if(is.null(i))
        return(thenames)
    if(is.null(thenames)){
        names(i) <- i
        return(i)
    }
    if(is.numeric(i)){
        RR <- thenames[i]
        if(is.null(names(RR)) & !is.null(names(thenames)))
            names(RR) <- names(thenames)[i]
        return(RR)
    }
    i <- thenames[match(i,thenames)]
    return(i)
}

FLNameMatch <- function(i,thenames){
    ##browser()
    if(is.null(thenames)){
        return(i)
    }
    if(is.numeric(i)){
        if(is.null(names(thenames))){
            return(thenames[i])
        } else {
            return(names(thenames)[i])
        } 
    }
    if(!is.null(names(thenames)))
        return(names(thenames)[match(i,thenames)])
    else
        return(i)
}

FLIndexOf <- function(i,thenames){
    if(is.null(thenames))
        return(i)
    if(!is.null(names(thenames))) ##FLindex?
        return(match(i,names(thenames)))
    else {
        if(is.numeric(i))
            return(i)
        else
            return(match(i,thenames))
    }
}

#' Appends where clauses for subsetting etc.
#'
#' @export
#' @param object An FLMatrix object
#' @param whereconditions constraints to be added
#' @param dimnames new dimension names
#' @param conditionDims vector of 2 LOGICAL values, if first is TRUE,
#' a inCondition for the rownames is appended, if 2 for the columns respectively.
restrictFLMatrix <-
    function(object,
             whereconditions = object@select@whereconditions,
             dimnames = object@dimnames,
             conditionDims = c(FALSE,FALSE)){
        ##browser()
        if(is.null(dimnames)) return(object)
        ## if there is a mapping table, use indices instead of dimnames
        vars <- c(object@select@variables$rowIdColumn,
                  object@select@variables$colIdColumn)
        for(i in 1:2)
            if(conditionDims[[i]])
                whereconditions <-
                c(whereconditions,
                  inCondition(
                      vars[[i]],
                      getConditionValues(
                          dimnames[[i]],
                          object@dimnames[[i]])))

        object@select@whereconditions <-
            unique(c(object@select@whereconditions,
                     whereconditions))
        for(i in 1:2)
            if(!is.null(dimnames[[i]])){
                object@dim[[i]] <- length(dimnames[[i]])
                object@dimnames[[i]] <- FLName(dimnames[[i]],
                                               object@dimnames[[i]])
            }

        object
    }

##' Amends dimension names to a matrix if a mapping exists.  
##' 
##' If no mapping exists uses the unique row and column names.
##' Dimnames 1:n are set to NULL in order to adhere to R conventions.
##'
##' @param flm FLMatrix 
##' @return the FLMatrix object, with slot dimnames re set 
##' @author  Gregor Kappler <g.kappler@@gmx.net>
FLamendDimnames <- function(flm,map_table) {
    ##browser()
    ##browser()

    checkNames <- function(colnames, addIndex=FALSE){
        if(is.numeric(colnames) && colnames==1:length(colnames))
            colnames <- c()
        if(length(colnames)==0) colnames <- NULL
        if(addIndex & !is.null(colnames) & is.null(names(colnames)))
            names(colnames) <- 1:length(colnames)
        return(colnames)
    }
    connection <- getConnection(flm)
    dimnames <- flm@dimnames
    ##print(dimnames)
    if(is.null(dimnames) & !is.null(map_table)){
        ## if there is currently no dimnames set,
        ## receive dimnames from map table
        ##browser()
        mapSelect <- flm@select
        mapSelect@variables <- list()
        mapSelect@table_name <- map_table
        names(mapSelect@table_name) <- names(flm@select@table_name) ## todo: generalize
        mapSelect@order <- c("DIM_ID","NUM_ID")
        mapSQL <- constructSelect(mapSelect)
        if(!is.null(mapSQL)){
            mapDF <- sqlQuery(connection, mapSQL)
            if(nrow(mapDF)>0){
                rownames <- mapDF[mapDF$DIM_ID==1,"NAME"]
                names(rownames) <- mapDF[mapDF$DIM_ID==1,"NUM_ID"]
                colnames <- mapDF[mapDF$DIM_ID==2,"NAME"]
                names(colnames) <- mapDF[mapDF$DIM_ID==2,"NUM_ID"]
            }
        } else
            rownames <- colnames <- c()
    } else {
        rownames <- dimnames[[1]]
        colnames <- dimnames[[2]]
    }
    
    ## if there is still no dimnames set,
    ## use unique values as dimnames
    selectUnique <- function(varname)
        paste0("SELECT unique(",
               flm@select@variables[[varname]],
               ") as V\n",
               "FROM  ",remoteTable(flm@select),
               constructWhere(
                   constraintsSQL(flm@select)),
               "\nORDER BY 1")
    if(length(rownames)==0)
        rownames <- sort(sqlQuery(connection,selectUnique("rowIdColumn"))$V)
    if(length(colnames)==0)
        colnames <- sort(sqlQuery(connection, selectUnique("colIdColumn"))$V)

    if(all(flm@dim==0))
        flm@dim <- c(length(rownames),length(colnames))
    
    dimnames <- flm@dimnames <- list(checkNames(rownames),
                                     checkNames(colnames))


    if(!is.null(map_table)){
        mConstraint <- c()
        tablenames <- c()
        variables <- list()
        names(map_table) <- NULL
        ## row name maps
        if(!is.null(dimnames[[1]])){
            tablenames <- c(tablenames,rnmap=map_table)
               variables$rowNameColumn <- "rnmap.NAME"
            mConstraint <-
                c(mConstraint,
                  gsub("mtrx","rnmap", flm@select@whereconditions),
                  equalityConstraint(
                      paste0(flm@select@variables$rowIdColumn),
                      "rnmap.NUM_ID"),
                  equalityConstraint("rnmap.DIM_ID","1"))
        }

        ## names(select@variables) <- gsub("IdColumn","NumIdColumn",
        ##                                 names(select@variables))
        ## names(select@variables) <- gsub("NameColumn","IdColumn",
        ##                                 names(select@variables))

        ## column name maps
        if(!is.null(dimnames[[2]])){
            tablenames <- c(tablenames,cnmap=map_table)
            variables$colNameColumn <- "cnmap.NAME"
            mConstraint <-
                c(mConstraint,
                  gsub("mtrx","cnmap", flm@select@whereconditions),
                  equalityConstraint(
                      paste0(flm@select@variables$colIdColumn),
                      "cnmap.NUM_ID"),
                  equalityConstraint("cmap.DIM_ID","2"))
        }
        flm@mapSelect <- new(
            "FLSelectFrom",
            connection = connection,
            database = database,
            table_name = tablenames,
            variables=variables,
            whereconditions=mConstraint,
            order = "")
    }

    return(flm)
}

#' Constructor function for FLMatrix.
#'
#' \code{FLMatrix} constructs an object of class \code{FLMatrix}.
#'
#' \code{FLMatrix} object is an in-database equivalent to matrix object.
#' This object is used as input for matrix operation functions.
#' @param connection ODBC connection handle as returned by \code{\link[RODBC]{odbcConnect}}
#' @param database name of the database
#' @param table_name name of the matrix table
#' @param matrix_id_value identifier for the input matrix
#' @param matrix_id_colname matrix id value in \code{table_name}
#' @param row_id_colname column name in \code{table_name} where row numbers are stored
#' @param col_id_colname column name in \code{table_name} where column numbers are stored
#' @param cell_val_colname column name in \code{table_name} where matrix elements are stored
#' @return \code{FLMatrix} returns an object of class FLMatrix mapped
#' to an in-database matrix.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' @export
FLMatrix <- function(connection,
                     database=getOption("ResultDatabaseFL"),
                     table_name,
                     matrix_id_value = "",
                     matrix_id_colname = "",
                     row_id_colname = "rowIdColumn",
                     col_id_colname = "colIdColumn",
                     cell_val_colname = "valueColumn",
                     dim=0,
                     dimnames = NULL,
                     conditionDims=c(FALSE,FALSE),
                     whereconditions=c(""),
                     map_table=NULL){
    mConstraint <-
        equalityConstraint(
            paste0("mtrx.",matrix_id_colname),
            matrix_id_value)
    names(table_name) <- NULL
    tablenames <- c(mtrx=table_name)
    variables <- list(
        rowIdColumn=paste0("mtrx.",row_id_colname),
        colIdColumn=paste0("mtrx.",col_id_colname),
        valueColumn=paste0("mtrx.",cell_val_colname))

    select <- new(
        "FLSelectFrom",
        connection = connection,
        database = database,
        table_name = tablenames,
        variables=variables,
        whereconditions=c(whereconditions, mConstraint),
        order = "")
    
    RESULT <- new("FLMatrix",
                  select = select,
                  dim = dim,
                  dimnames = dimnames)
    
    RESULT <- FLamendDimnames(RESULT,map_table)

    RESULT <- restrictFLMatrix(RESULT,
                               whereconditions,
                               dimnames,
                               conditionDims)

    if(""!=matrix_id_value && ""!=matrix_id_colname){
        select@variables$matrixId <- matrix_id_colname
    }
    return(RESULT)
}


constructWhere <- function(conditions) {
    if(!is.character(conditions))
        stop("Provide constraints as character vector")
    conditions <- setdiff(conditions,c(NA,""))
    if(length(conditions)>0)
        paste0("\nWHERE",paste0("   (",conditions,")",
                                collapse=" AND\n"))
    else
        ""
}


inCondition <- function(col,vals){
    if(length(vals)>0)
        paste0(col," IN (", paste0("'",vals,"'",collapse= ", "), ")")
    else
        ""
}

equalityConstraint <- function(tableColName,constantValue){
    if(""==constantValue)
        ""
    else
        paste0(tableColName, "=",constantValue)
}

## ##' Function to replace full table name with an alias.
## ##' todo: alias
## ##'
## ##' @param constraints 
## ##' @param oldName 
## ##' @param alias 
## ##' @return the constraints with aliases
## ##' @author  Gregor Kappler <g.kappler@@gmx.net>
## localizeConstraints <- function(constraints, oldName, alias=""){
##     ##browser()
##     if(alias!="")
##         gsub(oldName, alias, constraints)
##     else
##         gsub(paste0(oldName,"."), alias, constraints)
## }




## gk: todo:  I doubt that we need different connections for different options...
## we need to discuss moving the connection from an object slot to an option
setGeneric("getConnection", function(object) {
    standardGeneric("getConnection")
})
setMethod("getConnection", signature(object = "FLMatrix"),
          function(object) object@select@connection)
setMethod("getConnection", signature(object = "FLTable"),
          function(object) object@select@connection)
setMethod("getConnection", signature(object = "FLTableQuery"),
          function(object) object@select@connection)
setMethod("getConnection", signature(object = "FLVector"),
          function(object) object@select@connection)


setGeneric("constraintsSQL", function(object) {
    standardGeneric("constraintsSQL")
})
setMethod("constraintsSQL", signature(object = "FLMatrix"),
          function(object) {
              return(constraintsSQL(object@select))
          })
setMethod("constraintsSQL", signature(object = "FLTable"),
          function(object) {
              return(constraintsSQL(object@select))
          })
setMethod("constraintsSQL", signature(object = "FLVector"),
          function(object) {
              return(constraintsSQL(object@select))
          })
setMethod("constraintsSQL", signature(object = "FLSelectFrom"),
          function(object) {
              constraints <- object@whereconditions
              tablenames <- object@table_name
              if(!is.null(names(tablenames)))
                  for(ti in 1:length(tablenames)){
                      tname <- tablenames[ti]
                      talias <- names(tablenames)[ti]
                      constraints <- gsub(paste0("[^ ]*",tname,"\\."),
                                          paste0(" ",talias,"."),
                                          constraints)
                  }
              return(constraints)
          })


setGeneric("remoteTable", function(object, table) {
    standardGeneric("remoteTable")
})
setMethod("remoteTable", signature(object = "FLMatrix", table="missing"),
          function(object)
              remoteTable(object@select))
setMethod("remoteTable", signature(object = "FLTable", table="missing"),
          function(object)
              remoteTable(object@select))
setMethod("remoteTable", signature(object = "FLVector", table="missing"),
          function(object)
              remoteTable(object@select))
setMethod("remoteTable", signature(object = "character", table="character"),
          function(object,table)
              getRemoteTableName(object,table))
setMethod("remoteTable", signature(object = "FLSelectFrom", table="missing"),
          function(object){
              if(is.null(names(object@table_name)))
                  return(paste0(sapply(object@table_name,
                                       getRemoteTableName,
                                       database=object@database),
                                collapse=",\n    "))
              else
                  return(paste0(sapply(object@table_name,
                                       getRemoteTableName,
                                       database=object@database),
                                " AS ", names(object@table_name),
                                collapse=",\n    "))
          })


setGeneric("viewSelectMatrix", function(object,localName, withName) {
    standardGeneric("viewSelectMatrix")
})
setMethod("viewSelectMatrix", signature(object = "FLMatrix",
                                        localName="character",
                                        withName="missing"),
          function(object,localName, withName="z")
          {viewSelectMatrix(object,localName,withName="z")})
setMethod("viewSelectMatrix", signature(object = "FLMatrix",
                                        localName="character",
                                        withName="character"),
          function(object,localName, withName="z") {
              ##' todo: alias
              ##names(object@select@table_name)[[1]] <- localName
              object <- orderVariables(
                  updateVariable(object,"Matrix_ID",-1),
                  c("Matrix_ID","rowIdColumn","colIdColumn","valueColumn")
              )
              return(paste0(" WITH ",withName,
                            " (Matrix_ID, Row_ID, Col_ID, Cell_Val)
              AS (",constructSelect(object)," ) "))
          })

## outputSelectMatrix apples function given by func_name to view given by viewname
## and returns columns specified by outcolnames list. IncludeMID tells if max_matrix_id_value
## should be one of the columns returned.
setGeneric("outputSelectMatrix", function(func_name,includeMID,outColNames,
                                          viewName,localName,whereClause,
                                          vconnection) {
    standardGeneric("outputSelectMatrix")
})

setMethod("outputSelectMatrix", signature(func_name="character",includeMID="missing",outColNames="list",
                                          viewName="character",localName="character",whereClause="character",
                                          vconnection="missing"),
          function(func_name,includeMID,outColNames,viewName,localName,whereClause)
          {
              return(paste0(" SELECT ",paste0(localName,".",outColNames,collapse=","),paste0(" 
          FROM TABLE (",func_name,
          "(",viewName,".Matrix_ID, ",viewName,".Row_ID, ",viewName,".Col_ID, ",viewName,".Cell_Val)",
          " HASH BY z.Matrix_ID 
          LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID ",
          ") AS ",localName," ",whereClause)))
          })

setMethod("outputSelectMatrix", signature(func_name="character",includeMID="logical",outColNames="list",
                                          viewName="character",localName="character",whereClause="character",
                                          vconnection="RODBC"),
          function(func_name,includeMID,outColNames,viewName,localName,whereClause,vconnection)
          {
              return(paste0(" SELECT '%insertIDhere%' ",#ifelse(includeMID,getMaxMatrixId(vconnection),getMaxVectorId(vconnection)),
                            " AS OutputMatrixID ",
                            paste0(",",localName,".",outColNames,collapse=""),paste0(" 
          FROM TABLE (",func_name,
          "(",viewName,".Matrix_ID, ",viewName,".Row_ID, ",viewName,".Col_ID, ",viewName,".Cell_Val)",
          " HASH BY z.Matrix_ID 
          LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID ",
          ") AS ",localName," ",whereClause)))
          })

setMethod("outputSelectMatrix", signature(func_name="character",includeMID="logical",outColNames="list",
                                          viewName="character",localName="character",whereClause="character",
                                          vconnection="JDBCConnection"),
          function(func_name,includeMID,outColNames,viewName,localName,whereClause,vconnection)
          {
              return(paste0(" SELECT '%insertIDhere%' ",#ifelse(includeMID,getMaxMatrixId(vconnection),getMaxVectorId(vconnection)),
                            paste0(",",localName,".",outColNames,collapse=""),paste0(" 
          FROM TABLE (",func_name,
          "(",viewName,".Matrix_ID, ",viewName,".Row_ID, ",viewName,".Col_ID, ",viewName,".Cell_Val)",
          " HASH BY z.Matrix_ID 
          LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID ",
          ") AS ",localName," ",whereClause)))
          })

setMethod("outputSelectMatrix", signature(func_name="character",includeMID="missing",outColNames="missing",
                                          viewName="character",localName="character",whereClause="character",
                                          vconnection="missing"),
          function(func_name,includeMID,outColNames,viewName,localName,whereClause)
          {
              return(outputSelectMatrix(func_name,
                                        outColNames=list("OutputRowNum","OutputColNum","OutputVal"),viewName=viewName,
                                        localName=localName,whereClause=whereClause))
          })
setMethod("outputSelectMatrix", signature(func_name="character",includeMID="logical",outColNames="missing",
                                          viewName="character",localName="character",whereClause="character",
                                          vconnection="RODBC"),
          function(func_name,includeMID,
                   outColNames=list("OutputRowNum","OutputColNum","OutputVal"),
                   viewName,localName,whereClause,vconnection)
          {
              return(outputSelectMatrix(func_name,includeMID,outColNames=list("OutputRowNum","OutputColNum","OutputVal")
                                       ,viewName,localName,whereClause,vconnection))
          })
setMethod("outputSelectMatrix", signature(func_name="character",includeMID="logical",outColNames="missing",
                                          viewName="character",localName="character",whereClause="character",
                                          vconnection="JDBCConnection"),
          function(func_name,includeMID,
                   outColNames=list("OutputRowNum","OutputColNum","OutputVal"),
                   viewName,localName,whereClause,vconnection)
          {
              return(outputSelectMatrix(func_name,includeMID,outColNames=list("OutputRowNum","OutputColNum","OutputVal")
                                       ,viewName,localName,whereClause,vconnection))
          })

setMethod("outputSelectMatrix", signature(func_name="character",includeMID="logical",outColNames="list",
                                          viewName="character",localName="character",whereClause="missing",
                                          vconnection="RODBC"),
          function(func_name,includeMID,outColNames,viewName,localName,whereClause=";",vconnection)
          {
              return(outputSelectMatrix(func_name,includeMID,
                                        outColNames,viewName,localName,whereClause=";",vconnection))
          })
setMethod("outputSelectMatrix", signature(func_name="character",includeMID="logical",outColNames="list",
                                          viewName="character",localName="character",whereClause="missing",
                                          vconnection="JDBCConnection"),
          function(func_name,includeMID,outColNames,viewName,localName,whereClause=";",vconnection)
          {
              return(outputSelectMatrix(func_name,includeMID,
                                        outColNames,viewName,localName,whereClause=";",vconnection))
          })

setMethod("outputSelectMatrix", signature(func_name="character",includeMID="missing",outColNames="list",
                                          viewName="character",localName="character",whereClause="missing",
                                          vconnection="missing"),
          function(func_name,includeMID,outColNames,viewName,localName,whereClause=";")
          {
              return(outputSelectMatrix(func_name,outColNames=outColNames,viewName=viewName,
                                        localName=localName,whereClause=";"))
          })
setMethod("outputSelectMatrix", signature(func_name="character",includeMID="missing",outColNames="missing",
                                          viewName="character",localName="character",whereClause="missing",
                                          vconnection="missing"),
          function(func_name,includeMID,
                   outColNames=list("OutputRowNum","OutputColNum","OutputVal"),
                   viewName,localName,whereClause=";")
          {
              return(outputSelectMatrix(func_name,
                                        outColNames=list("OutputRowNum","OutputColNum","OutputVal"),viewName=viewName,
                                        localName=localName,whereClause=";"))
          })
setMethod("outputSelectMatrix", signature(func_name="character",includeMID="logical",outColNames="missing",
                                          viewName="character",localName="character",whereClause="missing",
                                          vconnection="RODBC"),
          function(func_name,includeMID,
                   outColNames=list("OutputRowNum","OutputColNum","OutputVal"),
                  viewName,localName,whereClause=";",vconnection)
          {
              return(outputSelectMatrix(func_name,includeMID,
                                        outColNames=list("OutputRowNum","OutputColNum","OutputVal"),
                                        viewName,localName,whereClause=";",vconnection))
          })
setMethod("outputSelectMatrix", signature(func_name="character",includeMID="logical",outColNames="missing",
                                          viewName="character",localName="character",whereClause="missing",
                                          vconnection="JDBCConnection"),
          function(func_name,includeMID,
                   outColNames=list("OutputRowNum","OutputColNum","OutputVal"),
                  viewName,localName,whereClause=";",vconnection)
          {
              return(outputSelectMatrix(func_name,includeMID,
                                        outColNames=list("OutputRowNum","OutputColNum","OutputVal"),
                                        viewName,localName,whereClause=";",vconnection))
          })

                                        # checkSingularity throws error if FLMatrix object is singular
                                        # setGeneric("checkSingularity", function(object) {
                                        #     standardGeneric("checkSingularity")
                                        # })
                                        # setMethod("checkSingularity", signature(object="FLMatrix"),
                                        #           function(object) {
                                        #           rank <- rankMatrix(object)
                                        #           if(rank < base::min(nrow(object),ncol(object)))
                                        #           stop("input matrix is exactly singular")
                                        #           })

                                        # # checkHermitianPositiveDefinite throws error if FLMatrix object is not HermitianPositiveDefinite
                                        # setGeneric("checkHermitianPositiveDefinite", function(object) {
                                        #     standardGeneric("checkHermitianPositiveDefinite")
                                        # })
                                        # setMethod("checkHermitianPositiveDefinite", signature(object="FLMatrix"),
                                        #           function(object) {
                                        #           ### Phani-- check if matrix = its conjugated transpose (hermitian)
                                        #           ### Phani-- check if eigenvalues of matrix are all +ve.
                                        #           return(TRUE)
                                        #           })

                                        #checkSameDims throws error if FLMatrix objects have different dimensions
setGeneric("checkSameDims", function(object1,object2) {
    standardGeneric("checkSameDims")
})
setMethod("checkSameDims", signature(object1="FLMatrix",object2="FLMatrix"),
          function(object1,object2) {
              if(!((nrow(object1)==nrow(object2))&&(ncol(object1)==ncol(object2))))
                  return(stop("ERROR: Invalid matrix dimensions for Operation"))
          })

print.FLMatrix <- function(object)
{
    ##gk: todo: implement caching
    print(as.matrix(object,sparse=TRUE))
}

setMethod("show","FLMatrix",print.FLMatrix)

setGeneric("checkMaxQuerySize", function(pObj1) {
    standardGeneric("checkMaxQuerySize")
})
setMethod("checkMaxQuerySize",
          signature(pObj1="character"),
          function(pObj1) {
              return(object.size(pObj1)>999000) # 1Kb tolerance
          })
setMethod("checkMaxQuerySize",
          signature(pObj1="FLMatrix"),
          function(pObj1) checkMaxQuerySize(constructSelect(pObj1)))
setMethod("checkMaxQuerySize",
          signature(pObj1="FLVector"),
          function(pObj1) checkMaxQuerySize(constructSelect(pObj1)))

