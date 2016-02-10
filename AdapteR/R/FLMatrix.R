#' @include utilities.R
#' @include FLTable.R
#' @include FLVector.R
NULL
setOldClass("RODBC")


## A table query models a select or a table result of a sql statement
#' @slot odbc_connection ODBC connectivity for R
setClass("FLTableQuery",
         slots=list(
             odbc_connection = "ANY",
             variables="list",
             whereconditions="character",
             order = "character"
         ))


##' A selectFrom models a select from a table
##' 
##' @slot db_name character
##' @slot table_name character
setClass("FLSelectFrom",
         contains="FLTableQuery",
         slots=list(
             db_name = "character",
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
##' @slot dimnames list of 2 elements with row, column names of FLMatrix object
##' @slot dimmap list of 2 FLTableQuery instances (or NULL) that map row_ids in the select to row-names in R
setClass(
    "FLMatrix",
    slots = list(
        select = "FLTableQuery",
        dimmap = "list",
        dimnames = "list"
    )
)

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
          signature(object = "character",returnType="character",connection="RODBC"),
          function(object,returnType,connection) store.character(object,returnType,connection))
setMethod("store",
          signature(object = "character",returnType="character",connection="JDBCConnection"),
          function(object,returnType,connection) store.character(object,returnType,connection))

store.FLMatrix <- function(object)
{
    if("FLMatrix" %in% class(object))
        if("FLSelectFrom" %in% class(object@select))
            return(object)
    if(is.FLMatrix(object)){
        MID <- getMaxMatrixId(getConnection(object))
        object <- updateVariable(object,"MATRIX_ID", MID) ## "max(MATRIX_ID)+1"
        object <- orderVariables(object,
                                 c("MATRIX_ID",
                                   "rowIdColumn",
                                   "colIdColumn",
                                   "valueColumn"))
        vSqlStr <- paste0(" INSERT INTO ",
                          getRemoteTableName(getOption("ResultDatabaseFL"),
                                            getOption("ResultMatrixTableFL")),
                          "\n",
                          gsub("'%insertIDhere%'",MID,constructSelect(object)),
                          "\n")

        sqlSendUpdate(getConnection(object),
                      vSqlStr)
		return(FLMatrix(
            connection = getConnection(object),
            database = getOption("ResultDatabaseFL"), 
            matrix_table = getOption("ResultMatrixTableFL"), 
            matrix_id_value = MID,
            matrix_id_colname = "MATRIX_ID", 
            row_id_colname = "rowIdColumn", 
            col_id_colname = "colIdColumn", 
            cell_val_colname = "valueColumn",
            ))
    }
}

store.FLVector <- function(object)
{
  connection <- getConnection(object)
  VID <- getMaxVectorId(connection)
  if(length(colnames(object))>1 && object@isDeep==FALSE)
  {
    object <- as.vector(object)
    return(as.FLVector(object,connection))
  }
  vSqlStr <- paste0(" INSERT INTO ",
                    getRemoteTableName(result_db_name,
                                      result_vector_table),
                    "\n",
                   gsub("'%insertIDhere%'",VID,constructSelect(object)),
                    "\n")
  sqlSendUpdate(getConnection(object),
                  vSqlStr)

  table <- FLTable(getConnection(object),
                   result_db_name,
                   result_vector_table,
                   "vectorIndexColumn",
                   whereconditions=paste0(result_db_name,".",result_vector_table,".vectorIdColumn = ",VID)
                  )

  return(table[,"vectorValueColumn"])
}

store.FLTable <- function(object)
{
  connection <- getConnection(object)
  table_name <- gen_unique_table_name("store")
  vSqlStr <- paste0(" CREATE TABLE ",
                    result_db_name,".",table_name,
                    " AS(",constructSelect(object),
                    ") WITH DATA;")

  sqlSendUpdate(connection,
                  vSqlStr)

  if(object@isDeep)
  table <- FLTable(connection,
                   result_db_name,
                   table_name,
                   "obs_id_colname",
                   "var_id_colname",
                   "cell_val_colname"
                  )
  else
  table <- FLTable(connection,
                   result_db_name,
                   table_name,
                   "obs_id_colname"
                  )
  return(table)
}
drop.FLTable <- function(object)
{
  vSqlStr <- paste0(" DROP TABLE ",
                    object@select@db_name,".",object@select@table_name)

  sqlSendUpdate(getConnection(object),
                  vSqlStr)
  return(paste0(object@select@table_name," DROPPED"))
}
store.character <- function(object,returnType,connection)
{
  if(toupper(returnType)=="MATRIX")
  {
    MID <- getMaxMatrixId(connection)
    vSqlStr <- paste0(" INSERT INTO ",
                      getRemoteTableName(getOption("ResultDatabaseFL"),
                                        getOption("ResultMatrixTableFL")),
                      "\n",
                      gsub("'%insertIDhere%'",MID,object),
                      "\n")

    sqlSendUpdate(connection,
                  vSqlStr)

    return(FLMatrix(
            connection = connection,
            database = getOption("ResultDatabaseFL"), 
            matrix_table = getOption("ResultMatrixTableFL"), 
            matrix_id_value = MID,
            matrix_id_colname = "MATRIX_ID", 
            row_id_colname = "rowIdColumn", 
            col_id_colname = "colIdColumn", 
            cell_val_colname = "valueColumn",
            ))
  }

  if(toupper(returnType)=="VECTOR")
  {
    
    VID <- getMaxVectorId(connection)
    vSqlStr <- paste0(" INSERT INTO ",
                      getRemoteTableName(getOption("ResultDatabaseFL"),
                                        getOption("ResultVectorTableFL")),
                      "\n",
                      gsub("'%insertIDhere%'",MID,object),
                      "\n")
    
    sqlSendUpdate(connection,
                  vSqlStr)

    table <- FLTable(connection,
                 getOption("ResultDatabaseFL"),
                 getOption("ResultVectorTableFL"),
                 "VECTOR_INDEX",
                 whereconditions=paste0(getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL"),".VECTOR_ID = ",VID)
                 )

    return(table[,"VECTOR_VALUE"])
  }
}


store.FLVector <- function(object)
{
  vSqlStr <- paste0(" INSERT INTO ",
                    getRemoteTableName(getOption("ResultDatabaseFL"),
                                      getOption("ResultVectorTableFL")),
                    "\n",
                    constructSelect(object),
                    "\n")
  sqlSendUpdate(getConnection(object),
                  vSqlStr)
  VID <- getMaxVectorId(getConnection(object))

  table <- FLTable(getConnection(object),
                   getOption("ResultDatabaseFL"),
                   getOption("ResultVectorTableFL"),
                   "VECTOR_INDEX",
                   whereconditions=paste0(getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL"),".VECTOR_ID = ",VID)
                   )

  return(table[,"VECTOR_VALUE"])
}
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
    contains="FLSelectFrom",
    slots = list(
        dimnames        = "list",
        isDeep = "logical"
    )
)

#' An S4 class to represent FLVector
#'
setClass(
    "FLVector",
    contains="FLSelectFrom",
    slots = list(
    	dimnames        = "list",
      isDeep= "logical"
    ))



setGeneric("getVariables", function(object) {
    standardGeneric("getVariables")
})
setMethod("getVariables",
          signature(object = "FLTableQuery"),
          function(object) object@variables)
setMethod("getVariables",
          signature(object = "FLMatrix"),
          function(object) getVariables(object@select))
setMethod("getVariables",
          signature(object = "FLTable"),
          function(object) getVariables(object@select))
setMethod("getVariables",
          signature(object = "FLVector"),
          function(object) getVariables(object@select))

## gk: localName maybe needs adding
setGeneric("constructSelect", function(object,localName) {
    standardGeneric("constructSelect")
})
setMethod("constructSelect",
          signature(object = "FLMatrix",
                    localName="character"),
          function(object,localName)
              constructSelect(object@select,localName))
setMethod("constructSelect",
          signature(object = "FLMatrix",
                    localName="missing"),
          function(object) constructSelect(object,""))

setMethod("constructSelect",
          signature(object = "FLTable",localName="missing"),
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
                colnames <- colnames[colnames!=vobsIDCol]
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

setMethod("constructSelect", signature(object = "FLVector",localName="missing"),
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
        variables <- getVariables(object)
        if(is.null(names(variables)))
            names(variables) <- variables
        else
            names(variables)[is.na(names(variables))] <- variables[is.na(names(variables))]
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
    ## add conditions
    if(conditionDims[[1]])
        whereconditions <-
        c(whereconditions,
          inCondition(paste0(remoteTable(object),".",
                             object@select@variables$rowIdColumn),
                      dimnames[[1]]))
    if(conditionDims[[2]])
        whereconditions <-
        c(whereconditions,
          inCondition(paste0(remoteTable(object),".",
                             object@select@variables$colIdColumn),
                      dimnames[[2]]))

    object@select@whereconditions <-
        unique(c(object@select@whereconditions,
                 whereconditions))
    object@dimnames <- dimnames
    object
}

#' Constructor function for FLMatrix.
#'
#' \code{FLMatrix} constructs an object of class \code{FLMatrix}.
#'
#' \code{FLMatrix} object is an in-database equivalent to matrix object.
#' This object is used as input for matrix operation functions.
#' @param connection ODBC connection handle as returned by \code{\link[RODBC]{odbcConnect}}
#' @param database name of the database
#' @param matrix_table name of the matrix table
#' @param matrix_id_value identifier for the input matrix
#' @param matrix_id_colname matrix id value in \code{matrix_table}
#' @param row_id_colname column name in \code{matrix_table} where row numbers are stored
#' @param col_id_colname column name in \code{matrix_table} where column numbers are stored
#' @param cell_val_colname column name in \code{matrix_table} where matrix elements are stored
#' @return \code{FLMatrix} returns an object of class FLMatrix mapped
#' to an in-database matrix.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' @export
FLMatrix <- function(connection, 
					 database=getOption("ResultDatabaseFL"), 
					 matrix_table, 
					 matrix_id_value = "",
					 matrix_id_colname = "", 
					 row_id_colname = "rowIdColumn", 
					 col_id_colname = "colIdColumn", 
					 cell_val_colname = "valueColumn", 
                     dim=NULL,
                     dimnames = NULL,
                     conditionDims=c(FALSE,FALSE),
                     whereconditions=c("")){
    ##browser()
    mConstraint <- equalityConstraint(
        tableColName =  paste0(
            getRemoteTableName(database,
                               matrix_table),
            ".",matrix_id_colname),
        constantValue = matrix_id_value)
    select <- new(
        "FLSelectFrom",
        odbc_connection = connection, 
        db_name = database, 
        table_name = matrix_table, 
        variables=list(
            rowIdColumn=row_id_colname,
            colIdColumn=col_id_colname,
            valueColumn=cell_val_colname),
        whereconditions=c(whereconditions, mConstraint),
        order = "")
    
    if(is.null(dimnames)){
        rownames <- sort(
            sqlQuery(
                connection, 
                paste0("SELECT unique(",
                       row_id_colname,") as rownames\n",
                       "FROM  ",getRemoteTableName(database,matrix_table),
                       constructWhere(constraintsSQL(select)),
                       "\nORDER 1"))$rownames)
        ## gk: max is broken, eg. ?
        ## FLMatrix(connection,"FL_TRAIN","tblmatrixMulti",3)
        ## if(is.numeric(rownames) && length(rownames)!=max(rownames))
        ##     rownames <- base::union(1:max(rownames),rownames)
        
        colnames <- sort(
            sqlQuery(
                connection, 
                paste0("SELECT unique(",col_id_colname,") as colnames\n",
                       "FROM\n ",getRemoteTableName(database,matrix_table),
                       constructWhere(constraintsSQL(select)),
                       "\nORDER 1"))$colnames)
        ## if(is.numeric(colnames) && length(colnames)!=max(colnames))
        ##     colnames <- base::union(1:max(colnames),colnames)
        browser()
        dimnames <- list(rownames,colnames)
    }
    
    RESULT <- new(
        "FLMatrix",
        select = select,
        dim = dim,
        dimnames = dimnames)

    
    if(""!=matrix_id_value && ""!=matrix_id_colname){
        select@variables$matrixId <- matrix_id_colname
        RESULT <- restrictFLMatrix(
            RESULT,
            "",
            dimnames,
            conditionDims)
    }
    return(RESULT)
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

localizeConstraints <- function(constraints, fullname, localName=""){
    ##browser()
    if(localName!="")
        gsub(fullname, localName, constraints)
    else 
        gsub(paste0(fullname,"."), localName, constraints)
}




## gk: todo:  I doubt that we need different connections for different options...
## we need to discuss moving the connection from an object slot to an option
setGeneric("getConnection", function(object) {
    standardGeneric("getConnection")
})
setMethod("getConnection", signature(object = "FLMatrix"),
          function(object) object@select@odbc_connection)
setMethod("getConnection", signature(object = "FLTable"),
          function(object) object@select@odbc_connection)
setMethod("getConnection", signature(object = "FLTableQuery"),
          function(object) object@select@odbc_connection)
setMethod("getConnection", signature(object = "FLVector"),
          function(object) object@select@odbc_connection)


setGeneric("constraintsSQL", function(object, localName) {
    standardGeneric("constraintsSQL")
})
setMethod("constraintsSQL", signature(object = "FLMatrix",localName="character"),
          function(object,localName="") {
              return(constraintsSQL(object@select,localName))
          })
setMethod("constraintsSQL", signature(object = "FLMatrix",localName="missing"),
          function(object) constraintsSQL(object,""))

setMethod("constraintsSQL", signature(object = "FLTable",localName="character"),
          function(object,localName="") {
              return(constraintsSQL(object@select,localName))
          })
setMethod("constraintsSQL", signature(object = "FLTable",localName="missing"),
          function(object) constraintsSQL(object,""))

setMethod("constraintsSQL", signature(object = "FLVector",localName="character"),
          function(object,localName="") {
              return(constraintsSQL(object@select,localName))
          })
setMethod("constraintsSQL", signature(object = "FLVector",localName="missing"),
          function(object) constraintsSQL(object,""))

setMethod("constraintsSQL", signature(object = "FLSelectFrom",localName="missing"),
          function(object) constraintsSQL(object,""))

setMethod("constraintsSQL", signature(object = "FLSelectFrom",localName="character"),
          function(object,localName="") {
              constraints <- object@whereconditions
              return(localizeConstraints(constraints,
                                         remoteTable(object),
                                         localName))
          })


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
          function(object)
              getRemoteTableName(object@db_name,
                                            object@table_name))


##setMethod("show","FLSparseMatrix",print.FLSparseMatrix)


### gk::  can you please comment these?  I like that you take abstraction seriously!
### Phani-- added comments

## checkSquare takes FLMatrix object and a character indicating name of function
## If the object is non-square, error is thrown inside the given function
## setGeneric("checkSquare", function(object,func_name) {
##     standardGeneric("checkSquare")
## })
## setMethod("checkSquare", signature(object = "FLMatrix",func_name="character"),
##           function(object,func_name="") {
##               if(nrow(object) != ncol(object)) 
##         stop(paste0(func_name," function is applicable on square matrix only"))
##           })
## setMethod("checkSquare", signature(object = "FLMatrix",func_name="missing"),
##           function(object) checkSquare(object,""))



## gk: I applaude the purpose, but the implementation was still inconsistent.
## gk: Never return partial expressions -- very error prone! (your bracket "with z(" was not closed within the function)!
## gk:  add second parameter for z please.
## gk: comment
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
              object <- orderVariables(
                  updateVariable(object,"Matrix_ID",-1),
                  c("Matrix_ID","rowIdColumn","colIdColumn","valueColumn")
              )
              return(paste0(" WITH ",withName,
                            " (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
              AS (",constructSelect(object,localName=localName)," ) "))
          })

                                        # outputSelectMatrix apples function given by func_name to view given by viewname
                                        # and returns columns specified by outcolnames list. IncludeMID tells if max_matrix_id_value
                                        # should be one of the columns returned.
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

