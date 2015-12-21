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


## A select from models a select from a table
#' @slot db_name character
#' @slot table_name character
setClass("FLSelectFrom",
         contains="FLTableQuery",
         slots=list(
             db_name = "character",
             table_name = "character"
         ))

#' An S4 class to represent FLMatrix
#'
#' @slot dimnames list dimension names of FLMatrix object
setClass(
    "FLMatrix",
    contains="FLSelectFrom",
    slots = list(
        dimnames = "list"
    )
)

setClass("FLTableFunctionQuery",
         contains="FLTableQuery",
         slots=list(
             SQLquery = "character"
         ))

#' An S4 class to represent FLMatrix
#'
#' @slot odbc_connection ODBC connectivity for R
#' @slot by character either rows or cols
setClass("FLUnionTables",
         slots = list(parts = "list",
                      by = "character"))




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


## gk: localName maybe needs adding
setGeneric("constructSelect", function(object,localName) {
    standardGeneric("constructSelect")
})
setMethod("constructSelect", signature(object = "FLMatrix",localName="character"),
          function(object,localName) {
            ifelse(""!=localName,t <- paste0(localName,"."),t <- "")
              paste0(" SELECT ",
                     ifelse(!is.numeric(object@variables$matrixId),t,""),
                     object@variables$matrixId,",",
                     t,object@variables$rowId,",",
                     t,object@variables$colId,",",
                     t,object@variables$value, 
                     " FROM ",remoteTable(object)," ",localName," ",
                     constructWhere(constraintsSQL(object,localName)))
          })

setMethod("constructSelect", signature(object = "FLMatrix",localName="missing"),
          function(object) constructSelect(object,""))

setMethod("constructSelect", signature(object = "FLTable"),
          function(object) {
              if(!object@isDeep) {
                  return(paste0("SELECT ",
                                paste0(object@variables$obs_id_colname,","),
                                paste(colnames(object),collapse=", "),
                                " FROM ",remoteTable(object),
                                constructWhere(c(constraintsSQL(object))),
                                " ORDER BY ",object@variables$obs_id_colname))
              } else {
                  return(paste0("SELECT ",
                                paste(c(object@variables$obs_id_colname,
                                        object@variables$var_id_colname,
                                        object@variables$cell_val_colname),
                                      collapse=", "),
                                " FROM ",remoteTable(object),
                                constructWhere(c(constraintsSQL(object)))))
              }
          })

setMethod("constructSelect", signature(object = "FLVector"),
          function(object) {
              if(!object@isDeep) {
                  return(paste0("SELECT ",
                                paste0(object@variables$obs_id_colname,","),
                                paste(colnames(object),collapse=", "),
                                " FROM ",remoteTable(object),
                                constructWhere(c(constraintsSQL(object))),
                                " ORDER BY ",object@variables$obs_id_colname))
              } else {
                  return(paste0("SELECT ",
                                paste(c(object@variables$obs_id_colname,
                                        object@variables$var_id_colname,
                                        object@variables$cell_val_colname),
                                      collapse=", "),
                                " FROM ",remoteTable(object),
                                constructWhere(c(constraintsSQL(object)))))
              }
          })

setMethod("constructSelect", signature(object = "FLUnionTables"),
          function(object) {
              paste0(sapply(object@parts,constructSelect),
                     collapse="\nUNION ALL ")
          })
setMethod("constructSelect", signature(object = "FLSelectFrom"),
          function(object) {
              variables <- object@variables
              if(is.null(names(variables)))
                  names(variables) <- variables
              else
                  names(variables)[is.na(names(variables))] <- variables[is.na(names(variables))]
              if(length(object@order)==0)
                  ordering <- ""
              else
                  # ordering <- paste0(" ORDER BY ",paste0(object@obs_id_colname,collapse = ", "))
                  ordering <- paste0(" ORDER BY ",paste0(object@variables,collapse = ", "))
              
              return(paste0("SELECT ",
                            paste0(variables, " as ", names(variables), collapse = ","),
                            " FROM ",remoteTable(object),
                            constructWhere(c(constraintsSQL(object))),
                            ordering))
          })


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
					 database=result_db_name, 
					 matrix_table, 
					 matrix_id_value = "",
					 matrix_id_colname = "", 
					 row_id_colname = "ROW_ID", 
					 col_id_colname = "COL_ID", 
					 cell_val_colname = "CELL_VAL", 
                     nrow=0,
                     ncol=0,
					 dimnames = NULL,
                     conditionDims=c(FALSE,FALSE),
                     whereconditions=c("")){
    RESULT <- new("FLMatrix",
                  odbc_connection = connection, 
                  db_name = database, 
                  table_name = matrix_table, 
                  variables=list(
                      rowId=row_id_colname,
                      colId=col_id_colname,
                      value=cell_val_colname),
                  order = "",
                  dimnames = list(),
                  whereconditions=whereconditions)

    if(""!=matrix_id_value && ""!=matrix_id_colname){
        RESULT@variables$matrixId <- matrix_id_colname
        whereconditions <- c(whereconditions,
                             equalityConstraint(
                                 tableColName =  paste0(getRemoteTableName(database,matrix_table),
                                                        ".",matrix_id_colname),
                                 constantValue = matrix_id_value))
    } else
        RESULT@variables$matrixId <- 1

    RESULT@whereconditions <- whereconditions
    if(is.null(dimnames)){
        rownames <- sort(sqlQuery(connection, 
                                  paste0("SELECT unique(",row_id_colname,") as rownames
							 FROM ",getRemoteTableName(database,matrix_table),
                             constructWhere(constraintsSQL(RESULT))))$rownames)
        ## gk: max is broken, eg. ?
        ## FLMatrix(connection,"FL_TRAIN","tblmatrixMulti",3)
        if(is.numeric(rownames) && length(rownames)!=max(rownames))
            rownames <- base::union(1:max(rownames),rownames)
        
        colnames <- sort(sqlQuery(connection, 
                                  paste0("SELECT unique(",col_id_colname,") as colnames
							 FROM ",getRemoteTableName(database,matrix_table),
                             constructWhere(constraintsSQL(RESULT))))$colnames)
        if(is.numeric(colnames) && length(colnames)!=max(colnames))
            colnames <- base::union(1:max(colnames),colnames)
        
        dimnames <- list(rownames,colnames)
    }
    
    ## add conditions
    if(conditionDims[[1]])
        whereconditions <- c(whereconditions,
                             inCondition(paste0(database,".",matrix_table,".",row_id_colname),
                                         dimnames[[1]]))
    if(conditionDims[[2]])
        whereconditions <- c(whereconditions,
                             inCondition(paste0(database,".",matrix_table,".",col_id_colname),
                                         dimnames[[2]]))
    ##browser()
    ## if(length(dimnames)!=0 && ((length(dimnames[[1]])!=0 && length(dimnames[[1]])!=nrow) ||
    ##                            (length(dimnames[[2]])!=0 && length(dimnames[[2]])!=nrow)))
    ## {
    ##     stop(" ERROR in dimnames: length of dimnames not equal to array extent ")
    ## }
    RESULT@dimnames <- dimnames
    RESULT@whereconditions <- whereconditions
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
          function(object) object@odbc_connection)
setMethod("getConnection", signature(object = "FLTable"),
          function(object) object@odbc_connection)
setMethod("getConnection", signature(object = "FLUnionTables"),
          function(object) object@parts[[1]]@odbc_connection)
setMethod("getConnection", signature(object = "FLTableQuery"),
          function(object) object@odbc_connection)


setGeneric("constraintsSQL", function(object, localName) {
    standardGeneric("constraintsSQL")
})
setMethod("constraintsSQL", signature(object = "FLMatrix",localName="character"),
          function(object,localName="") {
              constraints <- object@whereconditions
              return(localizeConstraints(constraints,
                                         remoteTable(object),
                                         localName))
          })
setMethod("constraintsSQL", signature(object = "FLMatrix",localName="missing"),
          function(object) constraintsSQL(object,""))

setMethod("constraintsSQL", signature(object = "FLTable",localName="character"),
          function(object,localName="") {
              conditions <- c(object@whereconditions)
              return(localizeConstraints(conditions,
                                         remoteTable(object),
                                         localName))
          })
setMethod("constraintsSQL", signature(object = "FLTable",localName="missing"),
          function(object) constraintsSQL(object,""))

setMethod("constraintsSQL", signature(object = "FLVector",localName="character"),
          function(object,localName="") {
              conditions <- c(object@whereconditions)
              ## equalityConstraint(tableColName =  paste0(remoteTable(object),".",object@obs_id_colname),
              ##                    constantValue = object@vector_id_value))
              return(localizeConstraints(conditions,
                                         remoteTable(object),
                                         localName))
          })
setMethod("constraintsSQL", signature(object = "FLSelectFrom",localName="missing"),
          function(object) constraintsSQL(object,""))

setMethod("constraintsSQL", signature(object = "FLSelectFrom",localName="character"),
          function(object,localName="") {
              constraints <- object@whereconditions
              return(localizeConstraints(constraints,
                                         remoteTable(object),
                                         localName))
          })
setMethod("constraintsSQL", signature(object = "FLMatrix",localName="missing"),
          function(object) constraintsSQL(object,""))

constructWhere <- function(conditions) {
    if(!is.character(conditions))
        stop("Provide constraints as character vector")
    conditions <- setdiff(conditions,c(NA,""))
    if(length(conditions)>0)
        paste0(" WHERE ",paste0("(",conditions,")",
                                collapse=" AND "))
    else
        ""
}


setGeneric("remoteTable", function(object, table) {
    standardGeneric("remoteTable")
})
setMethod("remoteTable", signature(object = "FLMatrix", table="missing"),
          function(object)
              getRemoteTableName(object@db_name,object@table_name))
setMethod("remoteTable", signature(object = "FLTable", table="missing"),
          function(object)
              getRemoteTableName(object@db_name,object@table_name))
setMethod("remoteTable", signature(object = "character", table="character"),
          function(object,table)
              getRemoteTableName(object,table))
setMethod("remoteTable", signature(object = "FLSelectFrom", table="missing"),
          function(object)
              getRemoteTableName(object@db_name,object@table_name))


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
              return(paste0(" WITH ",withName,
                            " (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
              AS (",constructSelect(object,localName="a")," ) "))
          })

                                        # outputSelectMatrix apples function given by func_name to view given by viewname
                                        # and returns columns specified by outcolnames list. IncludeMID tells if max_matrix_id_value
                                        # should be one of the columns returned.
setGeneric("outputSelectMatrix", function(func_name,includeMID,outColNames,viewName,localName,whereClause) {
    standardGeneric("outputSelectMatrix")
})
setMethod("outputSelectMatrix", signature(func_name="character",includeMID="missing",outColNames="list",
                                          viewName="character",localName="character",whereClause="character"),
          function(func_name,includeMID,outColNames,viewName,localName,whereClause)
          {
              return(paste0(" SELECT ",paste0(localName,".",outColNames,collapse=","),paste0(" 
          FROM TABLE (",func_name,
          "(",viewName,".Matrix_ID, ",viewName,".Row_ID, ",viewName,".Col_ID, ",viewName,".Cell_Val) 
          HASH BY z.Matrix_ID 
          LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS ",localName," ",whereClause)))
          })

setMethod("outputSelectMatrix", signature(func_name="character",includeMID="logical",outColNames="list",
                                          viewName="character",localName="character",whereClause="character"),
          function(func_name,includeMID,outColNames,viewName,localName,whereClause)
          {
              return(paste0(" SELECT ",ifelse(includeMID,max_matrix_id_value,max_vector_id_value),
                            paste0(",",localName,".",outColNames,collapse=""),paste0(" 
          FROM TABLE (",func_name,
          "(",viewName,".Matrix_ID, ",viewName,".Row_ID, ",viewName,".Col_ID, ",viewName,".Cell_Val) 
          HASH BY z.Matrix_ID 
          LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS ",localName," ",whereClause)))
          })

setMethod("outputSelectMatrix", signature(func_name="character",includeMID="missing",outColNames="missing",
                                          viewName="character",localName="character",whereClause="character"),
          function(func_name,includeMID,outColNames,viewName,localName,whereClause)
          {
              return(outputSelectMatrix(func_name,
                                        outColNames=list("OutputRowNum","OutputColNum","OutputVal"),viewName=viewName,
                                        localName=localName,whereClause=whereClause))
          })
setMethod("outputSelectMatrix", signature(func_name="character",includeMID="logical",outColNames="missing",
                                          viewName="character",localName="character",whereClause="character"),
          function(func_name,includeMID,
                   outColNames=list("OutputRowNum","OutputColNum","OutputVal"),viewName,localName,whereClause)
          {
              return(outputSelectMatrix(func_name,includeMID,outColNames=list("OutputRowNum","OutputColNum","OutputVal")
                                       ,viewName,localName,whereClause))
          })
setMethod("outputSelectMatrix", signature(func_name="character",includeMID="logical",outColNames="list",
                                          viewName="character",localName="character",whereClause="missing"),
          function(func_name,includeMID,outColNames,viewName,localName,whereClause=";")
          {
              return(outputSelectMatrix(func_name,includeMID,outColNames,viewName,localName,whereClause=";"))
          })
setMethod("outputSelectMatrix", signature(func_name="character",includeMID="missing",outColNames="list",
                                          viewName="character",localName="character",whereClause="missing"),
          function(func_name,includeMID,outColNames,viewName,localName,whereClause=";")
          {
              return(outputSelectMatrix(func_name,outColNames=outColNames,viewName=viewName,
                                        localName=localName,whereClause=";"))
          })
setMethod("outputSelectMatrix", signature(func_name="character",includeMID="missing",outColNames="missing",
                                          viewName="character",localName="character",whereClause="missing"),
          function(func_name,includeMID,
                   outColNames=list("OutputRowNum","OutputColNum","OutputVal"),
                   viewName,localName,whereClause=";")
          {
              return(outputSelectMatrix(func_name,
                                        outColNames=list("OutputRowNum","OutputColNum","OutputVal"),viewName=viewName,
                                        localName=localName,whereClause=";"))
          })
setMethod("outputSelectMatrix", signature(func_name="character",includeMID="logical",outColNames="missing",
                                          viewName="character",localName="character",whereClause="missing"),
          function(func_name,includeMID,
                   outColNames=list("OutputRowNum","OutputColNum","OutputVal"),viewName,localName,whereClause=";")
          {
              return(outputSelectMatrix(func_name,includeMID,
                                        outColNames=list("OutputRowNum","OutputColNum","OutputVal"),viewName,localName,whereClause=";"))
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
setMethod("show","FLUnionTables",print.FLMatrix)

