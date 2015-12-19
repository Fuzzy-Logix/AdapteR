#' @include utilities.R
#' @include FLTable.R
#' @include FLVector.R
NULL
setOldClass("RODBC")

## gk: localName maybe needs adding
setGeneric("constructSelect", function(object) {
    standardGeneric("constructSelect")
})
setMethod("constructSelect", signature(object = "FLMatrix"),
          function(object) {
              paste0(" SELECT ",
                     object@row_id_colname,",",
                     object@col_id_colname,",",
                     object@cell_val_colname,  
                     " FROM ",remoteTable(object),
                     constructWhere(constraintsSQL(object)))
          })
setMethod("constructSelect", signature(object = "FLTable"),
          function(object) {
              if(!object@isDeep) {
                  return(paste0("SELECT ",
                                paste0(object@obs_id_colname,","),
                                paste(colnames(object),collapse=", "),
                                " FROM ",remoteTable(object),
                                constructWhere(c(constraintsSQL(object))),
                                " ORDER BY ",object@obs_id_colname))
              } else {
                  return(paste0("SELECT ",
                                paste(c(object@obs_id_colname,
                                        object@var_id_colname,
                                        object@cell_val_colname),
                                      collapse=", "),
                                " FROM ",remoteTable(object),
                                constructWhere(c(constraintsSQL(object)))))
              }
          })
setMethod("constructSelect", signature(object = "FLUnionMatrix"),
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
                  ordering <- paste0(" ORDER BY ",paste0(object@obs_id_colname,collapse = ", "))
              
              return(paste0("SELECT ",
                            paste0(variables, "as", names(variables), collapse = ","),
                            " FROM ",remoteTable(object),
                            constructWhere(c(constraintsSQL(object))),
                            ordering))
          })


## gk: please move all classes into AllClasses.R
setClass("FLTableQuery",
         slots=list(
             odbc_connection = "ANY",
             variables="character",
             whereconditions="character",
             order = "character"
         ))


## gk: please move all classes into AllClasses.R
setClass("FLSelectFrom",
         contains="FLTableQuery",
         slots=list(
             db_name = "character",
             matrix_table = "character"
         ))

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



#' An S4 class to represent FLMatrix
#'
#' @slot odbc_connection ODBC connectivity for R
#' @slot db_name character
#' @slot matrix_table character
#' @slot matrix_id_value numeric id value
#' @slot matrix_id_colname character
#' @slot row_id_colname character
#' @slot col_id_colname character
#' @slot cell_val_colname character
#' @slot nrow numeric number of rows of FLMatrix object
#' @slot ncol numeric number of cols of FLMatrix object
#' @slot dimnames list dimension names of FLMatrix object
#'
setClass(
    "FLMatrix",
    contains="FLTableQuery",
    slots = list(
        dimnames = "list"
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
    contains="FLTableQuery",
    slots = list(
        dimnames        = "list",
        obs_id_colname  = "character", ## former primary_key
        isDeep = "logical",
        var_id_colname  = "character", ## 
        cell_val_colname = "character"
    )
)

#' An S4 class to represent FLVector
#'
setClass(
    "FLVector",
    contains="FLTableQuery",
    slots = list(
    	names        = "list",
    	obs_id_colname  = "character", ##former primary_key
        var_id_colname="character", 
    	cell_val_colname = "character"
    ))

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
					 matrix_id_value,
					 matrix_id_colname = "MATRIX_ID", 
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
                  matrix_table = matrix_table, 
                  matrix_id_value = as.character(matrix_id_value), 
                  matrix_id_colname = matrix_id_colname, 
                  row_id_colname = row_id_colname, 
                  col_id_colname = col_id_colname, 
                  cell_val_colname = cell_val_colname,
                  dimnames = list(),
                  whereconditions=whereconditions)
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
setMethod("getConnection", signature(object = "FLUnionMatrix"),
          function(object) object@parts[[1]]@odbc_connection)


setGeneric("constraintsSQL", function(object, localName) {
    standardGeneric("constraintsSQL")
})
setMethod("constraintsSQL", signature(object = "FLMatrix",localName="character"),
          function(object,localName="") {
              constraints <- c(
                  object@whereconditions,
                  equalityConstraint(
                      tableColName =  paste0(remoteTable(object),".",object@matrix_id_colname),
                      constantValue = object@matrix_id_value))
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
              conditions <- c(
                  object@whereconditions,
                  object@whereconditions)
              ## equalityConstraint(tableColName =  paste0(remoteTable(object),".",object@obs_id_colname),
              ##                    constantValue = object@vector_id_value))
              return(localizeConstraints(conditions,
                                         remoteTable(object),
                                         localName))
          })
setMethod("constraintsSQL", signature(object = "FLVector",localName="missing"),
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
              getRemoteTableName(object@db_name,object@matrix_table))
setMethod("remoteTable", signature(object = "FLTable", table="missing"),
          function(object)
              getRemoteTableName(object@db_name,object@table_name))
setMethod("remoteTable", signature(object = "character", table="character"),
          function(object,table)
              getRemoteTableName(object,table))



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
              AS (SELECT ",localName,".",object@matrix_id_colname,", 
                     ",localName,".",object@row_id_colname,", 
                     ",localName,".",object@col_id_colname,", 
                     ",localName,".",object@cell_val_colname,
              " FROM  ",remoteTable(object)," ",localName," ",
              constructWhere(constraintsSQL(object,localName)),
              " ) "))
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
