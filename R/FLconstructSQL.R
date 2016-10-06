#' @include FLMatrix.R
NULL


setClass("FLSkalarAggregate",
         slots=list(
             func="character",
             arguments="list"
         ))


#' Models sparse data objects.
#' 
#' Sparse Indexed values objects are table queries with a value column and
#' one (vectors), two (matrices) or several (arrays) index columns.
#'
#' The name of the values column
setClass("FLIndexedValues", slots=list(
                                dims = "integer"
                       ## dimColumns = "character" gk: todo: this needs some refactoring for FLVector
                       ))

setClass("FLAbstractColumn",
	slots=list(
            columnName = "character"))

#' A table query models a select or a table result of a sql statement.
#' 
#' 
#' @slot connectionName character variable not used right now, will be used for future support of working with several connections (to different platforms possibly)
#' @slot variables list Named list of variables for the table query: values are rownames, names (keys) are result column names.
#' @slot whereconditions character vector of strings restricting the select query (if any)
#' @slot order character ordering statements (if any)
#' @export
setClass("FLTableQuery",
         slots=list(
             variables  = "list",
             connectionName = "character",
             whereconditions="character",
             order = "character"
         ))

##' A selectFrom models a select from a table.
##'
##' @slot table_name character the name of the table to select from (possibly fully qualified, i.e. with database)
##' @export
setClass("FLSelectFrom",
         contains="FLTableQuery",
         slots=list(
             table_name = "character"
         ))

##' A TableFunctionQuery models a select from an arbitrary query
##'
##' @slot SQLquery character The free SQL query returning a table.
##' @export
setClass("FLTableFunctionQuery",
         contains="FLTableQuery",
         slots=list(
             SQLquery = "character"
         ))

setClass("FLAbstractTable",
    slots = list(
        select = "FLTableQuery",
        Dimnames = "list",
        isDeep = "logical",
        mapSelect = "FLSelectFrom"
    )
)

##' An S4 class to represent FLMatrix.
##' A Matrix can be either based off a query from a deep table (customizable by any where-condition)
##' -- or based off an arbitrary SQL statement returning a deep table.
##'
##' @slot dimnames list of 2 elements with row, column names of FLMatrix object
##' @slot dim list of 2 FLTableQuery instances (or NULL) that map row_ids in the select to row-names in R
##' @export
setClass("FLMatrix",
         contains="FLIndexedValues",
         slots = list(
             select = "FLTableQuery",
             mapSelect  = "FLSelectFrom",
             dimColumns = "character",
             type       = "character",
             Dimnames = "ANY"
         ), prototype = prototype(
             dimColumns=c("MATRIX_ID","rowIdColumn","colIdColumn","valueColumn"),
             type="double")
         )


setClass("FLMatrix.Hadoop", contains = "FLMatrix")
setClass("FLMatrix.TD", contains = "FLMatrix")
setClass("FLMatrix.TDAster", contains = "FLMatrix")

newFLMatrix <- function(...) {
  vtemp <- list(...)
  if(is.TDAster()){
      vtemp[["dimColumns"]]=c("matrix_id","rowidcolumn",
                              "colidcolumn","valuecolumn")
  }
  return(do.call("new",
                c(Class=paste0("FLMatrix.",getFLPlatform()),
                  vtemp)))
}

#' An S4 class to represent FLVector
#'
#' @export
setClass("FLVector",
         contains="FLIndexedValues",
         slots = list(
             select = "FLTableQuery",
             Dimnames = "list",
             isDeep= "logical",
             mapSelect = "FLSelectFrom",
             type       = "character"
         ),
         prototype = prototype(type="double")
         )

setClass("FLVector.Hadoop", contains = "FLVector")
setClass("FLVector.TD", contains = "FLVector")
setClass("FLVector.TDAster", contains = "FLVector")

newFLVector <- function(...) {
    new(paste0("FLVector.",getFLPlatform()), ...)
}

#' An S4 class to represent FLVector
#'
#' @export
setClass("FLSimpleVector",
         contains="FLIndexedValues",
         slots = list(
             select = "FLTableQuery",
             dimColumns = "character",
             names = "ANY",
             type       = "character"
         ),prototype = prototype(type="double")
         )


setGeneric("getValueSQLName", function(object) {
    standardGeneric("getValueSQLName")
})
setMethod("getValueSQLName",
          signature(object = "FLMatrix"),
          function(object) object@dimColumns[[4]])
setMethod("getValueSQLName",
          signature(object = "FLVector"),
          function(object) "vectorValueColumn") ## gk @ phani -- is that currently really constant??
setMethod("getValueSQLName",
          signature(object = "FLSimpleVector"),
          function(object) object@dimColumns[[2]])
setMethod("getValueSQLName",
          signature(object = "FLAbstractColumn"),
          function(object) object@columnName)

setGeneric("getValueSQLExpression", function(object) {
    standardGeneric("getValueSQLExpression")
})
setMethod("getValueSQLExpression",
          signature(object = "FLIndexedValues"),
          function(object) object@select@variables[[getValueSQLName(object)]])
setMethod("getValueSQLExpression",
          signature(object = "FLAbstractColumn"),
          function(object) object@columnName)



setGeneric("setValueSQLExpression", function(object, func,...) {
    standardGeneric("setValueSQLExpression")
})
setMethod("setValueSQLExpression",
          signature(object = "FLIndexedValues"),
          function(object,func,...) {
    object@select@variables[[getValueSQLName(object)]] <- func(object,...)
    object
})


setGeneric("getIndexSQLExpression", function(object,margin=1) {
    standardGeneric("getIndexSQLExpression")
})
setMethod("getIndexSQLExpression",
          signature(object = "FLIndexedValues"),
          function(object,margin=1)
              object@select@variables[[getIndexSQLExpressions(object=object,margin=margin)]])
setMethod("getIndexSQLExpression",
          signature(object = "FLAbstractColumn"),
          function(object,margin=1) object@columnName)

setGeneric("getIndexSQLName", function(object,margin) {
    standardGeneric("getIndexSQLName")
})
setMethod("getIndexSQLName",
          signature(object = "FLMatrix"),
          function(object,margin=1:2) object@dimColumns[2:3][margin])
setMethod("getIndexSQLName",
          signature(object = "FLVector"),
          function(object,margin=1) stop("use FLSimpleVector"))
setMethod("getIndexSQLName",
          signature(object = "FLSimpleVector"),
          function(object,margin=1) object@dimColumns[[1]])


setGeneric("setValueSQLExpression", function(object, func,...) {
    standardGeneric("setValueSQLExpression")
})
setMethod("setValueSQLExpression",
          signature(object = "FLIndexedValues"),
          function(object,func,...) {
    object@select@variables[[getValueSQLName(object)]] <- func(object,...)
    object
})



#' An S4 class to represent FLTable, an in-database data.frame.
#'
#' @slot select FLTableQuery the select statement for the table.
#' @slot dimnames the observation id and column names
#' @slot isDeep logical (currently ignored)
#' @method names FLTable
#' @param object retrieves the column names of FLTable object
#' @export
setClass("FLTable",
         slots = list(
             select = "FLTableQuery",
             Dimnames = "list",
             dims = "numeric",
             isDeep = "logical",
             mapSelect = "FLSelectFrom",
             type       = "character"
         ),
         prototype = prototype(type="double")
        )


setClass("FLTable.Hadoop", contains = "FLTable")
setClass("FLTable.TD", contains = "FLTable")
setClass("FLTable.TDAster", contains = "FLTable")

newFLTable <- function(...) {
    new(paste0("FLTable.",getFLPlatform()),
        ...)
}


#' An S4 class to represent FLTableMD, an in-database data.frame.
#'
#' @slot select FLTableQuery the select statement for the table.
#' @slot dimnames the observation id and column names
#' @slot isDeep logical (currently ignored)
#' @slot mapSelect \code{FLSelectFrom} object which contains the 
#' mapping information if any
#' @export
setClass("FLTableMD",
         contains="FLTable",
         slots = list(
             select = "FLTableQuery",
             Dimnames = "list",
             dims = "numeric",
             isDeep = "logical",
             mapSelect = "FLSelectFrom"
         )
         )

#' computes the length of FLVector object.
#' @param obj is a FLVector object.
#' @return \code{length} returns a R Vector giving the length of input object.
#' @export
length.FLSimpleVector <- function(obj) obj@dims

#' Get names of a FLVector
#'
#' @param x FLVector
#' @return character vector of names
#' of FLVector if exists. Else NULL
#' @export
names.FLSimpleVector <- function(x) x@names


#' Converts FLVector object to vector in R
#' @export
as.vector.FLSimpleVector <- function(object,mode="any")
{
    x <- sqlQuery(connection,constructSelect(object))
    return(x[[object@dimColumns[[2]]]])
}

#' @export
FLSerial <- function(min,max){
    new("FLSimpleVector",
        select=new("FLSelectFrom",
                   table_name="fzzlSerial",
                   connectionName=getFLConnectionName(),
                   variables=list(serialVal="serialVal"),
                   whereconditions=c(paste("serialVal>=",min),paste("serialVal<=",max)),
                   order="serialVal"),
        dimColumns = c("serialVal","serialVal"),
        names=NULL,
        dims    = as.integer(max-min+1),
        type       = "integer"
        )
}

##' constructs a sql statement returning the
##' deep table representation of the object.
##' 
##' @param object the object to query
##' @param ... arguments passed on to SQL generation. see joinNames
##' @return a character SQL representation
##' @export
setGeneric("constructSelect", function(object,...) {
    standardGeneric("constructSelect")
})

setMethod("constructSelect", signature(object = "FLSimpleVector"),
          function(object,...) {
    return(constructSelect(object@select,...))
})

setMethod("constructSelect", signature(object = "FLMatrix"),
          function(object,joinNames=TRUE,...){
            if(!"matrix_id" %in% tolower(names(getVariables(object))))
            object@select@variables <- c(list(MATRIX_ID= "'%insertIDhere%'"),
                                        getVariables(object))
            if(!is.null(object@dimColumns))
                names(object@select@variables) <- object@dimColumns
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

setMethod("constructSelect", signature(object = "FLTableQuery"),
          function(object,...) {
              return(paste0("SELECT ",
                            paste(colnames(object),collapse=", "),
                            " FROM ",tableAndAlias(object),
                            constructWhere(c(constraintsSQL(object))),
                            constructOrder(orderVars=object@order,...),
                            "\n"))
          })


setMethod("constructSelect", signature(object = "FLTable"),
          function(object,...) {
            if(class(object@select)=="FLTableFunctionQuery") 
            return(constructSelect(object@select))
            #browser()
              if(!object@isDeep) 
              {
                variables <- getVariables(object)
                # if(is.null(names(variables)))
                #     names(variables) <- variables
                # else
                #     names(variables)[is.na(names(variables))] <- variables[is.na(names(variables))]

                ifelse(is.null(variables$obs_id_colname),
                  vobsIDCol <- variables["vectorIndexColumn"],
                   vobsIDCol <- variables["obs_id_colname"])
                
                colnames <- appendTableName(colnames(object),
                              names(object@select@table_name)[1])
                #colnames <- colnames[colnames!=vobsIDCol]
                newColnames <- renameDuplicates(colnames(object))
                variables <- as.list(c(vobsIDCol[[1]],colnames))
                names(variables) <- c("obs_id_colname",
                                      newColnames)

                # return(paste0(
                #             "SELECT\n",
                #             paste0("     ",
                #                    variables," AS ",
                #                    names(variables),
                #                    collapse = ",\n"),
                #             "\n FROM ",tableAndAlias(object),
                #             constructWhere(c(constraintsSQL(object))),
                #             "\n"))
              }
              else 
              {
                variables <- getVariables(object)
                # if(is.null(names(variables)))
                #   names(variables) <- variables
                # else
                #     names(variables)[is.na(names(variables))] <- variables[is.na(names(variables))]

                # variables <- as.list(c(variables[["obs_id_colname"]],
                #   variables[["var_id_colname"]],
                #   variables[["cell_val_colname"]]))
                # names(variables) <- c("obs_id_colname",
                #                       "var_id_colname",
                #                       "cell_val_colname")
                # return(paste0(
                #           "SELECT\n",
                #           paste0("     ",
                #                  variables," AS ",
                #                  names(variables),
                #                  collapse = ",\n"),
                #           "\n FROM ",tableAndAlias(object),
                #           constructWhere(c(constraintsSQL(object))),
                #           "\n"))
              }
              object@select@variables <- variables
              return(constructSelect(object@select))
          })

setMethod("constructSelect", signature(object = "FLVector"),
          function(object,joinNames=TRUE,...) {
            if(class(object@select)=="FLTableFunctionQuery") 
                return(constructSelect(object@select))
            ## If mapSelect exists join tables
            # mapTable <- ""
            # addWhereClause <- ""
            # if(joinNames && length(object@mapSelect@table_name)>0)
            # {
            #   # if(ncol(object)==1) newnames <- rownames(object)
            #   # else newnames <- colnames(object)
            #   # namesflvector <- newFLVector(
            #   #           select=object@mapSelect,
            #   #           Dimnames=list(newnames,"NAME"),
            #   #           isDeep=FALSE)
            #   mapTable <- paste0(",(",constructSelect(object@mapSelect),") AS b ")
            # }
            variables <- getVariables(object)
            if(!object@isDeep) {
              newColnames <- renameDuplicates(colnames(object))
              # if(is.null(names(variables)))
              #     names(variables) <- variables
              # else
              #     names(variables)[is.na(names(variables))] <- variables[is.na(names(variables))]

              ifelse(is.null(variables$obs_id_colname),
                vobsIDCol <- variables["vectorIndexColumn"],
                 vobsIDCol <- variables["obs_id_colname"])

              # if(mapTable!="")
              # {
              #   addWhereClause <- c(paste0("CAST(",vobsIDCol[[1]]," AS VARCHAR(100)) = b.nameColname "))
              #   vobsIDCol[[1]] <- "b.numIdColname"
              # }
              colnames <- appendTableName(colnames(object),
                              names(object@select@table_name)[1])

              variables <- as.list(c("'%insertIDhere%'",vobsIDCol[[1]],colnames))
              names(variables) <- c("vectorIdColumn",
                                    "vectorIndexColumn",
                                    if(length(colnames(object))>1)
                                     newColnames else "vectorValueColumn")

              # return(paste0(
              #             "SELECT\n",
              #             paste0("     ",
              #                    variables," AS ",
              #                    names(variables),
              #                    collapse = ",\n"),
              #             "\n FROM ",tableAndAlias(object),mapTable,
              #             constructWhere(c(constraintsSQL(object),addWhereClause)),
              #             "\n"))
            } else {
                # if(is.null(names(variables)))
                #   names(variables) <- variables
                # else
                #     names(variables)[is.na(names(variables))] <- variables[is.na(names(variables))]

                if(ncol(object)>1 || (ncol(object)==1 &&colnames(object)==1 &&nrow(object)==1))
                vobsIDCol <- variables["var_id_colname"]
                else vobsIDCol <- variables["obs_id_colname"]

                # if(mapTable!="")
                # {
                #   addWhereClause <- c(paste0("CAST(",vobsIDCol[[1]]," AS VARCHAR(100)) = b.nameColname "))
                #   vobsIDCol[[1]] <- "b.numIdColname"
                # }
                variables <- as.list(c("'%insertIDhere%'",
                                    vobsIDCol[[1]],
                                    variables[["cell_val_colname"]]))
                names(variables) <- c("vectorIdColumn",
                                      "vectorIndexColumn",
                                      "vectorValueColumn")
                # return(paste0(
                #           "SELECT\n",
                #           paste0("     ",
                #                  variables," AS ",
                #                  names(variables),
                #                  collapse = ",\n"),
                #           "\n FROM ",tableAndAlias(object),mapTable,
                #           constructWhere(c(constraintsSQL(object),addWhereClause)),
                #           "\n"))
            }
              select <- object@select
              select@variables <- variables
              if(joinNames && length(object@mapSelect@table_name)>0)
              {
                select@variables <- c(variables,
                                    object@mapSelect@variables)
                select@table_name <- c(select@table_name,
                                       object@mapSelect@table_name)
                select@whereconditions <- c(select@whereconditions,
                                            object@mapSelect@whereconditions)
              }
              return(constructSelect(select))
          })

constructVariables <- function(variables){
  #browser()
    if(!is.null(names(variables)))
        return(paste0("     ",
                      variables, " ",
                      names(variables),
                      collapse = ",\n"))
    else
        return(paste0("     ",
                      variables, " ",
                      collapse = ",\n"))
               
}
setMethod("constructSelect",
    signature(object = "FLSelectFrom"),
    function(object,...) {
      #browser()
        variables <- getVariables(object)
        return(paste0(
            "SELECT\n",
            constructVariables(variables),
            "\n FROM ",tableAndAlias(object),
            constructWhere(c(constraintsSQL(object))),
            constructOrder(orderVars=object@order,...),
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


constructOrder <- function(orderVars, order=TRUE,...) {
    orderVars <- setdiff(orderVars,c(NA,""))
    if(!order | (length(orderVars)==0))
        return("")
    paste0("\n ORDER BY ",
           paste0(orderVars, collapse = ", "))
}

constructWhere <- function(conditions) {
    conditions <- setdiff(conditions,c(NA,""))
    if(length(conditions)==0)
      return("")
    if(!is.character(conditions))
        stop("Provide constraints as character vector")
    if(length(conditions)>0)
        paste0(" WHERE",paste0("   (",conditions,")",
                                collapse=" AND "))
    else
        ""
}


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
            if(!"matrix_id" %in% tolower(names(getVariables(object))))
            object@select@variables <- c(list(MATRIX_ID= "'%insertIDhere%'"),
                                        getVariables(object))
            
              object <- orderVariables(object,
                  c("MATRIX_ID","rowIdColumn","colIdColumn","valueColumn")
              )
              return(paste0(" WITH ",withName,
                            " (MATRIX_ID, Row_ID, Col_ID, Cell_Val)
              AS (",constructSelect(object,joinNames=FALSE)," ) "))
          })

## outputSelectMatrix apples function given by func_name to view given by viewname
## and returns columns specified by outcolnames list. IncludeMID tells if max_matrix_id_value
## should be one of the columns returned.
setGeneric("outputSelectMatrix",
           function(func_name,
                    viewName,
                    localName,...) {
    standardGeneric("outputSelectMatrix")
})

setMethod("outputSelectMatrix", signature(func_name="character",
                                          viewName="character",
                                          localName="character"),
          function(func_name,
                   viewName,
                   localName,
                   includeMID=FALSE,
                   outColNames=list(rowIdColumn="OutputRowNum",
                                    colIdColumn="OutputColNum",
                                    valueColumn="OutputVal"),
                   whereClause=";",...)
          {
              return(paste0(" SELECT ",
                            ifelse(!includeMID,"",
                                   "'%insertIDhere%' AS Matrix_ID, "),
                            constructVariables(outColNames),
                            "\n FROM TABLE (",func_name,
                            "(",viewName,".Matrix_ID, ",
                            viewName,".Row_ID, ",
                            viewName,".Col_ID, ",
                            viewName,".Cell_Val)",
                            " HASH BY z.Matrix_ID 
          LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID ",
          ") AS ",localName," ",whereClause))
          })
