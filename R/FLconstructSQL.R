#' @include FLMatrix.R
NULL


setClass("FLSkalarAggregate",
         slots=list(
             func="character",
             arguments="list"
         ))
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
             order = "character",
             group = "character"
         ))





#' Models sparse data objects.
#' 
#' Sparse Indexed values objects are table queries with a value column and
#' one (vectors), two (matrices) or several (arrays) index columns.
#'
#' The name of the values column
setClass("FLIndexedValues", slots=list(
                                select = "FLTableQuery",
                                Dimnames = "list",
                                dims = "ANY",
                                dimColumns = "character",
                                type = "character"## gk: todo: this needs some refactoring for FLVector
                            ))

setClass("FLAbstractColumn",
	slots=list(
            columnName = "character"))




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

##' @export
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
             mapSelect  = "FLSelectFrom"
             ##dimColumns = "character",
             ), prototype = prototype(
                    dimColumns=c("MATRIX_ID","rowIdColumn","colIdColumn","valueColumn"),
                    type="double")
         )

##' @export
setClass("FLMatrix.Hadoop", contains = "FLMatrix")
##' @export
setClass("FLMatrix.TD", contains = "FLMatrix")
##' @export
setClass("FLMatrix.TDAster", contains = "FLMatrix")

newFLMatrix <- function(...) {
  vtemp <- list(...)
  ## Results in Aster are not case-sensitive
  # if(is.TDAster()){
  #     vtemp[["dimColumns"]]=c("matrix_id","rowidcolumn",
  #                             "colidcolumn","valuecolumn")
  # }
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
             isDeep= "logical",
             mapSelect = "FLSelectFrom"             
         ),
         prototype = prototype(type="double")
         )

##' @export
setClass("FLVector.Hadoop", contains = "FLVector")
##' @export
setClass("FLVector.TD", contains = "FLVector")
##' @export
setClass("FLVector.TDAster", contains = "FLVector")

newFLVector <- function(...) {
  vtemp <- list(...)
  if(is.null(vtemp$dims)){
      vtemp$dims <- sapply(vtemp$Dimnames,length)
  }
  return(do.call("new",
                 c(Class=paste0("FLVector.",getFLPlatform()),
                   vtemp)))
}

#' An S4 class to represent FLVector
#'
#' @export
setClass("FLSimpleVector",
         contains="FLIndexedValues",
         slots = list(
             names = "ANY"          
             ),prototype = prototype(type="double")
         )


#' An S4 class to represent FLTable, an in-database data.frame.
#'
#' @slot select FLTableQuery the select statement for the table.
#' @slot dimnames the observation id and column names
#' @slot isDeep logical (currently ignored)
#' @method names FLTable
#' @param object retrieves the column names of FLTable object
#' @export
setClass("FLSimpleWideTable",
         contains="FLIndexedValues",
         slots = list(
             dims = "integer"
             ##mapSelect = "FLSelectFrom",          
             ))


#' @export
setGeneric("getValueSQLName", function(object) {
    standardGeneric("getValueSQLName")
})
setMethod("getValueSQLName",
          signature(object = "FLVector"),
          function(object) "vectorValueColumn") ## gk @ phani -- is that currently really constant??
setMethod("getValueSQLName",
          signature(object = "FLSimpleVector"),
          function(object) object@dimColumns[[2]])
setMethod("getValueSQLName",
          signature(object = "FLAbstractColumn"),
          function(object) object@columnName)
setMethod("getValueSQLName",
          signature(object = "FLMatrix"),
          function(object) object@dimColumns[[4]])

#' @export
setGeneric("setValueSQLName", function(object,value) {
    standardGeneric("setValueSQLName")
})
setMethod("setValueSQLName",
          signature(object = "FLMatrix"),
          function(object,value){
          t <- names(object@select@variables)
          t[t==object@dimColumns[[4]]] <- value
          names(object@select@variables) <- t
          object@dimColumns[[4]] <- value
          object
          })
# setMethod("setValueSQLName",
#           signature(object = "FLVector"),
#           function(object) "vectorValueColumn")
setMethod("setValueSQLName",
          signature(object = "FLSimpleVector"),
          function(object,value){
          t <- names(object@select@variables)
          t[t==object@dimColumns[[2]]] <- value
          names(object@select@variables) <- t
          object@dimColumns[[2]] <- value
          object
          })
setMethod("setValueSQLName",
          signature(object = "FLAbstractColumn"),
          function(object,value){
          object@columnName <- value
          object
          })

#' @export
setGeneric("getValueSQLExpression", function(object) {
    standardGeneric("getValueSQLExpression")
})
setMethod("getValueSQLExpression",
          signature(object = "FLIndexedValues"),
          function(object) object@select@variables[[getValueSQLName(object)]])
setMethod("getValueSQLExpression",
          signature(object = "FLAbstractColumn"),
          function(object) object@columnName)



#' @export
setGeneric("setValueSQLExpression", function(object, func,...) {
    standardGeneric("setValueSQLExpression")
})
setMethod("setValueSQLExpression",
          signature(object = "FLIndexedValues"),
          function(object,func,...) {
            vres <- func(object,...)
            if(is.FLSimpleVector(vres))
                return(vres)
            else{
                object@select@variables[[getValueSQLName(object)]] <- vres 
                object
            }
})


#' @export
setGeneric("getIndexSQLExpression", function(object,margin=1) {
    standardGeneric("getIndexSQLExpression")
})
setMethod("getIndexSQLExpression",
          signature(object = "FLIndexedValues"),
          function(object,margin=1){
    nam <- getIndexSQLName(object=object,margin=margin)
    expr <- object@select@variables[[nam]]
    if(is.null(expr)) return(nam)
    expr
})
setMethod("getIndexSQLExpression",
          signature(object = "FLAbstractColumn"),
          function(object,margin=1) object@columnName)

#' @export
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
          signature(object = "FLIndexedValues"),
          function(object,margin=1) object@dimColumns[[margin]])

#' @export
setGeneric("setIndexSQLName", function(object,margin,value) {
    standardGeneric("setIndexSQLName")
})
setMethod("setIndexSQLName",
          signature(object = "FLMatrix"),
          function(object,margin,value){
            t <- names(object@select@variables)
            t[t==object@dimColumns[2:3][margin]] <- value
            names(object@select@variables) <- t
            object@dimColumns[2:3][margin] <- value
            object
            })
setMethod("setIndexSQLName",
          signature(object = "FLVector"),
          function(object,margin,value) stop("use FLSimpleVector"))
setMethod("setIndexSQLName",
          signature(object = "FLIndexedValues"),
          function(object,margin,value){
            t <- names(object@select@variables)
            t[t==object@dimColumns[margin]] <- value
            names(object@select@variables) <- t
            object@dimColumns[[margin]] <- value
            object
            })
# setMethod("setIndexSQLName",
#           signature(object = "FLTable"),
#           function(object,margin=1,value){
#             t <- names(object@select@variables)
#             t[margin] <- value
#             names(object@select@variables) <- t
#             object
#             })

#' An S4 class to represent FLTable, an in-database data.frame.
#'
#' @slot select FLTableQuery the select statement for the table.
#' @slot dimnames the observation id and column names
#' @slot isDeep logical (currently ignored)
#' @method names FLTable
#' @param object retrieves the column names of FLTable object
#' @export
setClass("FLTable",
         contains="FLIndexedValues",
         slots = list(
             select = "FLTableQuery",
             Dimnames = "list",
             mapSelect = "FLSelectFrom",
             type       = "character"
         ),
         prototype = prototype(type="double",
                                dimColumns=c("obs_id_colname"))
        )


setClass("FLTable.Hadoop", contains = "FLTable")
setClass("FLTable.TD", contains = "FLTable")
setClass("FLTable.TDAster", contains = "FLTable")


#' An S4 class to represent FLTable, an in-database data.frame.
#'
#' @slot select FLTableQuery the select statement for the table.
#' @slot dimnames the observation id and column names
#' @slot isDeep logical (currently ignored)
#' @method names FLTableDeep
#' @param object retrieves the column names of FLTable object
#' @export
setClass("FLTableDeep",
         contains="FLTable",
         slots = list(
             wideTable = "ANY",
             wideToDeepAnalysisID = "character"
         ),
         prototype = prototype(type="double",
                                dimColumns=c("obs_id_colname"))
        )


setClass("FLTableDeep.Hadoop", contains = "FLTableDeep")
setClass("FLTableDeep.TD", contains = "FLTableDeep")
setClass("FLTableDeep.TDAster", contains = "FLTableDeep")

newFLTable <- function(isDeep,...) {
    if(isDeep)
        return(new(paste0("FLTableDeep.",getFLPlatform()), ...))
    else
        return(new(paste0("FLTable.",getFLPlatform()), ...))
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
             wideTable = "ANY",
             wideToDeepAnalysisID = "character"
         ),
         prototype = prototype(type="double",
                                dimColumns=c("group_id_colname","obs_id_colname"))
        )

setClass("FLTableMD.Hadoop", contains = "FLTableMD")
setClass("FLTableMD.TD", contains = "FLTableMD")
setClass("FLTableMD.TDAster", contains = "FLTableMD")

#' An S4 class to represent FLTableMDDeep, an in-database data.frame.
#'
#' @slot select FLTableQuery the select statement for the table.
#' @slot dimnames the observation id and column names
#' @slot isDeep logical (currently ignored)
#' @slot mapSelect \code{FLSelectFrom} object which contains the 
#' mapping information if any
#' @export
setClass("FLTableMDDeep",
         contains="FLTableMD",
         slots = list(
         ),
         prototype = prototype(type="double",
                                dimColumns=c("group_id_colname","obs_id_colname"))
        )

setClass("FLTableMDDeep.Hadoop", contains = "FLTableMDDeep")
setClass("FLTableMDDeep.TD", contains = "FLTableMDDeep")
setClass("FLTableMDDeep.TDAster", contains = "FLTableMDDeep")

newFLTableMD <- function(isDeep,...) {
    if(isDeep)
        return(new(paste0("FLTableMDDeep.",getFLPlatform()), ...))
    else
        return(new(paste0("FLTableMD.",getFLPlatform()), ...))
}

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
    ## Required as in Aster output cols are always lowercase
    colnames(x) <- toupper(colnames(x))
    vres <- x[[toupper(object@dimColumns[[2]])]]
    if(!is.null(names(object)))
        names(vres) <- as.vector(names(object))
    return(vres)
}

#' @export
FLSerial <- function(min,max){
    new("FLSimpleVector",
        select=new("FLSelectFrom",
                   table_name=c(fzzlSerial="fzzlSerial"),
                   connectionName=getFLConnectionName(),
                   variables=list(indexVal="fzzlSerial.serialVal"),
                   whereconditions=c(paste("fzzlSerial.serialVal>=",min),paste("fzzlSerial.serialVal<=",max)),
                   order="indexVal"),
        dimColumns = c("indexVal","indexVal"),
        ##names=NULL,
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

setMethod("constructSelect", signature(object = "FLSimpleWideTable"),
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
                            constructGroupBy(GroupByVars=getGroupSlot(object),...),
                            constructOrder(orderVars=object@order,...),
                            "\n"))
          })


setMethod("constructSelect", signature(object = "FLTable"),
          function(object,...) {
            if(class(object@select)=="FLTableFunctionQuery") 
            return(constructSelect(object@select))
              if(!isDeep(object)) 
              {
                variables <- getVariables(object)
                ifelse(is.null(variables$obs_id_colname),
                  vobsIDCol <- variables["vectorIndexColumn"],
                   vobsIDCol <- variables["obs_id_colname"])
                
                colnames <- appendTableName(colnames(object),
                              names(getTableNameSlot(object))[1])
                newColnames <- renameDuplicates(colnames(object))
                variables <- as.list(c(vobsIDCol[[1]],colnames))
                names(variables) <- c("obs_id_colname",
                                      newColnames)
              }
              else 
              {
                variables <- getVariables(object)
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
            if(!isDeep(object)) {
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
                              names(getTableNameSlot(object))[1])

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
            constructGroupBy(GroupByVars=getGroupSlot(object),...),
            constructOrder(orderVars=getOrderSlot(object),...),
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
constructGroupBy <- function(GroupByVars,...) {
    GroupByVars <- setdiff(GroupByVars,c(NA,""))
    if(length(GroupByVars)>0)
        paste0("\n GROUP BY ",
           paste0(GroupByVars, collapse = ", "))
    else return("")
}

constructWhere <- function(conditions,
                            includeWhere=TRUE) {
    conditions <- setdiff(conditions,c(NA,""))
    if(length(conditions)==0)
      return("")
    if(!is.character(conditions))
        stop("Provide constraints as character vector")
    if(!is.null(includeWhere) && includeWhere)
        vWhere <- " WHERE "
    else vWhere <- " "
    if(length(conditions)>0)
        paste0(vWhere,paste0("   (",conditions,")",
                                collapse=" AND ")," ")
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


getSelectSlot <- function(x){
    return(tryCatch(x@select,
                    error=function(x)NULL))
}

getWhereConditionsSlot <- function(x){
    return(tryCatch(x@whereconditions,
                error=function(e)
                        return(getWhereConditionsSlot(x@select))))
}
getDimsSlot <- function(x){
    return(tryCatch(x@dims,
                    error=function(x)NULL))
}
getTypeSlot <- function(x){
    return(tryCatch(x@type,
                    error=function(x)NULL))
}
getNamesSlot <- function(x){
    return(tryCatch(x@names,
                    error=function(x)NULL))
}
getGroupSlot <- function(x){
    return(tryCatch(x@group,
                    error=function(x)NULL))
}
getOrderSlot <- function(x){
    return(tryCatch(x@order,
                    error=function(x)NULL))
}
getDimColumnsSlot <- function(x){
    return(tryCatch(x@dimColumns,
                    error=function(x)NULL))
}
setNamesSlot <- function(x,value){
    tryCatch(x@names <- value,
                    error=function(x)NULL)
    x
}



getTablename <- function(x) gsub("^[^.]*\\.","",x)
getDatabase <- function(x) {
    db <- gsub("\\.[^.]*$","",x)
    if(db=="" | db==x) db <- getOption("ResultDatabaseFL")
    db
}

#' Getter for remote table name.
#' 
#' @export
setGeneric("getTableNameSlot", function(object) {
    standardGeneric("getTableNameSlot")
})
setMethod("getTableNameSlot",
          signature(object = "FLSelectFrom"),
          function(object) object@table_name)
setMethod("getTableNameSlot",
          signature(object = "FLIndexedValues"),
          function(object) getTableNameSlot(object@select))
setMethod("getTableNameSlot",
          signature(object = "FLTable"),
          function(object) getTableNameSlot(object@select))

isDeep <- function(x) inherits(x,"FLTableDeep") | inherits(x,"FLTableMDDeep")
