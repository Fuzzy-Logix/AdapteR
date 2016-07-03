#' @include FLMatrix.R
NULL


setOldClass("RODBC")


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

setMethod("constructSelect",
          signature(object = "FLMatrix"),
          function(object,joinNames=TRUE){
            if(!"matrix_id" %in% tolower(names(getVariables(object))))
            object@select@variables <- c(list(MATRIX_ID= "'%insertIDhere%'"),
                                        getVariables(object))

              if(!FLNamesMappedP(object) | !joinNames)
                  return(constructSelect(object@select))
              select <- object@select
              select@variables <- c(select@variables,
                                    object@mapSelect@variables)
              select@table_name <- c(select@table_name,
                                     object@mapSelect@table_name)
              select@whereconditions <- c(select@whereconditions,
                                          object@mapSelect@whereconditions)
              select@database <- c(select@database,
                                  rep(object@mapSelect@database,length(select@table_name)))
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
                #             "\n FROM ",remoteTable(object),
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
                #           "\n FROM ",remoteTable(object),
                #           constructWhere(c(constraintsSQL(object))),
                #           "\n"))
              }
              object@select@variables <- variables
              return(constructSelect(object@select))
          })

setMethod("constructSelect", signature(object = "FLVector"),
          function(object,joinNames=TRUE) {
            if(class(object@select)=="FLTableFunctionQuery") 
            return(constructSelect(object@select))
            ## If mapSelect exists join tables
            # mapTable <- ""
            # addWhereClause <- ""
            # if(joinNames && length(object@mapSelect@table_name)>0)
            # {
            #   # if(ncol(object)==1) newnames <- rownames(object)
            #   # else newnames <- colnames(object)
            #   # namesflvector <- new("FLVector",
            #   #           select=object@mapSelect,
            #   #           dimnames=list(newnames,"NAME"),
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
              #             "\n FROM ",remoteTable(object),mapTable,
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
                #           "\n FROM ",remoteTable(object),mapTable,
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
                select@database <- c(select@database,
                                    rep(object@mapSelect@database,
                                      length(select@table_name)))
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
setMethod(
    "constructSelect",
    signature(object = "FLSelectFrom"),
    function(object) {
      #browser()
        if(is.null(object@database)) return(NULL)
        if(length(object@database)==0) return(NULL)
        variables <- getVariables(object)
        order <- setdiff(object@order,c(NA,""))
        if(length(order)==0)
            ordering <- ""
        else
            ## ordering <- paste0(" ORDER BY ",paste0(object@obs_id_colname,collapse = ", "))
            ordering <- paste0("\n ORDER BY ",
                               paste0(order,
                                      collapse = ", "))
        return(paste0(
            "SELECT\n",
            constructVariables(variables),
            "\n FROM ",remoteTable(object),
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


## Phani-- removed \n as it was creating problem in FLCorrel test cases
constructWhere <- function(conditions) {
    if(!is.character(conditions))
        stop("Provide constraints as character vector")
    conditions <- setdiff(conditions,c(NA,""))
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
