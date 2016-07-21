getAlias <- function(object){
  return(names(object@select@table_name))
}
getObsIdColname <- function(object){
  if(object@isDeep && ncol(object)>1)
  return("var_id_colname")
  else return("obs_id_colname")
}

## returns INT for integers or bool,VARCHAR(255)
## for characters and FLOAT for numeric
getFLColumnType <- function(x,columnName=NULL){
    if(is.FL(x)){
      if(is.null(columnName)){
        vmapping <- c(valueColumn="FLMatrix",
                    vectorValueColumn="FLVector",
                    cell_val_colname="FLTable")
        columnName <- as.character(names(vmapping)[class(x)==vmapping])
      }
    ## Deprecated as no alternative for 'TYPE' in Aster and Hive
    #   if(!grepl("with",tolower(constructSelect(x)))){
    #     vresult <- tolower(sqlQuery(getOption("connectionFL"),
    #                         limitRowsSQL(paste0("SELECT TYPE(a.",columnName,
    #                                           ") \n FROM (",constructSelect(x),
    #                                           ") a"),1))[1,1])
    #     vmapping <- c("VARCHAR","INT","FLOAT","FLOAT")
    #     vtemp <- as.vector(sapply(c("char","int","float","number"),
    #                     function(y)
    #                     return(grepl(y,vresult))))
    #     vresult <- vmapping[vtemp]
    #   }
    #   else vresult <- "FLOAT"
    # }
    # else{
      vsqlstr <- limitRowsSQL(constructSelect(x),1)
      x <- sqlQuery(getConnection(),vsqlstr)
      colnames(x) <- tolower(colnames(x))
      x <- x[[tolower(columnName)]]
    }
    vmapping <- c(VARCHAR="character",
                  INT="integer",
                  FLOAT="numeric",
                  VARCHAR="logical")
    vresult <- names(vmapping)[vmapping==class(x)]
    if(vresult=="VARCHAR") 
    vresult <- "VARCHAR(255)"
    return(vresult)
}

setGeneric("getIdColname",function(object)
      standardGeneric("getIdColname"))
setMethod("getIdColname",signature(object="FLMatrix"),
      function(object){
        return("MATRIX_ID")
        })
setMethod("getIdColname",signature(object="FLVector"),
      function(object){
        return("vectorIdColumn")
        })
setMethod("getIdColname",signature(object="FLTable"),
      function(object){
        return("obs_id_colname")
        })


setGeneric("getValueColumn",function(object)
      standardGeneric("getValueColumn"))
setMethod("getValueColumn",signature(object="FLMatrix"),
      function(object){
        return(c(valueColumn=getVariables(object)[["valueColumn"]]))
        })
setMethod("getValueColumn",signature(object="FLVector"),
      function(object){
        if(object@isDeep)
        return(c(cell_val_colname=getVariables(object)[["cell_val_colname"]]))
        else{
          vtemp <- ""
          if(!is.null(getAlias(object)) && 
            getAlias(object)!="")
          vtemp <- paste0(getAlias(object),".")
          return(sapply(colnames(object),
                      function(x){
                        if(!grepl(vtemp,x))
                        return(paste0(vtemp,x))
                        else return(x)
                        }))
        }
      })

setMethod("getValueColumn",signature(object="FLTable"),
      function(object){
        if(object@isDeep)
        return(c(cell_val_colname=getVariables(object)[["cell_val_colname"]]))
        vtemp <- ""
        if(!is.null(getAlias(object)) && 
            getAlias(object)!="")
        vtemp <- paste0(getAlias(object),".")
        return(sapply(colnames(object),
                      function(x){
                        if(!grepl(vtemp,x))
                        return(paste0(vtemp,x))
                        else return(x)
                        }))
        })

getFLPlatform <- function(){
  return(getOption("FLPlatform"))
}

setMethod("getConnection",
          signature(object = "missing"),
          function(object) 
            getOption("connectionFL"))