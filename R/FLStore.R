#' @include FLMatrix.R
NULL


##' stores an object in database.
##' 
##'
##' @param object the object to store. Can be FLMatrix, FLVector, FLTable, character
##' @param pTableName If provided, the object is inserted into this table and 
##' this storage information is writen into fzzlAdapteRTablesInfo. This allows 
##' for selective storage of objects
##' @param ... Additional arguments like pNote to be included in fzzlAdapteRTablesInfo
##' @return in-database object after storing
##' @export
setGeneric("store", function(object,pTableName=NULL,...) {
    standardGeneric("store")
})
setMethod("store",
          signature(object = "FLMatrix"),
          function(object,pTableName=NULL,...) 
            store.FLMatrix(object,
                          pTableName=pTableName,
                          ...))
setMethod("store",
          signature(object = "FLVector"),
          function(object,pTableName=NULL,...) 
            store.FLVector(object,
                          pTableName=pTableName,
                          ...))
setMethod("store",
          signature(object = "FLTable"),
          function(object,pTableName=NULL,...) 
            store.FLTable(object,
                          pTableName=pTableName,
                          ...))
# setMethod("store",
#           signature(object = "character"),
#           function(object,...) store.character(object,returnType,...))
# setMethod("store",
#           signature(object = "character"),
#           function(object,...) store.character(object,returnType,...))

storeVarnameMapping <- function(connection,
                                tablename,
                                matrixId,
                                dimId,
                                mynames){
    Ndim <- length(mynames)
    names(mynames) <- 1:Ndim
    sqlstatements <- paste0(
        " INSERT INTO ",
        getOption("NameMapTableFL"),
        "(TABLENAME, MATRIX_ID, DIM_ID, ",
        "NAME, NUM_ID",
        ")",
        " VALUES (",
        "'",tablename,"', ",
        "'",matrixId,"', ",
        dimId,", ",
        "'",mynames,"', ",
        names(mynames),
        ");")

    if(class(connection)=="JDBCConnection")
    {
      mdeep <- data.frame(tablename,as.integer(matrixId),as.integer(dimId),
                        as.character(mynames),as.integer(names(mynames)))
      colnames(mdeep) <- c("TABLENAME","MATRIX_ID","DIM_ID","NAME","NUM_ID")
      t <- as.FLTable.data.frame(mdeep,connection,getOption("NameMapTableFL"),2,drop=FALSE)
    }
    else
    retobj<-sqlSendUpdate(connection,
                          paste(sqlstatements,
                                collapse="\n"))
    return(mynames)
}

#' @export
store.FLMatrix <- function(object,pTableName=NULL,...)
{
    ##browser()
    if(is.FLMatrix(object)){
      if("FLSelectFrom" %in% class(object@select) 
          && is.null(pTableName))
        return(object)

      ## Get table based on type of matrix
        vtemp <- getFLColumnType(object)
        vmapping <- c("FLOAT","INT","VARCHAR(255)")
        names(vmapping) <- c(getOption("ResultMatrixTableFL"),
                            getOption("ResultIntMatrixTableFL"),
                            getOption("ResultCharMatrixTableFL"))
        vtableName1 <- as.character(names(vmapping)[vtemp==vmapping])
        MID1 <- getMaxMatrixId(vtable=vtableName1,vdatabase=NULL)

        object <- orderVariables(object,
                                 c("MATRIX_ID",
                                   "rowIdColumn",
                                   "colIdColumn",
                                   "valueColumn"))
      
      if(!is.null(pTableName)){
        vtableName <- pTableName
        ## If tablename has database name
        ## If tablename has database name
        vtemp <- separateDBName(vtableName)
        vtableName <- vtemp["vtableName"]
        vdatabase <- vtemp["vdatabase"]
        MID <- getMaxMatrixId(vtable=vtableName,vdatabase=vdatabase)
        if(class(object@select)=="FLSelectFrom")
        object@select@variables[["MATRIX_ID"]] <- MID
        
        insertIntotbl(pTableName=getRemoteTableName(vdatabase,vtableName),
                      pSelect=gsub("'%insertIDhere%'",MID,
                                  constructSelect(object,joinNames=FALSE)))
        ## Store MetaInfo if permanent Storage
        updateMetaTable(pTableName=getRemoteTableName(vdatabase,vtableName),
                      pElementID=MID,
                      pType="Matrix",
                      ...)
      }
      if(class(object@select)=="FLSelectFrom")
        object@select@variables[["MATRIX_ID"]] <- MID1
      
      insertIntotbl(pTableName=vtableName1,
                    pSelect=gsub("'%insertIDhere%'",MID1,
                                  constructSelect(object,joinNames=FALSE)))
      ## Store in volatile table

      # vSqlStr <- paste0(" INSERT INTO ",
      #                   getRemoteTableName(getOption("ResultDatabaseFL"),
      #                                     vtableName),
      #                   "\n",
      #                   gsub("'%insertIDhere%'",MID,
      #                        constructSelect(object,joinNames=FALSE)),
      #                   "\n")

      # sqlSendUpdate(getConnection(object),
      #               vSqlStr)
      return(FLMatrix(
            connection = getConnection(object),
            table_name = vtableName1, 
            matrix_id_value = MID1,
            matrix_id_colname = "MATRIX_ID", 
            row_id_colname = "rowIdColumn", 
            col_id_colname = "colIdColumn", 
            cell_val_colname = "valueColumn",
            dim=dim(object),
            dimnames=dimnames(object),
            type=typeof(object)
            ))
    }
}

#' @export
store.FLVector <- function(object,pTableName=NULL,...)
{
  connection <- getConnection(object)
  if(length(colnames(object))>1 && object@isDeep==FALSE)
  {
    object <- as.vector(object)
    names(object)<-NULL
    return(as.FLVector(object))
  }

  ## Get table based on type of matrix
    vtemp <- getFLColumnType(object)
    vmapping <- c("FLOAT","INT","VARCHAR(255)")
    names(vmapping) <- c(getOption("ResultVectorTableFL"),
                        getOption("ResultIntVectorTableFL"),
                        getOption("ResultCharVectorTableFL"))
    vtableName1 <- as.character(names(vmapping)[vtemp==vmapping])
    VID1 <- getMaxVectorId(vtable=vtableName1,vdatabase=NULL)

  if(!is.null(pTableName)){
    vtableName <- pTableName
    ## If tablename has database name
    vtemp <- separateDBName(vtableName)
    vtableName <- vtemp["vtableName"]
    vdatabase <- vtemp["vdatabase"]
    VID <- getMaxVectorId(vtable=vtableName,
                          vdatabase=vdatabase)
    
    insertIntotbl(pTableName=getRemoteTableName(vdatabase,vtableName),
                  pSelect=gsub("'%insertIDhere%'",VID,
                              constructSelect(object)))
    ## Store MetaInfo if permanent Storage
    updateMetaTable(pTableName=getRemoteTableName(vdatabase,vtableName),
                  pElementID=VID,
                  pType="vector",
                  ...)
  }

  insertIntotbl(pTableName=vtableName1,
                  pSelect=gsub("'%insertIDhere%'",VID1,
                              constructSelect(object)))
  # vSqlStr <- paste0(" INSERT INTO ",
  #                   getRemoteTableName(getOption("ResultDatabaseFL"),
  #                                     vtableName),
  #                   "\n",
  #                  gsub("'%insertIDhere%'",VID,constructSelect(object)),
  #                   "\n")
  # sqlSendUpdate(getConnection(object),
  #                 vSqlStr)
  select <- new("FLSelectFrom",
                connection = connection, 
                table_name = vtableName1,
                variables = list(
                        obs_id_colname = "vectorIndexColumn"),
                whereconditions=paste0(vtableName1,
                                ".vectorIdColumn = ",VID1),
                order = "")

  if(ncol(object)==1) vindex <- rownames(object)
  else vindex <- colnames(object)
  return(new("FLVector",
                select=select,
                dimnames=list(vindex,"vectorValueColumn"),
                isDeep=FALSE,
                type=typeof(object)))
}

#' @export
store.FLTable <- function(object,pTableName=NULL,...)
{
  connection <- getConnection(object)
  if(is.null(pTableName))
    table_name <- gen_unique_table_name("store")
  else table_name <- pTableName

  createTable(pTableName=table_name,
              pSelect=constructSelect(object))
  # vSqlStr <- paste0(" CREATE TABLE ",
  #                   getOption("ResultDatabaseFL"),".",table_name,
  #                   " AS(",constructSelect(object),
  #                   ") WITH DATA;")

  # sqlSendUpdate(connection,
  #                 vSqlStr)
  
  ## If tablename has database name
  vtemp <- separateDBName(vtableName)
  vtableName <- vtemp["vtableName"]
  vdatabase <- vtemp["vdatabase"]

  ## Store MetaInfo if permanent Storage
  updateMetaTable(pTableName=getRemoteTableName(vdatabase,vtableName),
                  pType=ifelse(object@isDeep,"deepTable","wideTable"),
                  ...)

  if(object@isDeep)
  table <- FLTable(
                   vdatabase,
                   vtableName,
                   "obs_id_colname",
                   "var_id_colname",
                   "cell_val_colname",
                   type=typeof(object)
                  )
  else
  table <- FLTable(
                   vdatabase,
                   vtableName,
                   "obs_id_colname",
                   type=typeof(object)
                  )
  return(table)
}

# #' @export
# store.character <- function(object,returnType,connection)
# {
#   if(toupper(returnType)=="MATRIX")
#   {
#     MID <- getMaxMatrixId(connection)
#     vSqlStr <- paste0(" INSERT INTO ",
#                       getRemoteTableName(getOption("ResultDatabaseFL"),
#                                         getOption("ResultCharMatrixTableFL")),
#                       "\n",
#                       gsub("'%insertIDhere%'",MID,object),
#                       "\n")

#     sqlSendUpdate(connection,
#                   vSqlStr)

#     return(FLMatrix(
#             connection = connection,
#             database = getOption("ResultDatabaseFL"), 
#             table_name = getOption("ResultCharMatrixTableFL"), 
#             matrix_id_value = MID,
#             matrix_id_colname = "MATRIX_ID", 
#             row_id_colname = "rowIdColumn", 
#             col_id_colname = "colIdColumn", 
#             cell_val_colname = "valueColumn",
#             ))
#   }

#   if(toupper(returnType)=="VECTOR")
#   {
    
#     VID <- getMaxVectorId(connection)
#     vSqlStr <- paste0(" INSERT INTO ",
#                       getRemoteTableName(getOption("ResultDatabaseFL"),
#                                         getOption("ResultCharVectorTableFL")),
#                       "\n",
#                       gsub("'%insertIDhere%'",MID,object),
#                       "\n")
    
#     sqlSendUpdate(connection,
#                   vSqlStr)

#     table <- FLTable(
#                  getOption("ResultDatabaseFL"),
#                  getOption("ResultCharVectorTableFL"),
#                  "VECTOR_INDEX",
#                  whereconditions=paste0(getOption("ResultDatabaseFL"),".",
#                     getOption("ResultCharVectorTableFL"),".VECTOR_ID = ",VID)
#                  )

#     return(table[,"VECTOR_VALUE"])
#   }
# }
