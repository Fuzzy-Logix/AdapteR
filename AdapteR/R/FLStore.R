#' @export
store.FLMatrix <- function(object)
{
    ##browser()
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
                          gsub("'%insertIDhere%'",MID,
                               constructSelect(object,joinNames=FALSE)),
                          "\n")

        sqlSendUpdate(getConnection(object),
                      vSqlStr)
    return(FLMatrix(
            connection = getConnection(object),
            database = getOption("ResultDatabaseFL"), 
            table_name = getOption("ResultMatrixTableFL"), 
            matrix_id_value = MID,
            matrix_id_colname = "MATRIX_ID", 
            row_id_colname = "rowIdColumn", 
            col_id_colname = "colIdColumn", 
            cell_val_colname = "valueColumn",
            ))
    }
}

#' @export
store.FLVector <- function(object)
{
  connection <- getConnection(object)
  VID <- getMaxVectorId(connection)
  if(length(colnames(object))>1 && object@isDeep==FALSE)
  {
    object <- as.vector(object)
    names(object)<-NULL
    return(as.FLVector(object))
  }
  vSqlStr <- paste0(" INSERT INTO ",
                    getRemoteTableName(getOption("ResultDatabaseFL"),
                                      getOption("ResultVectorTableFL")),
                    "\n",
                   gsub("'%insertIDhere%'",VID,constructSelect(object)),
                    "\n")
  sqlSendUpdate(getConnection(object),
                  vSqlStr)
  select <- new(
                "FLSelectFrom",
                connection = connection, 
                database = getOption("ResultDatabaseFL"), 
                table_name = getOption("ResultVectorTableFL"),
                variables = list(
                        obs_id_colname = "vectorIndexColumn"),
                whereconditions=paste0(getOption("ResultDatabaseFL"),".",
                  getOption("ResultVectorTableFL"),".vectorIdColumn = ",VID),
                order = "")

  if(ncol(object)==1) vindex <- rownames(object)
  else vindex <- colnames(object)
  return(new("FLVector",
                select=select,
                dimnames=list(vindex,"vectorValueColumn"),
                isDeep=FALSE))
}

#' @export
store.FLTable <- function(object)
{
  connection <- getConnection(object)
  table_name <- gen_unique_table_name("store")
  vSqlStr <- paste0(" CREATE TABLE ",
                    getOption("ResultDatabaseFL"),".",table_name,
                    " AS(",constructSelect(object),
                    ") WITH DATA;")

  sqlSendUpdate(connection,
                  vSqlStr)

  if(object@isDeep)
  table <- FLTable(
                   getOption("ResultDatabaseFL"),
                   table_name,
                   "obs_id_colname",
                   "var_id_colname",
                   "cell_val_colname"
                  )
  else
  table <- FLTable(
                   getOption("ResultDatabaseFL"),
                   table_name,
                   "obs_id_colname"
                  )
  return(table)
}

#' @export
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
            table_name = getOption("ResultMatrixTableFL"), 
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

    table <- FLTable(
                 getOption("ResultDatabaseFL"),
                 getOption("ResultVectorTableFL"),
                 "VECTOR_INDEX",
                 whereconditions=paste0(getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL"),".VECTOR_ID = ",VID)
                 )

    return(table[,"VECTOR_VALUE"])
  }
}
