#' @include FLCastFunctions.R
#' @include FLStore.R
storeVarnameMapping <- function(connection,
                                tablename,
                                matrixId,
                                dimId,
                                mynames){
    Ndim <- length(mynames)
    names(mynames) <- 1:Ndim
    sqlstatements <- paste0(
        " INSERT INTO ",
        getOption("ResultDatabaseFL"),".",
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
        vtemp <- getFLColumnType(object)
        vmapping <- c("FLOAT","INT","VARCHAR(255)")
        names(vmapping) <- c(getOption("ResultMatrixTableFL"),
                            getOption("ResultIntMatrixTableFL"),
                            getOption("ResultCharMatrixTableFL"))
        vtableName <- as.character(names(vmapping)[vtemp==vmapping])
        vSqlStr <- paste0(" INSERT INTO ",
                          getRemoteTableName(getOption("ResultDatabaseFL"),
                                            vtableName),
                          "\n",
                          gsub("'%insertIDhere%'",MID,
                               constructSelect(object,joinNames=FALSE)),
                          "\n")

        sqlSendUpdate(getConnection(object),
                      vSqlStr)
    return(FLMatrix(
            connection = getConnection(object),
            database = getOption("ResultDatabaseFL"), 
            table_name = vtableName, 
            matrix_id_value = MID,
            matrix_id_colname = "MATRIX_ID", 
            row_id_colname = "rowIdColumn", 
            col_id_colname = "colIdColumn", 
            cell_val_colname = "valueColumn",
            dimnames=dimnames(object)
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
  vtemp <- getFLColumnType(object)
  vmapping <- c("FLOAT","INT","VARCHAR(255)")
  names(vmapping) <- c(getOption("ResultVectorTableFL"),
                      getOption("ResultIntVectorTableFL"),
                      getOption("ResultCharVectorTableFL"))
  vtableName <- as.character(names(vmapping)[vtemp==vmapping])
  vSqlStr <- paste0(" INSERT INTO ",
                    getRemoteTableName(getOption("ResultDatabaseFL"),
                                      vtableName),
                    "\n",
                   gsub("'%insertIDhere%'",VID,constructSelect(object)),
                    "\n")
  sqlSendUpdate(getConnection(object),
                  vSqlStr)
  select <- new(
                "FLSelectFrom",
                connection = connection, 
                database = getOption("ResultDatabaseFL"), 
                table_name = vtableName,
                variables = list(
                        obs_id_colname = "vectorIndexColumn"),
                whereconditions=paste0(getOption("ResultDatabaseFL"),".",
                  vtableName,".vectorIdColumn = ",VID),
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
                                        getOption("ResultCharMatrixTableFL")),
                      "\n",
                      gsub("'%insertIDhere%'",MID,object),
                      "\n")

    sqlSendUpdate(connection,
                  vSqlStr)

    return(FLMatrix(
            connection = connection,
            database = getOption("ResultDatabaseFL"), 
            table_name = getOption("ResultCharMatrixTableFL"), 
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
                                        getOption("ResultCharVectorTableFL")),
                      "\n",
                      gsub("'%insertIDhere%'",MID,object),
                      "\n")
    
    sqlSendUpdate(connection,
                  vSqlStr)

    table <- FLTable(
                 getOption("ResultDatabaseFL"),
                 getOption("ResultCharVectorTableFL"),
                 "VECTOR_INDEX",
                 whereconditions=paste0(getOption("ResultDatabaseFL"),".",
                    getOption("ResultCharVectorTableFL"),".VECTOR_ID = ",VID)
                 )

    return(table[,"VECTOR_VALUE"])
  }
}
