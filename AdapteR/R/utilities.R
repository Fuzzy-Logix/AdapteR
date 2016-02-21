# Contains the support functions
NULL
setOldClass("RODBC")

getRemoteTableName <- function(databaseName=getOption("ResultDatabaseFL"),
                               tableName) {
    ## gk: todo: use options(...) and start  session if required
    ##    if(is.null(getOption("ResultMatrixTableFL")))
    ##        FLStartSession()

    return(paste0(databaseName,".",tableName))
}



sqlError <- function(e){
    print(e)
    sys.call()
}
################################################################################
######  provide methods for JDBC with same signature as ODBC methods
################################################################################

if(!exists("sqlSendUpdate")) sqlSendUpdate <- function(channel,query) UseMethod("sqlSendUpdate")

sqlQuery <- function(connection,query) UseMethod("sqlQuery",connection)
## gk: this made packaging fail here, as I cannot install RODBC, and
## then it is unknown. Can we do a package check? We need to discuss
## this.
## sqlQuery.default <- RODBC::sqlQuery

options(debugSQL=TRUE)
sqlSendUpdate.JDBCConnection <- function(channel,query) {
    sapply(query, function(q){
        ##browser()
        if(getOption("debugSQL")) cat(paste0("SENDING SQL: \n",gsub(" +"," ",q),"\n"))
        tryCatch({
            R <- dbSendUpdate(connection,q)
            ##dbCommit(connection)
            return(R)
        },
        error=function(e) sqlError(e))
    })
}

sqlSendUpdate.RODBC <- function(connection,query) {
    odbcSetAutoCommit(connection, autoCommit = FALSE)
    sapply(query, function(q){
        if(getOption("debugSQL")) cat(paste0("SENDING SQL: \n",gsub(" +"," ",q),"\n"))
        err<-RODBC::sqlQuery(connection,q,errors=FALSE)
        errmsg<- odbcGetErrMsg(connection)
        if(length(errmsg) == 0 || as.character(errmsg)=="No Data")
        {
            odbcEndTran(connection, commit = TRUE)
        }
        else
        {
            odbcEndTran(connection, commit = FALSE)
            print(errmsg)
        }
        odbcClearError(connection)
    })
    odbcSetAutoCommit(connection, autoCommit = TRUE)
    cat("DONE...\n")
}

sqlQuery.JDBCConnection <- function(channel,query, ...) {
    if(length(query)==1){
        if(getOption("debugSQL")) cat(paste0("QUERY SQL: \n",query,"\n"))
        tryCatch({
            resd <- dbGetQuery(connection, query, ...)
            return(resd)
        },
        error=function(e) cat(paste0(sqlError(e))))
    }
    lapply(query, function(q){
        if(getOption("debugSQL")) cat(paste0("QUERY SQL: \n",q,"\n"))
        tryCatch({
            resd <- dbGetQuery(connection, q, ...)
            return(resd)
        },
        error=function(e) cat(paste0(sqlError(e))))
    })
}

sqlQuery.RODBC <- function(connection,query, ...) {
    if(length(query)==1){
        if(getOption("debugSQL")) cat(paste0("QUERY SQL: \n",query,"\n"))
        tryCatch({
            resd <- RODBC::sqlQuery(connection, query, ...)
            return(resd)
        },
        error=function(e) cat(paste0(sqlError(e))))
    }
    lapply(query, function(q){
        if(getOption("debugSQL")) cat(paste0("QUERY SQL: \n",q,"\n"))
        tryCatch({
            resd <- RODBC::sqlQuery(connection, q, ...)
            return(resd)
        },
        error=function(e) cat(paste0(sqlError(e))))
    })
}

list_to_where_clause <- function (x) {
    where_clause <- paste(names(x),x,sep="=\'",collapse="\' AND ");
    where_clause <- paste(where_clause,"\'",sep="");
    if(nchar(where_clause) > 1) {
        where_clause <- where_clause
    } else {
        where_clause <- "1=1"
    }
                                        #where_clause <- ifelse(nchar(where_clause) > 1, where_clause, "1=1");
    where_clause
}

                                        # /**
                                        #  * Converts List to class Spec. Used for Data Prep
                                        #  * @param  {list} x e.g. list(Varx="a",Vary="b")
                                        #  * @return {string}   "Varx(x), Vary(y)"
                                        #  */
list_to_class_spec <- function (x) {
    classSpec <- paste(names(x),x,sep="(",collapse="), ")
    classSpec <- paste(classSpec,")",sep="")
    if(nchar(classSpec) > 1) {
        classSpec <- classSpec
    } else {
        classSpec <- ""
    }
    ##classSpec <- ifelse(nchar(classSpec) > 1, classSpec, "");
    classSpec
}

                                        # /**
                                        #  * Converts List to class Spec. Used for Data Prep
                                        #  * @param  {list} x e.g. list(Varx="a",Vary="b")
                                        #  * @return {string}   "Varx(x), Vary(y)"
                                        #  */
list_to_exclude_clause <- function (x) {
    excludeClause <- paste(x, collapse=", ")
    excludeClause
}

calc_cat_to_dummy <- function(ClassSpec) {
    if (length(ClassSpec) == 0)
        CatToDummy <- 0
    else
        CatToDummy <- 1;
    CatToDummy
}

validate_args <- function (arg_list, type_list, class_list = list())
{
    for (name in names(type_list)) {
        if( typeof(arg_list[[name]]) != type_list[[name]])
        {
            stop(paste("Argument Type Mismatch:", name, "should be of type", type_list[[name]]))
        }
    }
    for (name in names(class_list)) {
        if( class(arg_list[[name]]) != class_list[[name]])
            stop(paste("Argument Type Mismatch:", name, "should be of class", class_list[[name]]))
    }
}

is_integer <- function(x) { (x == ceiling(x)||x == floor(x)) }
is_number  <- function(x) { (x == ceiling(x)||x == floor(x))&&(x>=1) }

gen_deep_table_name <- function(TableName){
    random_no <- rnorm(1);
    paste(TableName,"Deep",round(as.numeric(Sys.time())),round(random_no*random_no*10000000),sep="_");
}

trim <- function( x ) {
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

gen_score_table_name <- function(TableName){
    random_no <- rnorm(1);
    paste(TableName,"Score",round(as.numeric(Sys.time())),round(random_no*random_no*10000000),sep="_");
}

gen_wide_table_name <- function(TableName){
    random_no <- rnorm(1);
    paste(TableName,"Wide",round(as.numeric(Sys.time())),round(random_no*random_no*10000000),sep="_");
}

gen_unique_table_name <- function(TableName){
    random_no <- rnorm(1);
    paste(TableName,"Unique",round(as.numeric(Sys.time())),round(random_no*random_no*10000000),sep="_");
}

genRandVarName <- function(){
    vrnum <- rnorm(1)
    vrnum <- round(vrnum*vrnum*1000)
    vtime <- round(as.numeric(Sys.time()))
    return(paste0(sample(letters[1:26],1),vrnum,sample(letters[1:26],1),vtime))
}

FLodbcClose <- function(connection)
{
    sqlstr <- c(paste0("DROP TABLE ",getOption("ResultMatrixTableFL"),";"),
                paste0("DROP TABLE ",getOption("ResultSparseMatrixTableFL"),";"),
                paste0("DROP TABLE ",getOption("ResultVectorTableFL"),";"))

    if(length(tempDecompTableVector)>0)
	sqlstr <- c(sqlstr,paste0("DROP TABLE ",tempDecompTableVector,";"))

    sqlSendUpdate(connection,sqlstr)
    odbcClose(connection)
    flag1 <<- 0
    flag2 <<- 0
    flag3 <<- 0

    tempDecompTableVector <<- c()
}

gen_table_name <- function(prefix,suffix){
    ifelse(is.null(suffix),
           paste0(prefix),
           paste0(prefix,"_",suffix))
}

FLStartSession <- function(connection,
                           database="FL_DEMO",
                           persistent="test",
                           drop=TRUE,
                           tableoptions=", FALLBACK ,
				     NO BEFORE JOURNAL,
				     NO AFTER JOURNAL,
				     CHECKSUM = DEFAULT,
				     DEFAULT MERGEBLOCKRATIO")
{
    options(ResultDatabaseFL=database)
    ##    browser()
    options(ResultVectorTableFL=gen_table_name("tblVectorResult",persistent))
    options(ResultMatrixTableFL=gen_table_name("tblMatrixMultiResult",persistent))
    options(ResultSparseMatrixTableFL=gen_table_name("tblMatrixMultiSparseResult",persistent))
    options(MatrixNameMapTableFL=gen_table_name("tblMatrixNameMapping",persistent))
    sendqueries <- c(
        paste0("DATABASE ",getOption("ResultDatabaseFL"),";"),
        paste0("SET ROLE ALL;"))
    sqlSendUpdate(connection, sendqueries)


    tempDecompTableVector <<- c()

    if(drop){
    	sqlstr <- c(paste0("DROP TABLE ",getOption("ResultMatrixTableFL"),";"),
                    paste0("DROP TABLE ",getOption("MatrixNameMapTableFL"),";"),
                    paste0("DROP TABLE ",getOption("ResultSparseMatrixTableFL"),";"),
                    paste0("DROP TABLE ",getOption("ResultVectorTableFL"),";"))

    	if(length(tempDecompTableVector)>0)
            sqlstr <- c(sqlstr,paste0("DROP TABLE ",tempDecompTableVector,";"))

        sqlSendUpdate(connection,sqlstr)
    }

    sendqueries <- c(
        paste0(" CREATE ",ifelse(is.null(persistent),"VOLATILE TABLE ","TABLE "),
               getOption("ResultMatrixTableFL"),
               tableoptions,
               "     (
				      MATRIX_ID INTEGER,
				      rowIdColumn INTEGER,
					  colIdColumn INTEGER,
					  valueColumn FLOAT)
	    			 PRIMARY INDEX ( MATRIX_ID, rowIdColumn, colIdColumn );"),
        paste0(" CREATE ",ifelse(is.null(persistent),"VOLATILE TABLE ","TABLE "),
               getOption("MatrixNameMapTableFL"),
               tableoptions,
               "     (
			TABLENAME VARCHAR(100),
			MATRIX_ID INTEGER,
                        DIM_ID INTEGER, -- 1: row, 2: column
			NAME VARCHAR(100),
			NUM_ID INTEGER)
	    		PRIMARY INDEX (TABLENAME, MATRIX_ID, DIM_ID, NAME);"),
        paste0(" CREATE ",ifelse(is.null(persistent),"VOLATILE TABLE ","TABLE "),
               getOption("ResultSparseMatrixTableFL"),
               tableoptions,
               "
				     (
				      MATRIX_ID INTEGER,
				      rowIdColumn INTEGER,
					  colIdColumn INTEGER,
					  valueColumn FLOAT)
	    			 PRIMARY INDEX ( MATRIX_ID, rowIdColumn, colIdColumn );"),
        paste0(" CREATE ",ifelse(is.null(persistent),"VOLATILE TABLE ","TABLE "),
               getOption("ResultVectorTableFL"),
               tableoptions,
               "
			 		 ( vectorIdColumn INT,
			 		   vectorIndexColumn INT,
				 	   vectorValueColumn FLOAT )
			 		   PRIMARY INDEX (vectorIdColumn,vectorIndexColumn);" ))
    sqlSendUpdate(connection, sendqueries)
    cat("DONE..\n")
}

setGeneric("getMaxId", function(vdatabase,
                                vtable,
                                vcolName,
                                vconnection,...) {
    standardGeneric("getMaxId")
})

setMethod("getMaxId",
          signature(vdatabase="character",
                    vtable = "character",
                    vcolName="character",
                    vconnection="RODBC"),
          function(vdatabase,vtable,vcolName,vconnection,...)
          {
              sqlstr <- paste0(" SELECT MAX(",vcolName,
                               " )+1 FROM ",vdatabase,".",vtable)

              t <- sqlQuery(vconnection,sqlstr)[1,1]
              if(is.na(t)) return(0)
              else return(t)
          }
          )
setMethod("getMaxId",
          signature(vdatabase="character",
                    vtable = "character",
                    vcolName="character",
                    vconnection="JDBCConnection"),
          function(vdatabase,vtable,vcolName,vconnection,...)
          {
              sqlstr <- paste0(" SELECT MAX(",vcolName,
                               " )+1 FROM ",vdatabase,".",vtable)

              t <- sqlQuery(vconnection,sqlstr)[1,1]
              if(is.na(t)) return(0)
              else return(t)
          }
          )


setGeneric("getMaxMatrixId", function(vconnection,...) {
    standardGeneric("getMaxMatrixId")
})

setMethod("getMaxMatrixId",
          signature(vconnection="RODBC"),
          function(vconnection,...)
              getMaxValue(vdatabase=getOption("ResultDatabaseFL"),
                          vtable=getOption("ResultMatrixTableFL"),
                          vcolName="MATRIX_ID",
                          vconnection=vconnection)+1
          )
setMethod("getMaxMatrixId",
          signature(vconnection="JDBCConnection"),
          function(vconnection,...)
              getMaxValue(vdatabase=getOption("ResultDatabaseFL"),
                          vtable=getOption("ResultMatrixTableFL"),
                          vcolName="MATRIX_ID",
                          vconnection=vconnection)+1
          )



getMaxValue <- function(vdatabase=getOption("ResultDatabaseFL"),
                        vtable=getOption("ResultVectorTableFL"),
                        vcolName="vectorIdColumn",
                        vconnection=vconnection){
    R <- sqlQuery(connection,
                    paste0("SELECT max(",
                           vcolName,")",
                           " FROM ",
                           getRemoteTableName(vdatabase,
                                              vtable)))
    if(is.na(R)) return(0)
    else return(R)

}

setGeneric("getMaxVectorId", function(vconnection,...) {
    standardGeneric("getMaxVectorId")
})

setMethod("getMaxVectorId",
          signature(vconnection="RODBC"),
          function(vconnection,...)
              getMaxValue(vdatabase=getOption("ResultDatabaseFL"),
                          vtable=getOption("ResultVectorTableFL"),
                          vcolName="vectorIdColumn",
                          vconnection=vconnection)+1
          )
setMethod("getMaxVectorId",
          signature(vconnection="JDBCConnection"),
          function(vconnection,...)
              getMaxValue(vdatabase=getOption("ResultDatabaseFL"),
                          vtable=getOption("ResultVectorTableFL"),
                          vcolName="vectorIdColumn",
                          vconnection=vconnection)+1
          )

ensureQuerySize <- function(pResult,
                            pInput,
                            pOperator,
                            pStoreResult=FALSE,
                            ...)
{
    ##browser()
    if(checkMaxQuerySize(pResult))
    {
        vQuerySizes <- sapply(pInput,
                              FUN=function(x)
                                  object.size(constructSelect(x,...)))
        vbulkyInput <- which.max(vQuerySizes)
        pInput[[vbulkyInput]] <- store(pInput[[vbulkyInput]])
        return(do.call(pOperator,pInput))
    }
    else
    {
        if(pStoreResult) return(store(pResult,...))
        else return(pResult)
    }
}

flag1Check <- function(connection)
{
    return(TRUE)
    if(flag1==0)
    {
        sqlQuery(connection,paste0(" DATABASE ",getOption("ResultDatabaseFL"),"; SET ROLE ALL;"))

        temp <- sqlQuery(connection,
                         paste0(" CREATE TABLE ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),", FALLBACK ,
							     NO BEFORE JOURNAL,
							     NO AFTER JOURNAL,
							     CHECKSUM = DEFAULT,
							     DEFAULT MERGEBLOCKRATIO
							     (
							      MATRIX_ID INTEGER,
							      rowIdColumn INTEGER,
							      colIdColumn INTEGER,
							      valueColumn FLOAT)
				    			 PRIMARY INDEX ( MATRIX_ID, rowIdColumn, colIdColumn);"))

        if(temp!="No Data" || length(temp)!=1)
        {
            sqlQuery(connection,
                     paste0("DROP TABLE ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL"))))

            sqlQuery(connection,
                     paste0("CREATE TABLE ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),", FALLBACK ,
						     NO BEFORE JOURNAL,
						     NO AFTER JOURNAL,
						     CHECKSUM = DEFAULT,
						     DEFAULT MERGEBLOCKRATIO
						     (
						      MATRIX_ID INTEGER,
						      ROW_ID INTEGER,
						      COL_ID INTEGER,
						      CELL_VAL FLOAT)
			    			 PRIMARY INDEX ( MATRIX_ID, ROW_ID, COL_ID );"))
        }
        flag1 <<- 1
    }
}

flag2Check <- function(connection)
{
    return(TRUE)
    if(flag2==0)
    {
        sqlQuery(connection,paste0(" DATABASE ",getOption("ResultDatabaseFL"),"; SET ROLE ALL;"))

        temp <- sqlQuery(connection,
                         paste0(" CREATE TABLE ",getOption("ResultDatabaseFL"),".",getOption("ResultSparseMatrixTableFL"),", FALLBACK ,
							     NO BEFORE JOURNAL,
							     NO AFTER JOURNAL,
							     CHECKSUM = DEFAULT,
							     DEFAULT MERGEBLOCKRATIO
							     (
							      MATRIX_ID INTEGER,
							      ROW_ID INTEGER,
							      COL_ID INTEGER,
							      CELL_VAL FLOAT)
				    			 PRIMARY INDEX ( MATRIX_ID, ROW_ID, COL_ID );"))

        if(temp!="No Data" || length(temp)!=1)
        {
            sqlQuery(connection,
                     paste0("DROP TABLE ",getOption("ResultDatabaseFL"),".",getOption("ResultSparseMatrixTableFL")))

            sqlQuery(connection,
                     paste0("CREATE TABLE ",getOption("ResultDatabaseFL"),".",getOption("ResultSparseMatrixTableFL"),", FALLBACK ,
						     NO BEFORE JOURNAL,
						     NO AFTER JOURNAL,
						     CHECKSUM = DEFAULT,
						     DEFAULT MERGEBLOCKRATIO
						     (
						      MATRIX_ID INTEGER,
						      ROW_ID INTEGER,
						      COL_ID INTEGER,
						      CELL_VAL FLOAT)
			    			 PRIMARY INDEX ( MATRIX_ID, ROW_ID, COL_ID );"))
        }
        flag2 <<- 1
    }
}

flag3Check <- function(connection)
{
    return(TRUE)
    if(flag3==0)
    {
        sqlQuery(connection,paste0(" DATABASE ",getOption("ResultDatabaseFL"),"; SET ROLE ALL;"))

        temp <- sqlQuery(connection,
                         paste0("CREATE TABLE ",getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL"),"
	 					 		 ( VECTOR_ID INT,
	 					 		   VECTOR_INDEX INT,
	 					 		   VECTOR_VALUE VARCHAR(20) )
	 					 		   PRIMARY INDEX (VECTOR_ID, VECTOR_INDEX);" ))
        if(temp != TRUE || length(temp)!=1)
        {
            sqlQuery(connection,
                     paste0("DROP TABLE ",getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL")))

            sqlQuery(connection,
                     paste0("CREATE TABLE ",getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL"),"
	 		 		 		( VECTOR_ID INT,
	 		 		 		  VECTOR_INDEX INT,
	 		 		 		  VECTOR_VALUE VARCHAR(20) )
	 		 		 		  PRIMARY INDEX (VECTOR_ID, VECTOR_INDEX);" ))
        }
        flag3 <<- 1
    }
}

"dimnames<-" <- function(pObject,pDimnames)
{
    UseMethod("dimnames<-", pObject)
}

`dimnames<-.default` <- function(pObject,pDimnames)
{
  op <- .Primitive("dimnames<-")
  op(pObject,pDimnames)
}

`dimnames<-.FLMatrix` <- function(pObject,pDimnames)
{
  connection <- getConnection(pObject)
  mydimnames <- pDimnames
  mydims <- dim(pObject)
  if(mydims[[1]]!=length(pDimnames[[1]]) || mydims[[2]]!=length(pDimnames[[2]]))
  stop(" dimnames not compatible with matrix dimensions")

  remoteTable <- getRemoteTableName(
            getOption("ResultDatabaseFL"),
            getOption("ResultMatrixTableFL"))

  t<-sqlSendUpdate(connection,paste0(" DELETE FROM ",remoteTable,
                              " mtrx ",constructWhere(pObject@select@whereconditions)))
  
  mapTable <- pObject@mapSelect@table_name
  MID <- getVariables(object)$MATRIX_ID
  for(i in 1:length(mydimnames))
      #if(is.character(mydimnames[[i]]))
      {
          mapTable <- getOption("MatrixNameMapTableFL")
          mydimnames[[i]] <- storeVarnameMapping(
              connection,
              mapTable,
              MID,
              i,
              mydimnames[[i]])
      }
  pObject@dimnames <- mydimnames
  print(names(mydimnames[[2]]))

  RESULT <- FLamendDimnames(pObject,mapTable)
  return(RESULT)
}
