# Contains the support functions
NULL
setOldClass("RODBC")

getRemoteTableName <- function(databaseName=getOption("ResultDatabaseFL"),
                               tableName) {

    return(paste0(databaseName,".",tableName))
}

sqlError <- function(e){
    print(e)
    sys.call()
}
################################################################################
######  provide methods for JDBC with same signature as ODBC methods
################################################################################
#' Send a query to database
#' 
#' No result is returned
#' @param channel JDBC connection object
#' @param query SQLQuery to be sent
#' @export
sqlSendUpdate <- function(connection,query) UseMethod("sqlSendUpdate")

#' Send a query to database
#' 
#' Result is returned as data.frame
#' @param channel ODBC/JDBC connection object
#' @param query SQLQuery to be sent
#' @export
sqlQuery <- function(connection,query,...) UseMethod("sqlQuery")
## gk: this made packaging fail here, as I cannot install RODBC, and
## then it is unknown. Can we do a package check? We need to discuss
## this.
## sqlQuery.default <- RODBC::sqlQuery

#' Send a query to database
#' 
#' No result is returned
#' @param channel JDBC connection object
#' @param query SQLQuery to be sent
#' @export
sqlSendUpdate.JDBCConnection <- function(connection,query) {
    sapply(query, function(q){
        ##browser()
        if(getOption("debugSQL")) cat(paste0("SENDING SQL: \n",gsub(" +"," ",q),"\n"))
        tryCatch({
            R <- RJDBC::dbSendUpdate(connection,q)
            ##dbCommit(connection)
            return(R)
        },
        error=function(e) sqlError(e))
    })
}

#' Send a query to database
#' 
#' No result is returned
#' @param channel ODBC connection object
#' @param query SQLQuery to be sent
#' @export
sqlSendUpdate.RODBC <- function(connection,query) {
    RODBC::odbcSetAutoCommit(connection, autoCommit = FALSE)
    sapply(query, function(q){
        if(getOption("debugSQL")) cat(paste0("SENDING SQL: \n",gsub(" +"," ",q),"\n"))
        err<-RODBC::sqlQuery(connection,q,errors=FALSE)
        errmsg<- RODBC::odbcGetErrMsg(connection)
        if(length(errmsg) == 0 || as.character(errmsg)=="No Data")
        {
            RODBC::odbcEndTran(connection, commit = TRUE)
        }
        else
        {
            RODBC::odbcEndTran(connection, commit = FALSE)
            print(errmsg)
        }
        RODBC::odbcClearError(connection)
    })
    RODBC::odbcSetAutoCommit(connection, autoCommit = TRUE)
    #cat("DONE...\n")
}

#' @export
sqlQuery.JDBCConnection <- function(connection,query, AnalysisIDQuery=NULL, ...) {
    if(length(query)==1){
        if(getOption("debugSQL")) cat(paste0("QUERY SQL: \n",query,"\n"))
        if(is.null(AnalysisIDQuery))
            tryCatch({
                resd <- DBI::dbGetQuery(connection, query, ...)
                return(resd)
            },
            error=function(e) cat(paste0(sqlError(e))))
        else
            tryCatch({
                DBI::dbSendQuery(connection, query, ...)
                resd <- DBI::dbGetQuery(connection,AnalysisIDQuery,...)
                return(resd)
            },
            error=function(e) cat(paste0(sqlError(e))))
    }
    lapply(query, function(q){
        if(getOption("debugSQL")) cat(paste0("QUERY SQL: \n",q,"\n"))
        tryCatch({
            resd <- DBI::dbGetQuery(connection, q, ...)
            return(resd)
        },
        error=function(e) cat(paste0(sqlError(e))))
    })
}

#' @export
sqlQuery.RODBC <- function(connection,query,AnalysisIDQuery=NULL, ...) {
    if(length(query)==1){
        if(getOption("debugSQL")) cat(paste0("QUERY SQL: \n",query,"\n"))
            resd <- RODBC::sqlQuery(connection, query, ...)
            resd <- checkSqlQueryOutput(resd)
            return(resd)
    }
    lapply(query, function(q){
        if(getOption("debugSQL")) cat(paste0("QUERY SQL: \n",q,"\n"))
            resd <- RODBC::sqlQuery(connection, q, ...)
            resd <- checkSqlQueryOutput(resd)
            return(resd)
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
    where_clause
}

                                        # /**
                                        #  * Converts List to class Spec. Used for Data Prep
                                        #  * @param  {list} x e.g. list(Varx="a",Vary="b")
                                        #  * @return {string}   "Varx(x), Vary(y)"
                                        #  */

list_to_class_spec <- function (x) {
    classSpec <- paste0(names(x),"(",x,")",collapse=", ")
    if(nchar(classSpec) > 1) {
        classSpec <- classSpec
    } else {
        classSpec <- ""
    }
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
    #random_no <- rnorm(1);
    vtbl <- paste0("ARBase",TableName,"D",round(as.numeric(Sys.time())))
    options("FLTempTables"=c(getOption("FLTempTables"),vtbl))
    vtbl
}

trim <- function( x ) {
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

gen_score_table_name <- function(TableName){
    #random_no <- rnorm(1);
    vtbl <- paste0("ARBase",TableName,"S",round(as.numeric(Sys.time())))
    options("FLTempTables"=c(getOption("FLTempTables"),vtbl))
    vtbl
}

gen_wide_table_name <- function(TableName){
    #random_no <- rnorm(1);
    vtbl <- paste0("ARBase",TableName,"W",round(as.numeric(Sys.time())))
    options("FLTempTables"=c(getOption("FLTempTables"),vtbl))
    vtbl
}

gen_unique_table_name <- function(TableName){
    #random_no <- rnorm(1);
    vtbl <- paste0("ARBase",TableName,"U",round(as.numeric(Sys.time())))
    options("FLTempTables"=c(getOption("FLTempTables"),vtbl))
    vtbl
}

gen_view_name <- function(TableName=""){
    #random_no <- rnorm(1);
    vtbl <- paste0("ARBase",TableName,"V",round(as.numeric(Sys.time())))
    options("FLTempViews"=c(getOption("FLTempViews"),vtbl))
    vtbl
}

genRandVarName <- function(){
    vrnum <- rnorm(1)
    vrnum <- round(vrnum*vrnum*1000)
    vtime <- round(as.numeric(Sys.time()))
    return(paste0(sample(letters[1:26],1),vrnum,sample(letters[1:26],1),vtime))
}

genSessionID <- function(){
    vtbl <- paste0("ARBase",round(as.numeric(Sys.time())))
    options("FLSessionID"=vtbl)
    vtbl
}

genAnalysisIDQuery <- function(pTable,pNote)
{
    paste0("SELECT top 1 ANALYSISID from ",pTable,
        " where Note=",fquote(pNote)," order by RUNENDTIME DESC")
}

genNote <- function(pFunction){
    paste0(pFunction," from ",getOption("FLSessionID"))
}
#' Close Session and Drop temp Tables
#'
#' Strongly recommended to run before quitting current R session
#' @param connection ODBC/JDBC connection object
#' @export
FLodbcClose <- function(connection)
{
    sqlstr <- c()

    if(length(getOption("FLTempTables"))>0)
	sqlstr <- c(sqlstr,paste0("DROP TABLE ",getOption("FLTempTables"),";"))
    if(length(getOption("FLTempViews"))>0)
    sqlstr <- c(sqlstr,paste0("DROP VIEW ",getOption("FLTempViews"),";"))

    sqlSendUpdate(connection,sqlstr)
    if(class(connection)=="RODBC")
    RODBC::odbcClose(connection)
    else RJDBC::dbDisconnect(connection)
    options(flag1=0)
    options(flag1=0)
    options(flag1=0)
    options("FLTempTables"=c())
    options("FLTempViews"=c())
    options("FLSessionID"=c())
}

gen_table_name <- function(prefix,suffix){
    vtbl <- ifelse(is.null(suffix),
                   paste0(prefix),
                   paste0(prefix,"_",suffix))
    options("FLTempTables"=c(getOption("FLTempTables"),vtbl))
    vtbl
}

##' Creates either an ODBC connection or an JDBC connection and initializes
##' the FL session tables.
##'
##' @param odbcSource 
##' @param host 
##' @param database 
##' @param user 
##' @param passwd 
##' @param dir.jdbcjars if provided, class paths for tdgssconfig.jar and terajdbc4.jar in that dir are loaded.  Issues can occur unless you provide the fully qualified path.
##' @param ... 
##' @return either an ODBC connection or an JDBC connection
##' @export
flConnect <- function(host=NULL,database=NULL,user=NULL,passwd=NULL,
                      dir.jdbcjars=NULL,
                      odbcSource=NULL,
                      ...){
    connection <- NULL
    if(!is.null(host) &
       !is.null(database)){
        if(is.null(user)) user <- readline("Your username:")
        if(is.null(passwd)) passwd <- readline("Your password:")
        myConnect <- function(){
            ## add jdbc driver and security jars to classpath
            require(RJDBC)
            if(!is.null(dir.jdbcjars)){
                cat(paste0("adding classpath ",dir.jdbcjars,"/terajdbc4.jar and ",
                           dir.jdbcjars,"/tdgssconfig.jar\n"))
                .jaddClassPath(paste0(dir.jdbcjars,"/terajdbc4.jar"))
                .jaddClassPath(paste0(dir.jdbcjars,"/tdgssconfig.jar"))
            }
            Sys.sleep(1)
            require(teradataR)
            tdConnect(dsn=host,uid=user,pwd=passwd,database=database,dType="jdbc")
        }

        ## following connection code takes care of this bug:
        ## need to add class path twice (recurring problem in MAC as of:
        ## http://forums.teradata.com/forum/analytics/connecting-to-teradata-in-r-via-the-teradatar-package
        tryCatch({
            connection <- myConnect()
        },error=function(e)e,
        finally = {
            if(is.null(dir.jdbcjars))
                dir.jdbcjars <- readline("Directory of teradata jdbc jar files:")
            ##Sys.sleep(3)
            connection <- myConnect()
        })
    } else if (!is.null(odbcSource)){
        require(RODBC)
        tryCatch({
            connection <- odbcConnect(odbcSource)
            },error=function(e)e)
    }
    if(is.null(connection))
        stop("Please provide either odbcSource for connecting to an ODBC source; or provide host, database, user, passwd for connecting to JDBC")
    
    if(is.null(database))
    {
      cat(" setting FL_DEMO as ResultDatabaseFL ")
      database <- "FL_DEMO"
    }
    FLStartSession(connection=connection,database=database,...)
    return(connection)
}

    
#' Starts Session and Creates temp Tables for result storage
#'
#' Strongly recommended to run before beginning a new R session
#' use options to specify the following:- 
#' ResultDatabaseFL, ResultVectorTableFL, ResultMatrixTableFL, 
#' MatrixNameMapTableFL, ResultSparseMatrixTableFL
#' @param connection ODBC/JDBC connection object
#' @param database name of current database
#' @param persistent NULL if result tables are to be created as volatile tables
#' @param drop logical to specify to drop result tables if already existing
#' @param tableoptions options used to create result tables
#' @export
FLStartSession <- function(connection,
                           database="FL_DEMO",
                           persistent="test",
                           drop=TRUE,
                           debug=FALSE,
                           tableoptions=paste0(", FALLBACK ,NO BEFORE JOURNAL,NO AFTER JOURNAL,CHECKSUM = DEFAULT,DEFAULT MERGEBLOCKRATIO "))
{
    options(debugSQL=debug)
    options(ResultDatabaseFL=database)
    ##    browser()
    options(connectionFL=connection)
    options(InteractiveFL=TRUE)
    options(ResultVectorTableFL=gen_table_name("tblVectorResult",persistent))
    options(ResultMatrixTableFL=gen_table_name("tblMatrixMultiResult",persistent))
    options(ResultSparseMatrixTableFL=gen_table_name("tblMatrixMultiSparseResult",persistent))
    options(MatrixNameMapTableFL=gen_table_name("tblMatrixNameMapping",persistent))
    options(ResultCharVectorTableFL=gen_table_name("tblCharVectorResult",persistent))
    options(ResultCharMatrixTableFL=gen_table_name("tblCharMatrixMultiResult",persistent))
    options(ResultIntMatrixTableFL=gen_table_name("tblIntMatrixMultiResult",persistent))
    options(ResultIntVectorTableFL=gen_table_name("tblIntVectorResult",persistent))

    options(scipen=999)
    #options(stringsAsFactors=FALSE)
    sendqueries <- c(
        paste0("DATABASE ",getOption("ResultDatabaseFL"),";"),
        paste0("SET ROLE ALL;"))
    sqlSendUpdate(connection, sendqueries)

    if(drop){
    	sqlstr <- c()

    	if(length(getOption("FLTempTables"))>0)
            sqlstr <- c(sqlstr,paste0("DROP TABLE ",getOption("FLTempTables"),";"))
        if(length(getOption("FLTempViews"))>0)
            sqlstr <- c(sqlstr,paste0("DROP VIEW ",getOption("FLTempViews"),";"))

        sqlSendUpdate(connection,sqlstr)
    }

    sendqueries <- c(genCreateResulttbl(tablename=getOption("ResultMatrixTableFL"),
                                        persistent=persistent,
                                        tableoptions=tableoptions),
                    genCreateResulttbl(tablename=getOption("ResultSparseMatrixTableFL"),
                                        persistent=persistent,
                                        tableoptions=tableoptions),
                    genCreateResulttbl(tablename=getOption("ResultCharMatrixTableFL"),
                                        persistent=persistent,
                                        tableoptions=tableoptions,
                                        type=" VARCHAR(100) "),
                    genCreateResulttbl(tablename=getOption("ResultIntMatrixTableFL"),
                                        persistent=persistent,
                                        tableoptions=tableoptions,
                                        type=" INTEGER "),
                    genCreateResulttbl(tablename=getOption("ResultVectorTableFL"),
                                        vclass="vector",
                                        persistent=persistent,
                                        tableoptions=tableoptions),
                    genCreateResulttbl(tablename=getOption("ResultCharVectorTableFL"),
                                        vclass="vector",
                                        persistent=persistent,
                                        tableoptions=tableoptions,
                                        type=" VARCHAR(100) "),
                    genCreateResulttbl(tablename=getOption("ResultIntVectorTableFL"),
                                        vclass="vector",
                                        persistent=persistent,
                                        tableoptions=tableoptions,
                                        type=" INTEGER "),
                    paste0(" CREATE ",ifelse(is.null(persistent),
                                            "VOLATILE TABLE ",
                                            "TABLE "),
                                   getOption("MatrixNameMapTableFL"),"\n",
                                   tableoptions,"\n",
                                "(TABLENAME VARCHAR(100),\n",
                                " MATRIX_ID INTEGER,\n",
                                " DIM_ID INTEGER, -- 1: row, 2: column \n",
                                " NAME VARCHAR(100),\n",
                                " NUM_ID INTEGER)\n",
                                " PRIMARY INDEX (TABLENAME, MATRIX_ID, DIM_ID, NAME);\n"))
   #  sendqueries <- c(
   #      paste0(" CREATE ",ifelse(is.null(persistent),"VOLATILE TABLE ","TABLE "),
   #             getOption("ResultMatrixTableFL"),
   #             tableoptions,
   #             "     (
			# 	      MATRIX_ID INTEGER,
			# 	      rowIdColumn INTEGER,
			# 		  colIdColumn INTEGER,
			# 		  valueColumn FLOAT)
	  #   			 PRIMARY INDEX ( MATRIX_ID, rowIdColumn, colIdColumn );"),
   #      paste0(" CREATE ",ifelse(is.null(persistent),"VOLATILE TABLE ","TABLE "),
   #             getOption("MatrixNameMapTableFL"),
   #             tableoptions,
   #             "     (
			# TABLENAME VARCHAR(100),
			# MATRIX_ID INTEGER,
   #                      DIM_ID INTEGER, -- 1: row, 2: column
			# NAME VARCHAR(100),
			# NUM_ID INTEGER)
	  #   		PRIMARY INDEX (TABLENAME, MATRIX_ID, DIM_ID, NAME);"),
   #      paste0(" CREATE ",ifelse(is.null(persistent),"VOLATILE TABLE ","TABLE "),
   #             getOption("ResultSparseMatrixTableFL"),
   #             tableoptions,
   #             "
			# 	     (
			# 	      MATRIX_ID INTEGER,
			# 	      rowIdColumn INTEGER,
			# 		  colIdColumn INTEGER,
			# 		  valueColumn FLOAT)
	  #   			 PRIMARY INDEX ( MATRIX_ID, rowIdColumn, colIdColumn );"),
   #      paste0(" CREATE ",ifelse(is.null(persistent),"VOLATILE TABLE ","TABLE "),
   #             getOption("ResultVectorTableFL"),
   #             tableoptions,
   #             "
			#  		 ( vectorIdColumn INT,
			#  		   vectorIndexColumn INT,
			# 	 	   vectorValueColumn FLOAT )
			#  		   PRIMARY INDEX (vectorIdColumn,vectorIndexColumn);"),
   #      paste0(" CREATE ",ifelse(is.null(persistent),"VOLATILE TABLE ","TABLE "),
   #             getOption("ResultCharVectorTableFL"),
   #             tableoptions,
   #             "
   #                   ( vectorIdColumn INT,
   #                     vectorIndexColumn INT,
   #                     vectorValueColumn VARCHAR(100) )
   #                     PRIMARY INDEX (vectorIdColumn,vectorIndexColumn);"))
    sqlSendUpdate(connection, sendqueries)

    genSessionID()
    cat("Session Started..\n")
}

genCreateResulttbl <- function(tablename,vclass="matrix",type="FLOAT",
                            persistent="test",
                            tableoptions=paste0(", FALLBACK ,NO BEFORE JOURNAL,",
                                "NO AFTER JOURNAL,CHECKSUM = DEFAULT,",
                                "DEFAULT MERGEBLOCKRATIO ")){
    if(vclass=="matrix"){
        return(paste0(" CREATE ",ifelse(is.null(persistent),
                        "VOLATILE TABLE ","TABLE "),"\n",
                        tablename,"\n",
                        tableoptions,
                        " ( MATRIX_ID INTEGER,\n",
                        " rowIdColumn INTEGER,\n",
                        " colIdColumn INTEGER,\n",
                        " valueColumn ",type,")\n",
                        " PRIMARY INDEX ( MATRIX_ID, rowIdColumn, colIdColumn );\n"))
    }
    else if(vclass=="vector"){
        return(paste0(" CREATE ",ifelse(is.null(persistent),
                        "VOLATILE TABLE ","TABLE "),"\n",
                        tablename,"\n",
                        tableoptions,
                        "( vectorIdColumn INT,\n",
                        " vectorIndexColumn INT,\n",
                        " vectorValueColumn ",type," )\n",
                        " PRIMARY INDEX (vectorIdColumn,vectorIndexColumn);\n"))
    }
}
getMaxId <- function(vdatabase,vtable,vcolName,
                     vconnection=getOption("connectionFL"),...){
    sqlstr <- paste0(" SELECT MAX(",vcolName,
                     " )+1 FROM ",vdatabase,".",vtable)

    t <- sqlQuery(vconnection,sqlstr)[1,1]
    if(is.na(t)) return(0)
    else return(t)
}

#' Get Max Matrix ID+1 from result Matrix table
#'
#' used to know ID of next entry in table
#' @param vconnection ODBC/JDBC connection object
getMaxMatrixId <- function(vconnection=getOption("connectionFL"),
                            vtable=getOption("ResultMatrixTableFL"),
                            ...)
    getMaxValue(vdatabase=getOption("ResultDatabaseFL"),
                vtable=vtable,
                vcolName="MATRIX_ID",
                vconnection=vconnection)+1


#' Get Max ID from given table
#'
#' used to know ID of last entry in table
#' @param vconnection ODBC/JDBC connection object
#' @param vtable name of the table
#' @param vdatabase name of the database of table
#' @param vcolName name of the primary index column in table

getMaxValue <- function(vdatabase=getOption("ResultDatabaseFL"),
                        vtable=getOption("ResultVectorTableFL"),
                        vcolName="vectorIdColumn",
                        vconnection=vconnection){
    R <- sqlQuery(vconnection,
                    paste0("SELECT max(",
                           vcolName,")",
                           " FROM ",
                           getRemoteTableName(vdatabase,
                                              vtable)))[1,1]
    if(is.na(R)) return(0)
    else return(R)

}

#' Get Max Vector ID+1 from result Vector table
#'
#' used to know ID of next entry in table
#' @param vconnection ODBC/JDBC connection object
getMaxVectorId <- function(vconnection=getOption("connectionFL"),
                        vtable=getOption("ResultVectorTableFL"),...)
    getMaxValue(vdatabase=getOption("ResultDatabaseFL"),
                vtable=vtable,
                vcolName="vectorIdColumn",
                vconnection=vconnection)+1

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

checkYorN <- function(pInput)
{
    pInput <- toupper(pInput)
    if(pInput=="N") return(FALSE)
    else if(pInput=="Y") return(TRUE)
    else stop("invalid input. Expected y or n")
}

checkSqlQueryOutput <- function(pObject)
{
    if(!is.data.frame(pObject) && length(pObject)==2 && is.character(pObject))
    stop("Error in Query:-",pObject[1])
    else return(pObject)
}

fquote <- function(pname) return(paste0("'",pname,"'"))

checkValidFormula <- function(pObject,pData)
{
    if(class(pObject)!="formula")
    stop("invalid formula object")
    vallVars <- base::all.vars(pObject)
    vcolnames <- colnames(pData)
    sapply(vallVars,function(x)
        if(!(x %in% vcolnames))
        stop(x," not in colnames of data\n"))
}

flag1Check <- function(connection)
{
    return(TRUE)
    if(getOption("flag1")==0)
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
        options(flag1=1)
    }
}

flag2Check <- function(connection)
{
    return(TRUE)
    if(getOption("flag2")==0)
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
        options(flag2=1)
    }
}

flag3Check <- function(connection)
{
    return(TRUE)
    if(getOption("flag3")==0)
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
        options(flag3=1)
    }
}
