# Contains the support functions
#' @include FLMatrix.R
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
sqlSendUpdate <- function(connection,query,...) UseMethod("sqlSendUpdate")

#' Send a query to database
#' Result is returned as data.frame
#' @param channel ODBC/JDBC connection object
#' @param query SQLQuery to be sent
#' @export
sqlQuery <- function(connection,query,...) UseMethod("sqlQuery")

#' Send a query to database
#' Result is returned as data.frame
#' @param channel ODBC/JDBC connection object
#' @param query SQLQuery to be sent
#' @export
sqlStoredProc <- function(connection, query, 
                          outputParameter,
                          ...)
    UseMethod("sqlStoredProc")

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
sqlSendUpdate.JDBCConnection <- function(connection,query,...) {
    sapply(query, function(q){
        ##browser()
        if(getOption("debugSQL")) cat(paste0("SENDING SQL: \n",gsub(" +"," ",q),"\n"))
        tryCatch({
            res <- DBI::dbSendQuery(connection, q, ...)
            ##dbCommit(connection)
            dbClearResult(res)
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
sqlSendUpdate.RODBC <- function(connection,query,...) {
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
sqlStoredProc.RODBC <- function(connection, query, 
                                outputParameter,
                                ...) {
    #browser()
    args <- list(...)
    ## Setting up input parameter value
    pars <- character()
    ai <- 1L
    for(a in args){
        if(is.character(a)){
            if(a=="NULL")
                pars[ai] <- "NULL"
            else
                pars[ai] <- fquote(a)
        } else
            pars[ai] <- a
        ai <- ai+1L
    }
    sqlstr <- paste0("CALL ",query,"(",
                     paste0(pars, collapse=", \n "),
                     ",",
                     paste0(names(outputParameter), collapse=", \n "),
                     ")")
    retobj <- sqlQuery(connection,sqlstr)
    return(retobj)
}

#' @export
sqlStoredProc.JDBCConnection <- function(connection, query, 
                                         outputParameter,
                                         ...) {
    ## http://developer.teradata.com/doc/connectivity/jdbc/reference/current/jdbcug_chapter_2.html
    ## Creating a CallableStatement object, representing
    ## a precompiled SQL statement and preparing the callable
    ## statement for execution.
    args <- list(...)
    query <- paste0("CALL ",query, "(",
                    paste0(rep("?", length(args)+length(outputParameter)),
                           collapse=","),
                     ")")
    cStmt = .jcall(connection@jc,"Ljava/sql/PreparedStatement;","prepareStatement",query)
    ##CallableStatement cStmt = con.prepareCall(sCall);

    ## Setting up input parameter value
    ai <- 1L
    for(a in args){
        if(is.character(a)){
            if(a=="NULL")
                .jcall(cStmt,"V","setNull",ai,.jfield("java/sql/Types",,"VARCHAR"))
            else
                .jcall(cStmt,"V","setString",ai,a)
        } else if(is.integer(a))
            .jcall(cStmt,"V","setInt",ai,as.integer(a))
        else if(is.numeric(a))
            .jcall(cStmt,"V","setFloat",ai,.jfloat(a))
        else if(is.null(a))
            .jcall(cStmt,"V","setNull",ai,.jfield("java/sql/Types",,"VARCHAR"))
        ai <- ai+1L
    }
    ##browser()
    ## Setting up output parameters for data retrieval by
    ## declaring parameter types.
    for(a in outputParameter){
        if(is.character(a))
            a <- .jfield("java/sql/Types",,"VARCHAR")
        else if(is.integer(a))
            a <- .jfield("java/sql/Types",,"BIGINT")
        else if(is.numeric(a))
            a <- .jfield("java/sql/Types",,"FLOAT")
        .jcall(cStmt,"V","registerOutParameter",ai,a)
        ai <- ai+1
    }
    ## Making a procedure call
    exR <- .jcall(cStmt,"I","executeUpdate")
    argOffset <- length(args)
    ai <- 1L
    result <- list()
    for(a in outputParameter){
        if(is.character(a))
            a <- .jcall(cStmt,"S","getString",argOffset+ai)
        else if(is.integer(a))
            a <- .jcall(cStmt,"I","getInt",argOffset+ai)
        else if(is.numeric(a))
            a <- .jcall(cStmt,"F","getFloat",argOffset+ai)
        result[[names(outputParameter)[[ai]]]] <- a
    }
    .jcall(cStmt,"V","close")
    return(as.data.frame(result))
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
                warning(paste0("Use of AnalysisIDQuery is deprecated. Please use sqlStoredProc!\n",query))
                res <- DBI::dbSendQuery(connection, query, ...)
                dbClearResult(res)
            },
            error=function(e) cat(paste0(sqlError(e))))
        resd <- DBI::dbGetQuery(connection,AnalysisIDQuery,...)
        return(resd)
    }
    lapply(query, function(q){
        sqlQuery(connection, q, AnalysisIDQuery,...)
    })
}

#' @export
sqlQuery.RODBC <- function(connection,query,AnalysisIDQuery=NULL, ...) {
    if(!is.null(AnalysisIDQuery))
        warning(paste0("Use of AnalysisIDQuery is deprecated. Please use sqlStoredProc!\n",query))
    if(length(query)==1){
        if(getOption("debugSQL")) cat(paste0("QUERY SQL: \n",query,"\n"))
            resd <- RODBC::sqlQuery(connection, query, as.is=TRUE,...)
            resd <- checkSqlQueryOutput(resd)
            return(resd)
    }
    lapply(query, function(q){
        if(getOption("debugSQL")) cat(paste0("QUERY SQL: \n",q,"\n"))
            resd <- RODBC::sqlQuery(connection, q, as.is=TRUE,...)
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
##' @param host 
##' @param database 
##' @param user 
##' @param passwd 
##' @param jdbc.jarsDir if provided, class paths for jars to be loaded.
##' If one element, a directory, all jar files in that directory will be loaded.
##' If multiple elements, specific jar files, these will be loaded.
##' Issues can occur unless you provide the fully qualified path.
##' @param jdbc.options 
##' @param #"TMODE 
##' @param CHARSET 
##' @param odbcSource 
##' @param driverClass 
##' @param verbose print debugging messages
##' @param ... 
##' @return either an ODBC connection or an JDBC connection
##' @examples
##' connection <- flConnect("jdbc:ncluster://10.47.10.30:2406",
##'                           "fuzzylogix",
##'                          "mroy","mroy",
##'                         c("C:/Users/phani/Downloads/noarch-aster-jdbc-driver.jar",
##'                             "C:/Users/phani/Downloads/noarch-aster-adfs-client.jar"),
##'                         driverClass="com.asterdata.ncluster.Driver")
##' connection <- flConnect("jdbc:teradata://10.200.4.116",
##'                           "FL_DEMO",
##'                          "psrikar","fzzlpass",
##'                         c("C:/Users/phani/Downloads/terajdbc4.jar",
##'                             "C:/Users/phani/Downloads/tdgssconfig.jar"),
##'                         driverClass="com.teradata.jdbc.TeraDriver")
##' connection <- flConnect("jdbc:hive2://192.168.2.179:10000",
##'                          "dblytix",
##'                          "hive","",
##'                         c("C:/Users/phani/Downloads/hive-jdbc-1.2.1-standalone.jar",
##'                             "C:/Users/phani/Downloads/hadoop-common-2.6.0.jar"),
##'                         driverClass="org.apache.hive.jdbc.HiveDriver")
##' @export
flConnect <- function(host=NULL,database=NULL,user=NULL,passwd=NULL,
                      jdbc.jarsDir=NULL,
                      jdbc.options="",# "TMODE=TERA,CHARSET=ASCII",
                      odbcSource=NULL,
                      driverClass=NULL,
                      verbose=FALSE,
                      ...){
    connection <- NULL
    if(!is.null(host) &
       !is.null(database)){
        if(is.null(user)) user <- readline("Your username:")
        if(is.null(passwd)) passwd <- readline("Your password:")
        if(is.null(driverClass)) driverClass <- readline("driverClass")
        if(!grepl("^jdbc:(teradata|aster)://",host)) stop(paste0("host needs to start with 'jdbc:teradata://' or 'jdbc:aster://'."))
        if(is.null(jdbc.jarsDir)) stop("provide fully qualified path to jar files vector.")
        if(is.null(driverClass)) stop("You must provide a jdbc driver class, e.g. com.teradata.jdbc.TeraDriver.")
        myConnect <- function(){
            ## add jdbc driver and security jars to classpath
            require(RJDBC)
            ##browser()
            if(!is.null(jdbc.jarsDir)){
                if(length(jdbc.jarsDir)==1 & dir.exists(jdbc.jarsDir))
                    jdbc.jarsDir <- list.files(jdbc.jarsDir,".*\\.jar",full.names = TRUE,ignore.case = TRUE)
                for(jarF in jdbc.jarsDir){
                    if(verbose)
                        cat(paste0("adding classpath ",jarF,"\n"))
                    .jaddClassPath(jarF)
                }
            }
            Sys.sleep(1)
            require(RJDBC)
            drv <- JDBC(driverClass)
            st <- paste0(host)
            if(!is.null(database))
                st <- paste0(st, "/",database[1], 
                        ifelse(jdbc.options=="",
                                "",
                                paste0("/",jdbc.options)))
            else
                st <- paste0(st,"/",jdbc.options)
            connection <- dbConnect(drv, st, user = user, password = passwd)
            invisible(connection)
        }
        
        ## following connection code takes care of this bug:
        ## need to add class path twice (recurring problem in MAC as of:
        ## http://forums.teradata.com/forum/analytics/connecting-to-teradata-in-r-via-the-teradatar-package
        tryCatch({
            connection <- myConnect()
        },error=function(e)e,
        finally = {
            if(is.null(jdbc.jarsDir))
                jdbc.jarsDir <- readline("Directory of teradata jdbc jar files:")
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
    assign("connection", connection, envir = .GlobalEnv)
    FLStartSession(connection=connection,database=database,...)
    return(connection)
}

    
#' Starts Session and Creates temp Tables for result storage
#'
#' Strongly recommended to run before beginning a new R session
#' use options to specify the following:- 
#' ResultDatabaseFL, ResultVectorTableFL, ResultMatrixTableFL, 
#' NameMapTableFL, ResultSparseMatrixTableFL
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
    options(NameMapTableFL=gen_table_name("tblNameMapping",persistent))
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
        options(FLTempViews=character())
        options(FLTempTables=character())
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
                                   getOption("NameMapTableFL"),"\n",
                                   tableoptions,"\n",
                                "(TABLENAME VARCHAR(100),\n",
                                " MATRIX_ID INTEGER,\n",
                                " DIM_ID INTEGER, -- 1: row, 2: column \n",
                                " NAME VARCHAR(100),\n",
                                " NUM_ID INTEGER)\n",
                                " PRIMARY INDEX (TABLENAME, MATRIX_ID, DIM_ID, NAME);\n"))
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
                        vconnection=getOption("connectionFL"))
{
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
getMaxVectorId <- function(vconnection = getOption("connectionFL"),
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
      if(!grepl("with",tolower(constructSelect(x)))){
        vresult <- tolower(sqlQuery(getOption("connectionFL"),
                            paste0("SELECT TOP 1 TYPE(a.",columnName,
                                    ") \n FROM (",constructSelect(x),
                                    ") a"))[1,1])
        vmapping <- c("VARCHAR","INT","FLOAT","FLOAT")
        vtemp <- as.vector(sapply(c("char","int","float","number"),
                        function(y)
                        return(grepl(y,vresult))))
        vresult <- vmapping[vtemp]
      }
      else vresult <- "FLOAT"
    }
    else{
      vmapping <- c(VARCHAR="character",
                    INT="integer",
                    FLOAT="numeric",
                    INT="logical")
      vresult <- names(vmapping)[vmapping==class(x)]
    }
    if(vresult=="VARCHAR") 
    vresult <- "VARCHAR(255)"
    return(vresult)
}

is.FL <- function(x){
    if(class(x) %in% c("FLMatrix",
                        "FLVector",
                        "FLTable",
                        "FLTableQuery",
                        "FLSelectFrom",
                        "FLTableFunctionQuery"))
    return(TRUE)
    else return(FALSE)
}

is.RSparseMatrix <- function(object){
    vsparseClass <- c("dgCMatrix","dgeMatrix","dsCMatrix",
                    "dgTMatrix","dtrMatrix","pMatrix",
                    "dspMatrix","dtCMatrix","dgRMatrix",
                    "ddiMatrix","dpoMatrix"
                    )
    if(class(object) %in% vsparseClass)
    return(TRUE)
    else
    return(FALSE)
}

checkRemoteTableExistence <- function(databaseName=getOption("ResultDatabaseFL"),
                                    tableName)
{
    vtemp <- sqlQuery(getOption("connectionFL"),paste0(
                        "SELECT 1 FROM dbc.tables \n ",
                        " WHERE databaseName = ",fquote(databaseName),
                        " AND tablename = ",fquote(tableName)))
    if(!is.na(vtemp[1,1]) && vtemp[1,1]==1)
    return(TRUE)
    else return(FALSE)
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
            sqlSendUpdate(connection,
                     paste0("DROP TABLE ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL"))))

            sqlSendUpdate(connection,
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
