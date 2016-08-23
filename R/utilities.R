# Contains the support functions
#' @include FLMatrix.R
NULL


setOldClass("RODBC")


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
    verrflag<-sapply(query, function(q){
                            ##browser()
                            if(getOption("debugSQL")) cat(paste0("SENDING SQL: \n",gsub(" +"," ",q),"\n"))
                            tryCatch({
                                if(is.TDAster())
                                    res <- RJDBC::dbSendUpdate(connection,q,...)
                                else{
                                    res <- DBI::dbSendQuery(connection, q, ...)
                                    ##dbCommit(connection)
                                    dbClearResult(res)
                                }
                                return(TRUE)
                            },
                            error=function(e){
                                sqlError(e)
                                return(FALSE)
                                })
                        })
    return(verrflag)
}

#' Send a query to database
#' 
#' No result is returned
#' @param channel ODBC connection object
#' @param query SQLQuery to be sent
#' @export
sqlSendUpdate.RODBC <- function(connection,query,...){
    if(!is.TDAster())
    RODBC::odbcSetAutoCommit(connection, autoCommit = FALSE)
    else RODBC::odbcSetAutoCommit(connection, autoCommit = TRUE)

    verrflag <- sapply(query, function(q){
                                if(getOption("debugSQL")) cat(paste0("SENDING SQL: \n",gsub(" +"," ",q),"\n"))
                                err<-RODBC::sqlQuery(connection,q,errors=FALSE)
                                errmsg<- RODBC::odbcGetErrMsg(connection)
                                if(length(errmsg) == 0 || as.character(errmsg)=="No Data")
                                {
                                    RODBC::odbcEndTran(connection, commit = TRUE)
                                    verrflag <- TRUE
                                }
                                else
                                {
                                    RODBC::odbcEndTran(connection, commit = FALSE)
                                    print(errmsg)
                                    verrflag <- FALSE
                                }
                                RODBC::odbcClearError(connection)
                                return(verrflag)
                            })
    RODBC::odbcSetAutoCommit(connection, autoCommit = TRUE)
    return(verrflag)
    #cat("DONE...\n")
}

#' @export
sqlStoredProc.RODBC <- function(connection, query, 
                                outputParameter,
                                ...) {
    #browser()
    sqlstr <- constructStoredProcSQL(pConnection=connection,
                                    pFuncName=query,
                                    pOutputParameter=outputParameter,
                                    ...)
    # args <- list(...)
    # ## Setting up input parameter value
    # pars <- character()
    # ai <- 1L
    # for(a in args){
    #     if(is.character(a)){
    #         if(a=="NULL")
    #             pars[ai] <- "NULL"
    #         else
    #             pars[ai] <- fquote(a)
    #     } else
    #         pars[ai] <- a
    #     ai <- ai+1L
    # }
    # sqlstr <- paste0("CALL ",query,"(",
    #                  paste0(pars, collapse=", \n "),
    #                  ",",
    #                  paste0(names(outputParameter), collapse=", \n "),
    #                  ")")
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
    if("pInputParams" %in% names(args))
        args <- args[["pInputParams"]]
    else if(length(args)==1 && is.list(args[[1]]))
        args <- args[[1]]
    # query <- paste0("CALL ",query, "(",
    #                 paste0(rep("?", length(args)+length(outputParameter)),
    #                        collapse=","),
    #                  ")")
    query <- constructStoredProcSQL(pConnection=connection,
                                    pFuncName=query,
                                    pOutputParameter=outputParameter,
                                    ...)

    if(getOption("debugSQL")) cat(paste0("CALLING Stored Pro: \n",gsub(" +"," ",query),"\n"))
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
        else {
            tryCatch({
                warning(paste0("Use of AnalysisIDQuery is deprecated. Please use sqlStoredProc!\n",query))
                res <- DBI::dbSendQuery(connection, query, ...)
                dbClearResult(res)
            },
            error=function(e) cat(paste0(sqlError(e))))
            resd <- DBI::dbGetQuery(connection,AnalysisIDQuery,...)
            return(resd)
        }
    } else
    lapply(query, function(q){
        sqlQuery(connection, q, AnalysisIDQuery,...)
    })
}

#' @export
sqlQuery.RODBC <- function(connection,query,AnalysisIDQuery=NULL, ...) {
    if(is.TDAster())
        RODBC::odbcSetAutoCommit(connection, autoCommit = TRUE)

    if(!is.null(AnalysisIDQuery))
        warning(paste0("Use of AnalysisIDQuery is deprecated. Please use sqlStoredProc!\n",query))
    if(length(query)==1){
        if(getOption("debugSQL")) cat(paste0("QUERY SQL: \n",query,"\n"))
            resd <- RODBC::sqlQuery(connection, query,...)
            resd <- checkSqlQueryOutput(resd)
            return(resd)
    }
    lapply(query, function(q){
        if(getOption("debugSQL")) cat(paste0("QUERY SQL: \n",q,"\n"))
            resd <- RODBC::sqlQuery(connection, q,...)
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

FLGenTableName <- function(pTableName,
                            pCode){
    pTableName <- removeAlias(pTableName)
    vtbl <- paste0("ARBase",pTableName,pCode,round(as.numeric(Sys.time())))
    #options("FLTempTables"=c(getOption("FLTempTables"),vtbl))
    vtbl
}
gen_deep_table_name <- function(TableName){
    return(FLGenTableName(pTableName=TableName,
                        pCode="D"))
}

trim <- function( x ) {
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

gen_score_table_name <- function(TableName){
    return(FLGenTableName(pTableName=TableName,
                        pCode="S"))
}

gen_wide_table_name <- function(TableName){
    return(FLGenTableName(pTableName=TableName,
                        pCode="W"))
}

gen_unique_table_name <- function(TableName){
    return(FLGenTableName(pTableName=TableName,
                        pCode="U"))
}

gen_view_name <- function(TableName=""){
   return(FLGenTableName(pTableName=TableName,
                        pCode="V"))
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
   # if(length(getOption("FLTempTables"))>0)
   #      sapply(getOption("FLTempTables"),dropTable)
   #  if(length(getOption("FLTempViews"))>0)
   #      sapply(getOption("FLTempViews"),dropView)

    if(class(connection)=="RODBC")
    RODBC::odbcClose(connection)
    else RJDBC::dbDisconnect(connection)
    options(flag1=0)
    options(flag1=0)
    options(flag1=0)
    #options("FLTempTables"=c())
    #options("FLTempViews"=c())
    options("FLSessionID"=c())
}

gen_table_name <- function(prefix,suffix=NULL){
    vtbl <- ifelse(is.null(suffix),
                   paste0(prefix),
                   paste0(prefix,"_",suffix))
    #options("FLTempTables"=c(getOption("FLTempTables"),vtbl))
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
##' connection <- flConnect("jdbc:teradata://xx.xxx.x.xxx",
##'                           "FL_DEMO",
##'                          "UserName","PassWord",
##'                         c("C:/Users/xxx/terajdbc4.jar",
##'                             "C:/Users/xxx/tdgssconfig.jar"),
##'                         driverClass="com.teradata.jdbc.TeraDriver")
##' connection <- flConnect("jdbc:hive2://xx.xxx.x.xxx",
##'                          "dblytix",
##'                          "userName","Password",
##'                         c("C:/Users/xxx/hive-jdbc-1.2.1-standalone.jar",
##'                             "C:/Users/xxx/hadoop-common-2.6.0.jar"),
##'                         driverClass="org.apache.hive.jdbc.HiveDriver")
##' connection <- flConnect("jdbc:ncluster://xx.xxx.x.xxx","fuzzylogix",
##'                          "UserName","Password",
##'                         c("C:/Users/xxx/noarch-aster-jdbc-driver.jar",
##'                           "C:/Users/xxx/noarch-aster-adfs-client.jar"),
##'                         driverClass = "com.asterdata.ncluster.Driver")
##' connection <- flConnect(odbcSource="sourceName",database="FL_DEMO",platform="TD")
##' connection <- flConnect(odbcSource="SorceName",database="fuzzylogix",platform="TDAster")
##' @export
flConnect <- function(host=NULL,database=NULL,user=NULL,passwd=NULL,
                      jdbc.jarsDir=NULL,
                      jdbc.options="",# "TMODE=TERA,CHARSET=ASCII",
                      odbcSource=NULL,
                      driverClass=NULL,
                      verbose=FALSE,
                      ...){
    getPlatform <- function(pdrvClass,pDotsList){
        #browser()
        matchPlatform <- function(pObj1){
            pObj2 <- list(c("TD","teradata","com.teradata.jdbc.TeraDriver"),
                          c("TDAster","aster","astertd",
                            "teradataaster","com.asterdata.ncluster.Driver"),
                          c("Hadoop","hive","cloudera",
                            "clouderahive","hive2","org.apache.hive.jdbc.HiveDriver"))
            return(sapply(pObj2,
                        function(i){
                            if(tolower(pObj1) %in% tolower(i))
                            return(as.vector(i[[1]]))
                            else return(NULL)
                            }))
        }
        vplatform <- NULL
        if(!is.null(pdrvClass))
            vplatform <- matchPlatform(pdrvClass)
        else if("platform" %in% names(pDotsList))
            vplatform <- matchPlatform(pDotsList$platform)
        vplatform <- as.character(vplatform[sapply(vplatform,
                                                    function(x)
                                                        !is.null(x))])
        if(length(vplatform)==0)
            stop("invalid driverClass or platform argument in flConnect \n ")
        return(vplatform)
    }

    options(ResultDatabaseFL=database)
    options(FLUsername=user)
    connection <- NULL

    if(!is.null(host)){
        if(is.null(user)) user <- readline("Your username:  ")
        if(is.null(passwd)) passwd <- readline("Your password:  ")
        # if(is.null(driverClass)) driverClass <- readline("driverClass:  ")
        if(is.null(jdbc.jarsDir)) stop("provide fully qualified path to jar files vector \n ")
        if(is.null(driverClass)){
            getDriverClass <- function(pHost){
                vdrvClasses <- c(teradata="com.teradata.jdbc.TeraDriver",
                                ncluster="com.asterdata.ncluster.Driver",
                                hive2="org.apache.hive.jdbc.HiveDriver")
                vindex <- sapply(names(vdrvClasses),
                                function(x) grepl(x,pHost))
                return(vdrvClasses[vindex])
            }
            driverClass <- getDriverClass(host)
        }


        if(!grepl("^jdbc:",host)) stop(paste0("host needs to start with 'jdbc:' \n "))

        myConnect <- function(){
            ## add jdbc driver and security jars to classpath
            #browser()
            require(RJDBC)
            ##browser()
            if(!is.null(jdbc.jarsDir)){
                if(length(jdbc.jarsDir)==1)
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
    

    vplatform <- getPlatform(pdrvClass=driverClass,
                            pDotsList=list(...))
    options(FLPlatform=vplatform)
    options(connectionFL              = connection)
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
#' @param temporaryTables TRUE if result tables are to be created as volatile tables
#' @param drop logical to specify to drop result tables if already existing
#' @param tableoptions options used to create result tables
#' @export
FLStartSession <- function(connection,
                           database=getOption("ResultDatabaseFL"),
                           temporaryTables=TRUE,
                           drop=FALSE,
                           debug=FALSE,
                           tableoptions=NULL,
                           ...)
{
    options(debugSQL=debug)
    if(is.null(database))
        stop("database argument cannot be NULL \n ")
    #if(tolower(getOption("ResultDatabaseFL"))!=tolower(database))
    setCurrentDatabase(database)
    options(ResultDatabaseFL=database)

    ## Drop Any Tables overSplling from previous unclosed Session
    # if(drop){
    #     if(length(getOption("FLTempTables"))>0)
    #         sapply(getOption("FLTempTables"),dropTable)
    #     if(length(getOption("FLTempViews"))>0)
    #         sapply(getOption("FLTempViews"),dropView)
    #     options(FLTempViews=character())
    #     options(FLTempTables=character())
    # }
    #browser()
    options(InteractiveFL             = TRUE)
    options(temporaryTablesFL         = temporaryTables)
    options(ResultVectorTableFL       = gen_table_name("tblVectorResult"))
    options(ResultMatrixTableFL       = gen_table_name("tblMatrixMultiResult"))
    options(ResultSparseMatrixTableFL = gen_table_name("tblMatrixMultiSparseResult"))
    options(NameMapTableFL            = gen_table_name("tblNameMapping"))
    options(ResultCharVectorTableFL   = gen_table_name("tblCharVectorResult"))
    options(ResultCharMatrixTableFL   = gen_table_name("tblCharMatrixMultiResult"))
    options(ResultIntMatrixTableFL    = gen_table_name("tblIntMatrixMultiResult"))
    options(ResultIntVectorTableFL    = gen_table_name("tblIntVectorResult"))

    options(scipen=999)
    #options(stringsAsFactors=FALSE)

    vresultTables <- c("ResultMatrixTableFL","ResultSparseMatrixTableFL",
                        "ResultCharMatrixTableFL","ResultIntMatrixTableFL",
                        "ResultVectorTableFL","ResultCharVectorTableFL",
                        "ResultIntVectorTableFL")
    sapply(vresultTables,
        function(x){
            vtable <- getOption(x)
            if(grepl("matrix",tolower(vtable)))
            vclass <- "matrix"
            else vclass <- "vector"
            if(grepl("int",tolower(vtable)))
            vtype <- "INT"
            else if(grepl("char",tolower(vtable)))
            vtype <- "VARCHAR(100)"
            else vtype <- "FLOAT"
            genCreateResulttbl(tablename=vtable,
                                temporaryTable=temporaryTables,
                                tableoptions=tableoptions,
                                vclass=vclass,
                                type=vtype,
                                pDrop=drop)
        })

    ## Create names mapping table
    createTable(pTableName=getOption("NameMapTableFL"),
                pColNames=c("TABLENAME","MATRIX_ID",
                            "DIM_ID","NAME","NUM_ID"),
                pColTypes=c("VARCHAR(100)","INT",
                            "INT","VARCHAR(100)",
                            "INT"),
                pTableOptions=tableoptions,
                pPrimaryKey=c("TABLENAME","MATRIX_ID",
                            "DIM_ID","NAME"),
                pTemporary=temporaryTables,
                pDrop=drop)

    ## Create system table for TablesMetadataInfo
    createTable(pTableName="fzzlAdapteRTablesInfo",
                pColNames=c("TimeInfo","DateInfo",
                            "UserName","DatabaseName",
                            "TableName","ElementID",
                            "ObjType",
                            "Comments"),
                pColTypes=c("VARCHAR(100)","VARCHAR(100)",
                            "VARCHAR(100)","VARCHAR(100)",
                            "VARCHAR(100)","INT","VARCHAR(100)",
                            "VARCHAR(100)"),
                pTableOptions=tableoptions,
                pPrimaryKey="UserName",
                pTemporary=FALSE,
                pDrop=FALSE)

    genSessionID()
    cat("Session Started..\n")
}

genCreateResulttbl <- function(tablename,
                               temporaryTable=TRUE,
                               tableoptions=NULL,
                               vclass,
                               type,
                               pDrop){
    if(vclass=="matrix"){
        createTable(pTableName=tablename,
                    pColNames=c("MATRIX_ID","rowIdColumn",
                                "colIdColumn","valueColumn"),
                    pColTypes=c("INT","INT",
                                "INT",type),
                    pTableOptions=tableoptions,
                    pPrimaryKey=c("MATRIX_ID",
                                "rowIdColumn","colIdColumn"),
                    pTemporary=temporaryTable,
                    pDrop=pDrop)
    }
    else if(vclass=="vector"){
        createTable(pTableName=tablename,
                    pColNames=c("vectorIdColumn",
                                "vectorIndexColumn",
                                "vectorValueColumn"),
                    pColTypes=c("INT","INT",
                                type),
                    pTableOptions=tableoptions,
                    pPrimaryKey=c("vectorIdColumn",
                                "vectorIndexColumn"),
                    pTemporary=temporaryTable,
                    pDrop=pDrop)
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
    getMaxValue(vtable=vtable,
                vcolName="MATRIX_ID",
                vconnection=vconnection)+1


#' Get Max ID from given table
#'
#' used to know ID of last entry in table
#' @param vconnection ODBC/JDBC connection object
#' @param vtable name of the table
#' @param vdatabase name of the database of table
#' @param vcolName name of the primary index column in table

getMaxValue <- function(vtable=getOption("ResultVectorTableFL"),
                        vcolName="vectorIdColumn",
                        vconnection=getOption("connectionFL"))
{
    R <- sqlQuery(vconnection,
                    paste0("SELECT max(",
                           vcolName,")",
                           " FROM ",
                           vtable))[1,1]
    if(is.na(R)) return(0)
    else return(R)

}

#' Get Max Vector ID+1 from result Vector table
#'
#' used to know ID of next entry in table
#' @param vconnection ODBC/JDBC connection object
getMaxVectorId <- function(vconnection = getOption("connectionFL"),
                           vtable=getOption("ResultVectorTableFL"),
                           ...)
    getMaxValue(vtable=vtable,
                vcolName="vectorIdColumn",
                vconnection=vconnection)+1

#' Ensure sqlQuery constructed meets limits
#' namely max size and max nestings
#'
#' @param pResult object whose constructSelect
#' needs to be within limits
#' @param pInput list of input objects
#' @param pOperator function which generated the pResult
#' @param pStoreResult Flag whether to store the pResult
#' @return pResult after storing transparently inputs 
#' and recomputing the operation
#' @examples 
#' flm <- FLMatrix("tblmatrixMulti",3,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
#' flv <- as.FLVector(rnorm(25))
#' vexpression <- paste0(rep("flm+flv",13),collapse="")
#' cat("no.of Nested Queries: ",length(gregexpr("FROM",constructSelect(eval(parse(text=vexpression))))[[1]]))
#' vResult <- eval(parse(text=paste0(vexpression,"+",vexpression)))
#' cat("no.of Nested Queries in Result: ",length(gregexpr("FROM",constructSelect(vResult))[[1]]))
ensureQuerySize <- function(pResult,
                            pInput,
                            pOperator,
                            pStoreResult=FALSE,
                            ...)
{
    ##browser()
    if(checkQueryLimits(pResult))
    {
        vQuerySizes <- sapply(pInput,
                              FUN=function(x)
                                  ifelse(is.FL(x),
                                        object.size(constructSelect(x,...)),
                                        0))
        vbulkyInput <- which.max(vQuerySizes)
        if(vbulkyInput==0)
            return(pResult)
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

rearrangeInputCols <- function(pInputCols,
                                pIndex){
    return(pInputCols[pIndex])
}

separateDBName <- function(vtableName){
  ## If tablename has database name
  names(vtableName) <- NULL
  if(grepl(".",vtableName,fixed=TRUE)){
    vdatabase <- base::strsplit(vtableName,".",fixed=TRUE)[[1]]
    vtableName <- vdatabase[2]
    vdatabase <- vdatabase[1]
  }
  else vdatabase <- getOption("ResultDatabaseFL")
  return(c(vdatabase=vdatabase,
          vtableName=vtableName))
}

removeAlias <- function(pName){
    return(changeAlias(pName,"",""))
}

flag1Check <- function(connection)
{
    return(TRUE)
    # if(getOption("flag1")==0)
    # {
    #     sqlQuery(connection,paste0(" DATABASE ",getOption("ResultDatabaseFL"),"; SET ROLE ALL;"))

    #     temp <- sqlQuery(connection,
    #                      paste0(" CREATE TABLE ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),", FALLBACK ,
                #                NO BEFORE JOURNAL,
                #                NO AFTER JOURNAL,
                #                CHECKSUM = DEFAULT,
                #                DEFAULT MERGEBLOCKRATIO
                #                (
                #                 MATRIX_ID INTEGER,
                #                 rowIdColumn INTEGER,
                #                 colIdColumn INTEGER,
                #                 valueColumn FLOAT)
                #                PRIMARY INDEX ( MATRIX_ID, rowIdColumn, colIdColumn);"))

    #     if(temp!="No Data" || length(temp)!=1)
    #     {
    #         sqlSendUpdate(connection,
    #                  paste0("DROP TABLE ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL"))))

    #         sqlSendUpdate(connection,
    #                  paste0("CREATE TABLE ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),", FALLBACK ,
                #            NO BEFORE JOURNAL,
                #            NO AFTER JOURNAL,
                #            CHECKSUM = DEFAULT,
                #            DEFAULT MERGEBLOCKRATIO
                #            (
                #             MATRIX_ID INTEGER,
                #             ROW_ID INTEGER,
                #             COL_ID INTEGER,
                #             CELL_VAL FLOAT)
             #               PRIMARY INDEX ( MATRIX_ID, ROW_ID, COL_ID );"))
    #     }
    #     options(flag1=1)
    # }
}

flag2Check <- function(connection)
{
    return(TRUE)
    # if(getOption("flag2")==0)
    # {
    #     sqlQuery(connection,paste0(" DATABASE ",getOption("ResultDatabaseFL"),"; SET ROLE ALL;"))

    #     temp <- sqlQuery(connection,
    #                      paste0(" CREATE TABLE ",getOption("ResultDatabaseFL"),".",getOption("ResultSparseMatrixTableFL"),", FALLBACK ,
                #                NO BEFORE JOURNAL,
                #                NO AFTER JOURNAL,
                #                CHECKSUM = DEFAULT,
                #                DEFAULT MERGEBLOCKRATIO
                #                (
                #                 MATRIX_ID INTEGER,
                #                 ROW_ID INTEGER,
                #                 COL_ID INTEGER,
                #                 CELL_VAL FLOAT)
                #                PRIMARY INDEX ( MATRIX_ID, ROW_ID, COL_ID );"))

    #     if(temp!="No Data" || length(temp)!=1)
    #     {
    #         sqlQuery(connection,
    #                  paste0("DROP TABLE ",getOption("ResultDatabaseFL"),".",getOption("ResultSparseMatrixTableFL")))

    #         sqlQuery(connection,
    #                  paste0("CREATE TABLE ",getOption("ResultDatabaseFL"),".",getOption("ResultSparseMatrixTableFL"),", FALLBACK ,
                #            NO BEFORE JOURNAL,
                #            NO AFTER JOURNAL,
                #            CHECKSUM = DEFAULT,
                #            DEFAULT MERGEBLOCKRATIO
                #            (
                #             MATRIX_ID INTEGER,
                #             ROW_ID INTEGER,
                #             COL_ID INTEGER,
                #             CELL_VAL FLOAT)
             #               PRIMARY INDEX ( MATRIX_ID, ROW_ID, COL_ID );"))
    #     }
    #     options(flag2=1)
    # }
}

flag3Check <- function(connection)
{
    return(TRUE)
    # if(getOption("flag3")==0)
    # {
    #     sqlQuery(connection,paste0(" DATABASE ",getOption("ResultDatabaseFL"),"; SET ROLE ALL;"))

    #     temp <- sqlQuery(connection,
    #                      paste0("CREATE TABLE ",getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL"),"
            #                    ( VECTOR_ID INT,
            #                      VECTOR_INDEX INT,
            #                      VECTOR_VALUE VARCHAR(20) )
            #                      PRIMARY INDEX (VECTOR_ID, VECTOR_INDEX);" ))
    #     if(temp != TRUE || length(temp)!=1)
    #     {
    #         sqlQuery(connection,
    #                  paste0("DROP TABLE ",getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL")))

    #         sqlQuery(connection,
    #                  paste0("CREATE TABLE ",getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL"),"
            #               ( VECTOR_ID INT,
            #                 VECTOR_INDEX INT,
            #                 VECTOR_VALUE VARCHAR(20) )
            #                 PRIMARY INDEX (VECTOR_ID, VECTOR_INDEX);" ))
    #     }
    #     options(flag3=1)
    # }
}
