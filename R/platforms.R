#' @export
setClass("FLConnection",slots=list())

setOldClass("RODBC")

#' A FLConnection object stores either a JDBC or a ODBC connection
#' as well as the platform that is connected to.
#' 
#' @export
#' @param connection ODBC/JDBC connection class for connectivity for R
#' @param platform character, either TD, TDAster, or Hadoop
FLConnection <- function(connection, platform, name)
    # structure(connection=connection,platform=platform,class="FLConnection")
    structure(list(connection),
              platform=platform,
              name=name,
            class="FLConnection",
            names="connection")

##' @export
setGeneric("getFLConnection", function(object) {
    standardGeneric("getFLConnection")
})
setMethod("getFLConnection", signature(object = "ANY"), function(object) getFLConnection())
setMethod("getFLConnection", signature(object = "missing"), function(object) getOption("FLConnection"))
## setMethod("getConnection", signature(object = "FLMatrix"), function(object) object@select@connection)
## setMethod("getConnection", signature(object = "FLTable"), function(object) object@select@connection)
## setMethod("getConnection", signature(object = "FLTableQuery"), function(object) object@select@connection)
## setMethod("getConnection", signature(object = "FLVector"), function(object) object@select@connection)

getFLConnectionName <- function(...) attr(getFLConnection(...),"name")

##' @export
getFLPlatform <- function(connection=getFLConnection()) return(attr(getFLConnection(),"platform"))
is.TD         <- function(Connection=getFLConnection()) getFLPlatform()=="TD"
is.TDAster    <- function(Connection=getFLConnection()) getFLPlatform()=="TDAster"
is.Hadoop     <- function(Connection=getFLConnection()) getFLPlatform()=="Hadoop"

##' @export
setGeneric("getRConnection", function(object) {
    standardGeneric("getRConnection")
})
setMethod("getRConnection", 
    signature(object = "FLConnection"), 
    function(object) 
    object$connection)



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
##' @param temporary TRUE if result tables are to be created as volatile tables
##' @param verbose print debugging messages
##' @param ... include platform here. Use TD for Teradata.
##' platform is mandatory for odbc connection
##' @return either an ODBC connection or an JDBC connection
##' @examples
##' connection <- flConnect("jdbc:teradata://xx.xxx.x.xxx",
##'                           "FL_TRAIN",
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
##' connection <- flConnect(odbcSource="sourceName",database="FL_TRAIN",platform="TD")
##' connection <- flConnect(odbcSource="SorceName",database="fuzzylogix",platform="TDAster")
##' @export
flConnect <- function(host=NULL,database=NULL,user=NULL,passwd=NULL,
                      jdbc.jarsDir=NULL,
                      jdbc.options="",# "TMODE=TERA,CHARSET=ASCII",
                      odbcSource=NULL,
                      driverClass=NULL,
                      temporary=FALSE,
                      verbose=FALSE,
                      ...){

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

    platformMap <- c("teradata"                        ="TD",
                     "com.teradata.jdbc.TeraDriver"    ="TD",
                     "aster"                           ="TDAster",
                     "astertd"                         ="TDAster",
                     "teradataaster"                   ="TDAster",
                     "com.asterdata.ncluster.Driver"   ="TDAster",
                     "hive"                            ="Hadoop",
                     "cloudera"                        ="Hadoop",
                     "clouderahive"                    ="Hadoop",
                     "hive2"                           ="Hadoop",
                     "org.apache.hive.jdbc.HiveDriver" ="Hadoop",
                     "TDAster"                         ="TDAster",
                     "TD"                              ="TD",
                     "Hadoop"                          ="Hadoop")
    platform <- platformMap[driverClass]
    if(length(platform)==0) platform <- list(...)$platform ## if platform cannot be determined from driverClass, use platform argument
    if(!is.null(platform)) {
        if(!(platform %in% unique (platformMap))) ## use map
            platform <- platformMap[[platform]]
    }
    connection <- FLConnection(connection, platform, name=ifelse(is.null(host),odbcSource,host))
    options("FLConnection" = connection)
    assign("connection", connection, envir = .GlobalEnv)
    FLStartSession(connection=connection,database=database,temporary = temporary,...)
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
#' @param temporary TRUE if result tables are to be created as volatile tables
#' @param drop logical to specify to drop result tables if already existing
#' @param tableoptions options used to create result tables
#' @export
FLStartSession <- function(connection,
                           database=getOption("ResultDatabaseFL"),
                           temporary=TRUE,
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
    options(temporaryFL               = temporary)
    options(ResultVectorTableFL       = gen_table_name("tblVectorResult"))
    options(ResultMatrixTableFL       = gen_table_name("tblMatrixMultiResult"))
    options(ResultSparseMatrixTableFL = gen_table_name("tblMatrixMultiSparseResult"))
    options(NameMapTableFL            = gen_table_name("tblNameMapping"))
    options(ResultCharVectorTableFL   = gen_table_name("tblCharVectorResult"))
    options(ResultCharMatrixTableFL   = gen_table_name("tblCharMatrixMultiResult"))
    options(ResultIntMatrixTableFL    = gen_table_name("tblIntMatrixMultiResult"))
    options(ResultIntVectorTableFL    = gen_table_name("tblIntVectorResult"))
    options(ResultByteIntVectorTableFL    = gen_table_name("tblByteIntVectorResult"))

    options(scipen=999)
    #options(stringsAsFactors=FALSE)

    vresultTables <- c("ResultMatrixTableFL","ResultSparseMatrixTableFL",
                        "ResultCharMatrixTableFL","ResultIntMatrixTableFL",
                        "ResultVectorTableFL","ResultCharVectorTableFL",
                        "ResultIntVectorTableFL","ResultByteIntVectorTableFL")
    sapply(vresultTables,
        function(x){
            vtable <- getOption(x)
            if(grepl("matrix",tolower(vtable)))
            vclass <- "matrix"
            else vclass <- "vector"
            if(grepl("byteint",tolower(vtable)))
            vtype <- "BYTEINT"
            else if(grepl("int",tolower(vtable)))
            vtype <- "INT"
            else if(grepl("char",tolower(vtable)))
            vtype <- "VARCHAR(100)"
            else vtype <- "FLOAT"
            genCreateResulttbl(tablename=vtable,
                                temporaryTable=temporary,
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
                pTemporary=temporary,
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

#' Close Session and Drop temp Tables
#'
#' Strongly recommended to run before quitting current R session
#' @param connection ODBC/JDBC connection object
#' @export
FLClose <- function(connection)
{
   # if(length(getOption("FLTempTables"))>0)
   #      sapply(getOption("FLTempTables"),dropTable)
   #  if(length(getOption("FLTempViews"))>0)
   #      sapply(getOption("FLTempViews"),dropView)

    if(class(connection)=="RODBC")
        RODBC::odbcClose(connection)
    else
        RJDBC::dbDisconnect(connection)
    options(flag1=0)
    options(flag1=0)
    options(flag1=0)
    #options("FLTempTables"=c())
    #options("FLTempViews"=c())
    options("FLSessionID"=c())
}
