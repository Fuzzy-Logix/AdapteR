#' @include platformMappings.R
NULL
## platform mappings are to created during
## build time:
##
## FLcreatePlatformsMapping()
## storedProcMappingsFL <- getOption("storedProcMappingsFL")
## dump("storedProcMappingsFL",file="AdapteR/R/platformMappings.R")
## MatrixUDTMappingsFL <- getOption("MatrixUDTMappingsFL")
## dump("MatrixUDTMappingsFL",file="AdapteR/R/platformMappings.R",append=TRUE)
## FLcreatePlatformsMapping()

options(MatrixUDTMappingsFL=MatrixUDTMappingsFL)
options(storedProcMappingsFL=storedProcMappingsFL)

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
getFLPlatform <- function(connection=getFLConnection()) return(attr(connection,"platform"))

##' @export
is.TD         <- function(connection=getFLConnection()) getFLPlatform(connection)=="TD"
##' @export
is.TDAster    <- function(connection=getFLConnection()) getFLPlatform(connection)=="TDAster"
##' @export
is.Hadoop     <- function(connection=getFLConnection()) getFLPlatform(connection)=="Hadoop"

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
##' @param ... include 'platform' here. Use TD for Teradata.
##' platform is mandatory for odbc connection. 
##' 'TestDatabase' on which tests are to be run.
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
                      temporary=TRUE,
                      verbose=FALSE,
                      tablePrefix=NULL,
                      ...){
    if(is.null(tablePrefix) & temporary)
        tablePrefix <- user
    if(is.null(tablePrefix) & temporary)
        tablePrefix <- genRandVarName()
    options(ResultDatabaseFL=database)
    options(FLUsername=user)
    options(DSN=list(...)$DSN)
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
        if (!requireNamespace("RODBC", quietly = TRUE)){
            stop("RODBC package needed for using ODBC connections. Please install it.",
                 call. = FALSE)
        }
        tryCatch({
            library(RODBC)
            connection <- RODBC::odbcConnect(odbcSource)
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

    ## store database where tests need to be run
    TestDatabase <- list(...)$TestDatabase
    if(is.null(TestDatabase)){
        vmap <- c(TD="FL_TRAIN",TDAster="fuzzylogix",Hadoop="mazdoo")
        TestDatabase <- vmap[platform]
    }
    else names(TestDatabase) <- platform
    if(platform=="Hadoop")
        options(viewToTable=TRUE)
    else options(viewToTable=FALSE)
    options("TestDatabase"=TestDatabase)
    connection <- FLConnection(connection, platform, name=ifelse(is.null(host),odbcSource,host))
    options("FLConnection" = connection)
    assign("connection", connection, envir = .GlobalEnv)
    FLStartSession(connection=connection,database=database,temporary = temporary,tablePrefix=tablePrefix,...)
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
                           drop=TRUE,
                           debug=FALSE,
                           tableoptions=NULL,
                           tablePrefix=NULL,
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
    ## }
    ##browser()
    options(InteractiveFL             = TRUE)
    options(temporaryFL               = temporary)
    options(NameMapTableFL="tblNameMapping")
    ## Create system table for TablesMetadataInfo
    if(!checkRemoteTableExistence(tableName="fzzlAdapteRTablesInfo"))
        createTable(pTableName="fzzlAdapteRTablesInfo",
                    pColNames=c("TimeInfo","DateInfo",
                                "UserName","DatabaseName",
                                "TableName","ElementID",
                                "ObjType",
                                "UserComments"),
                    pColTypes=c("VARCHAR(255)","VARCHAR(255)",
                                "VARCHAR(255)","VARCHAR(255)",
                                "VARCHAR(255)","INT","VARCHAR(255)",
                                "VARCHAR(255)"),
                    pTableOptions=tableoptions,
                    pPrimaryKey="UserName",
                    pTemporary=FALSE,
                    pDrop=TRUE)
    ## Create names mapping table
    if(drop | !checkRemoteTableExistence(tableName="tblNameMapping"))
        createTable(pTableName="tblNameMapping",
                    pColNames=c("TABLENAME","MATRIX_ID",
                                "DIM_ID","NAME","NUM_ID"),
                    pColTypes=c("VARCHAR(255)","INT",
                                "INT","VARCHAR(255)",
                                "INT"),
                    pTableOptions=tableoptions,
                    pPrimaryKey=c("TABLENAME","MATRIX_ID",
                                  "DIM_ID","NAME"),
                    pTemporary=temporary,
                    pDrop=drop)
    resultTables <- c(
        "ResultVectorTableFL" = "tblVectorResult",
        "ResultMatrixTableFL" = "tblMatrixMultiResult",
        "ResultSparseMatrixTableFL"= "tblMatrixMultiSparseResult",
        "ResultCharVectorTableFL" = "tblCharVectorResult",
        "ResultCharMatrixTableFL" = "tblCharMatrixMultiResult",
        "ResultIntMatrixTableFL" = "tblIntMatrixMultiResult",
        "ResultIntVectorTableFL" = "tblIntVectorResult",
        "ResultByteIntVectorTableFL" = "tblByteIntVectorResult")
    vresultTables <- names(resultTables)
    if(!temporary)
        resultTables <- paste0(database,".",resultTables)
    else
        resultTables <- paste0(tablePrefix,resultTables)
    options(resultTablesFL=resultTables)
    names(resultTables) <- vresultTables
    eval(parse(text=paste0("options(",names(resultTables),"='",resultTables,"')", collapse="\n")))

    options(scipen=999)
    #options(stringsAsFactors=FALSE)

    sapply(vresultTables,
        function(x){
            vtable <- getOption(x)
            if(grepl("matrix",tolower(vtable)))
                vclass <- "matrix"
            else if(grepl("vector",tolower(vtable)))
                vclass <- "vector"
            else
                vclass <- NULL
            if(grepl("byteint",tolower(vtable)))
                vtype <- "BYTEINT"
            else if(grepl("int",tolower(vtable)))
                vtype <- "INT"
            else if(grepl("char",tolower(vtable)))
                vtype <- "VARCHAR(255)"
            else vtype <- "FLOAT"
            if(!is.null(vclass))
                genCreateResulttbl(tablename=vtable,
                                   temporaryTable=temporary,
                                   tableoptions=tableoptions,
                                   vclass=vclass,
                                   type=vtype,
                                   pDrop=drop)
        })
    genSessionID()


    cat("Session Started..\n")
}

parsePlatformMapping <- function(definition){
    if(grepl("^ *$",definition)) return(NULL)
    if(grepl("^ *#.*",definition)) return(NULL)
    lhs <- gsub(" *<-.*","",definition)
    rhs <- gsub(".*<- *","",definition)
    lhsArgs <- gsub(".*\\(|\\).*","", lhs)
    if(lhsArgs==lhs) lhsArgs <- ""
    rhsArgs <- gsub(".*\\(|\\).*","", rhs)
    if(rhsArgs==rhs) rhsArgs <- ""
    ##
    funNameFull <- gsub(" *\\(.*\\) *","",lhs)
    funName <- gsub("\\..*","",funNameFull)
    platform <- gsub("^.*\\.","",funNameFull)
    storedProcPlatform <- gsub(" *\\(.*\\) *","",rhs)
    args <- unlist(strsplit(lhsArgs," *, *"))
    SargsPlatform <- unlist(strsplit(rhsArgs," *, *"))
    argsPlatform <- sapply(strsplit(SargsPlatform," *= *"),
                           function(x){
        r <- x[[length(x)]]
        names(r) <- x[[1]]
        r
    })
    result <- list(funcName=funName,
                   platform=platform,
                   funcNamePlatform=storedProcPlatform,
                   args=args,
                   argsPlatform=argsPlatform)
    return(result)
}


## gk: todo document
#' @export
getStoredProcMapping <- function(query) getOption("storedProcMappingsFL")[[paste0(query,".",getFLPlatform(connection=connection))]]

#' Function to generate platforms mappings for stored procs and UDTs from definitions file.
#'
#' The definitions file has one definition per line
#' <TD_FNAME>.<PLATFORM>(<TD_ARGS>) <- <PLATFORM_FNAME>(<PLATFORM_ARGS>)
#' The definitions file for UDTs has one definition per line
#' <TD_FNAME>.<PLATFORM>(<TD_OUTPUTCOLS>) <- <PLATFORM_FNAME>(<PLATFORM_OUTPUTCOLS>)
# FLcreatePlatformMatrixUDTMapping <- function(definitions='def/platformMatrixUDT.rfl'){
#     defs <- readLines(system.file(definitions, package='AdapteR'))

    
# }

#' @export
FLcreatePlatformsMapping <- function(definitions=c('def/platformStoredProcs.rfl',
                                                    'def/platformMatrixUDT.rfl')){
    defs <- readLines(system.file(definitions[1], package='AdapteR'),encoding="UTF-8")

    storedProcMappings <- lapply(defs,
                                parsePlatformMapping)
    names(storedProcMappings) <- sapply(storedProcMappings,
                                        function(x) paste0(x$funcName,".",x$platform))

    storedProcMappings$prefix.TD="CALL "
    storedProcMappings$prefix.TDAster="SELECT * FROM "
    storedProcMappings$prefix.Hadoop="SELECT "


    storedProcMappings$preArgs.TD=""
    storedProcMappings$preArgs.TDAster="ON (SELECT 1 ) PARTITION BY 1 \n"
    storedProcMappings$preArgs.Hadoop=""

    storedProcMappings$extraPars.TD=c()
    storedProcMappings$extraPars.TDAster=c(DSN=ifelse(is.null(getOption("DSN")),
                                                    "NULL",
                                                    getOption("DSN")))
    storedProcMappings$extraPars.Hadoop=c()

    storedProcMappings$withOutputPars.TD=TRUE
    storedProcMappings$withOutputPars.TDAster=FALSE
    storedProcMappings$withOutputPars.Hadoop=FALSE

    storedProcMappings$withArgNames.TD="none"
    storedProcMappings$withArgNames.TDAster="()"
    storedProcMappings$argSeparator.TDAster="\n"
    storedProcMappings$withArgNames.Hadoop="none"

    storedProcMappings$includeWhere.Hadoop=FALSE
    storedProcMappings$includeWhere.TDAster=TRUE
    storedProcMappings$includeWhere.TD=TRUE

    storedProcMappings$valueMapping.TDAster <- list("NULL"="")
    storedProcMappings$valueMapping.Hadoop <- list("NULL"="")

    options(storedProcMappingsFL=storedProcMappings)

    defs <- readLines(system.file(definitions[2], package='AdapteR'),encoding="UTF-8")
    
    MatrixUDTMappings <- lapply(defs,
                                parsePlatformMapping)
    names(MatrixUDTMappings) <- sapply(MatrixUDTMappings,
                                        function(x) 
                                            paste0(x$funcName,".",x$platform))
    options(MatrixUDTMappingsFL=MatrixUDTMappings)
}


## gk: todo document
getMatrixUDTMapping <- function(query) 
    getOption("MatrixUDTMappingsFL")[[paste0(query,".",getFLPlatform(connection=connection))]]

## return the name of systemTable based on platform
getSystemTableMapping <- function(query,connection=getFLConnection()){
    vlist <- getOption("storedProcMappingsFL")[[paste0("FLSystemTables.",getFLPlatform(connection=connection))]]
    argsMap <- vlist$argsPlatform
    return(names(argsMap)[argsMap==query])
}

genCreateResulttbl <- function(tablename,
                               temporaryTable=TRUE,
                               tableoptions=NULL,
                               vclass,
                               type,
                               pDrop){
    ##browser()
    if(!pDrop & checkRemoteTableExistence(tableName=tablename))
        return()
    
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
flClose <- function(connection=getFLConnection())
{
   # if(length(getOption("FLTempTables"))>0)
   #      sapply(getOption("FLTempTables"),dropTable)
   #  if(length(getOption("FLTempViews"))>0)
   #      sapply(getOption("FLTempViews"),dropView)
    if(inherits(connection,"FLConnection")){
        connection <- connection$connection
    }
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

#' Close Session and Drop temp Tables
#'
#' Strongly recommended to run before quitting current R session
#' @param connection ODBC/JDBC connection object
#' @export
FLClose <- function(connection=getFLConnection()){
    warning("Deprecated, calling flClose(connection).")
    flClose(connection)
}

## check if hypothesis tables exists
#' @export
checkHypoSystemTableExists <- function(){
    ## Create System table for HypothesisTesting Statistics Mapping
    vdf <- tryCatch(read.csv(system.file('def/HypothesisTestsMapping.rfl',package='AdapteR'),encoding="UTF-8"),
                    error=function(e){
                        suppressWarnings({data("HypothesisTestsMapping")
                        vdf <- HypothesisTestsMapping
                        require(plyr)
                        vdf <- apply(vdf,1,function(x)strsplit(as.character(x),","))
                        vdf <- ldply(vdf,function(vdf)vdf[[1]])
                        colnames(vdf) <- c("X","rownames",
                                           "FLFuncName","FLStatistic")
                        rm(HypothesisTestsMapping,envir=.GlobalEnv)})
                        return(vdf)
                    })
    if(!checkRemoteTableExistence(tableName="fzzlARHypTestStatsMap"))
        t <- as.FLTable(vdf,tableName="fzzlARHypTestStatsMap",
                        temporary=FALSE,drop=TRUE)
        
}
