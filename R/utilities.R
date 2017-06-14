# Contains the support functions
#' @include platforms.R
NULL


setOldClass("RODBC")
setOldClass("FLConnection")


cleanNames <- function(x){
    ##change factors to strings
    if(is.factor(x) || class(x)=="Date")
        x <- as.character(x)
    if(is.character(x))
        x <- gsub("^ +| +$","",x)
    x
}

sqlError <- function(e){
    warning(e)
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

#' @export
sqlSendUpdate.FLConnection <- function(connection,query,...) 
    sqlSendUpdate(connection$connection,query,...)

#' Send a query to database
#' Result is returned as data.frame
#' @param channel ODBC/JDBC connection object
#' @param query SQLQuery to be sent
#' @export
sqlQuery <- function(connection,query,...) UseMethod("sqlQuery")

#' @export
sqlQuery.FLConnection <- function(connection,query,...)
    sqlQuery(connection$connection,query,...)

#' Send a query to database
#' 
#' No result is returned
#' @param channel JDBC connection object
#' @param query SQLQuery to be sent
#' @export
sqlSendUpdate.JDBCConnection <- function(connection,query,warn=TRUE,...) {
    verrflag<-sapply(query, function(q){
                            ##browser()
                            if(getOption("debugSQL")) cat(paste0("SENDING SQL: \n",gsub(" +"," ",q),"\n"))
                            tryCatch({
                                if(is.TDAster() || is.Hadoop())
                                    res <- RJDBC::dbSendUpdate(connection,q,...)
                                else{
                                    res <- DBI::dbSendQuery(connection, q, ...)
                                    ##dbCommit(connection)
                                    dbClearResult(res)
                                }
                                return(TRUE)
                            },
                            error=function(e){
                                if(warn) sqlError(e)
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
sqlSendUpdate.RODBC <- function(connection,query,warn=FALSE,...){
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
                                    ##print(sys.calls())
                                    if(warn) sqlError(errmsg)
                                    verrflag <- FALSE
                                }
                                RODBC::odbcClearError(connection)
                                return(verrflag)
                            })
    RODBC::odbcSetAutoCommit(connection, autoCommit = TRUE)
    return(verrflag)
    #cat("DONE...\n")
}
sqlSendUpdate.ODBCConnection <- function(connection, query , warn = TRUE){
    suppressWarnings(sqlQuery(connection, query))
}

#' @export
constructStoredProcArgs <- function(query,
                                    outputParameter,
                                    ...){
    args <- list(...)
    if("pInputParams" %in% names(args))
        args <- args[["pInputParams"]]
    else if(length(args)==1 && is.list(args[[1]]))
        args <- args[[1]]

    ## print("stored PROC Arguments:")
    ## print(args)
    spMap <- getStoredProcMapping(query)
    ## print("stored PROC Mapping:")
    ## print(spMap)
    if(!is.null(spMap)){
        query <- spMap$funcNamePlatform
        if(length(spMap$argsPlatform)>0){
            args <- args[spMap$argsPlatform]
            names(args) <- names(spMap$argsPlatform)
        }
    }
    ## print("remapped stored PROC Arguments:")
    ## print(args)
    return(list(args=args,
                query=query))
}


#' Send a query to database
#' Result is returned as data.frame
#' @param channel ODBC/JDBC connection object
#' @param query SQLQuery to be sent
#' @export
sqlStoredProc <- function(connection, 
                        query, 
                        outputParameter, 
                        ...) 
                    UseMethod("sqlStoredProc")

#' @export
sqlStoredProc.FLConnection <- function(connection,
                                        query,
                                        outputParameter=NULL,
                                        ...) {
    if((is.TDAster(connection=connection)||is.Hadoop(connection=connection)) && 
        class(getRConnection(connection))=="JDBCConnection")
        class(connection$connection) <- "JDBCTDAster"
    sqlStoredProc(connection=getRConnection(connection),
                query=query,
                outputParameter=outputParameter,
                ...)
}

#' @export
sqlStoredProc.JDBCTDAster <- function(connection,
                                    query,
                                    outputParameter,
                                    ...) {
    vlist <- constructStoredProcArgs(query=query,
                                    outputParameter=outputParameter,
                                    ...)
    args <- vlist$args
    query <- vlist$query
    sqlstr <- do.call("constructStoredProcSQL",
                      append(list(pConnection="string",
                                  pFuncName=query,
                                  pOutputParameter=outputParameter),
                             args))
    if(getOption("debugSQL")) {    
        cat(paste0("CALLING Stored Proc: \n",
                   gsub(" +","    ", sqlstr),"\n"))
    }

    class(connection) <- "JDBCConnection"
    retobj <- DBI::dbGetQuery(connection,sqlstr)
    return(retobj)
}

#' @export
sqlStoredProc.RODBC <- function(connection, query, 
                                outputParameter,
                                ...) {
    vlist <- constructStoredProcArgs(query=query,
                                     outputParameter=outputParameter,
                                     ...)
    args <- vlist$args
    query <- vlist$query
    sqlstr <- do.call("constructStoredProcSQL",
                      append(list(pConnection=connection,
                                  pFuncName=query,
                                  pOutputParameter=outputParameter),
                             args))
    retobj <- sqlQuery(connection,sqlstr)
    return(retobj)
}
sqlStoredProc.ODBCConnection <- function(connection,
                                         query,
                                         outputParameter, ...)
{
    return(sqlStoredProc.RODBC(connection, query, outputParameter, ...))
}

#' @export
sqlStoredProc.JDBCConnection <- function(connection, query, 
                                         outputParameter=NULL,
                                         ...) { #browser()
    ## http://developer.teradata.com/doc/connectivity/jdbc/reference/current/jdbcug_chapter_2.html
    ## Creating a CallableStatement object, representing
    ## a precompiled SQL statement and preparing the callable
    ## statement for execution.
    vlist <- constructStoredProcArgs(query=query,
                                    outputParameter=outputParameter,
                                    ...)
    args <- vlist$args
    query <- vlist$query
    
    if(getOption("debugSQL")) {
        sqlstr <- do.call("constructStoredProcSQL",
                          append(list(pConnection="string",
                                      pFuncName=query,
                                      pOutputParameter=outputParameter),
                                 args))        
        cat(paste0("CALLING Stored Proc: \n",
                   gsub(" +","    ", sqlstr),"\n"))
    }
    query <- do.call("constructStoredProcSQL",
                      append(list(pConnection=connection,
                                  pFuncName=query,
                                  pOutputParameter=outputParameter),
                             args))        
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
        .jcall(cStmt,"V","registerOutParameter",ai,a) ## Error Hadoop:- method registerOutParameter with signature (II)V not found 
        ai <- ai+1L
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
            resd <- RODBC::sqlQuery(connection, query, stringsAsFactors = FALSE,...)
            resd <- checkSqlQueryOutput(resd)
            return(resd)
    }
    lapply(query, function(q){
        if(getOption("debugSQL")) cat(paste0("QUERY SQL: \n",q,"\n"))
            resd <- RODBC::sqlQuery(connection, q, stringsAsFactors = FALSE,...)
            resd <- checkSqlQueryOutput(resd)
            return(resd)
    })
}

#' @export
sqlQuery.ODBCConnection <- function(connection, query, ...){
    resd <- DBI::dbGetQuery(connection,query )
    if(is.null(resd)){
        return(TRUE)
    }
    else
        return(resd)
}

sqlQuery.NULL <- function(connection, query, ...){
    stop("please connect to the database before using AdapteR")
}

##' drop a table
##' 
##' @param object FLTable object 
##' @return message if the table is dropped
##' @export
dbDrop <- function(object)
{
    vSqlStr <- paste0(" DROP TABLE ",getTableNameSlot(object))
    sqlSendUpdate(getFLConnection(object), vSqlStr)
    return(paste0(getTableNameSlot(object)," DROPPED"))
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
        if(!inherits(arg_list[[name]],class_list[[name]]))
            stop(paste("Argument Type Mismatch:", name, "should inherit class", class_list[[name]]))
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

gen_table_name <- function(prefix,suffix=NULL){
    vtbl <- ifelse(is.null(suffix),
                   paste0(prefix),
                   paste0(prefix,"_",suffix))
    #options("FLTempTables"=c(getOption("FLTempTables"),vtbl))
    vtbl
}


getMaxId <- function(vdatabase,vtable,vcolName,
                     vconnection=getFLConnection(),...){
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
getMaxMatrixId <- function(vconnection=getFLConnection(),
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
                        vconnection=getFLConnection())
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
getMaxVectorId <- function(vconnection = getFLConnection(),
                           vtable=getOption("ResultVectorTableFL"),
                           ...)
    getMaxValue(vtable=vtable,
                vcolName="vectorIdColumn",
                vconnection=vconnection)+1

#' Ensure sqlQuery constructed meets limits
#' namely max size(1MB) and max nestings(140-147)
#'
#' @param pResult object whose constructSelect
#' needs to be within limits
#' @param pInput list of input objects
#' @param pOperator function which generated the pResult
#' @param pStoreResult Flag whether to store the pResult
#' @return pResult after storing transparently inputs 
#' and recomputing the operation
#' @examples
#' cat("Below Example shows how expressions with number of nested queries exceeding the limit are handled:")
#' flm <- FLMatrix("tblmatrixMulti",3,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
#' flv <- as.FLVector(rnorm(25))
#' vexpression <- paste0(rep("flm+flv",17),collapse="+")
#' cat(vexpression)
#' cat("no.of Nested Queries: ",length(gregexpr("FROM",constructSelect(eval(parse(text=vexpression))))[[1]]))
#' vResult <- eval(parse(text=vexpression))
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

#' @export
checkRemoteTableExistence <- function(databaseName=getOption("ResultDatabaseFL"),
                                      tableName)
{
    ## shortcut in case of a results table -- setup during session start, assumed to not having been dropped
    # if(tableName %in% getOption("resultTablesFL")) return(TRUE)
    ## check if tableName has database
    if(grepl(".",tableName,fixed=TRUE)){
        vdb <- strsplit(tableName,".",fixed=TRUE)[[1]][1]
        vtbl <- strsplit(tableName,".",fixed=TRUE)[[1]][2]
        if(!missing(databaseName) && vdb!=databaseName)
            stop("databaseName and database included in tableName dont match \n ")
        databaseName <- vdb
        tableName <- vtbl
    }

    if(is.TD()){
        vtemp <- sqlQuery(getFLConnection(),paste0(
                        "SELECT 1 FROM dbc.tables \n ",
                        " WHERE databaseName = ",fquote(databaseName),
                        " AND tablename = ",fquote(tableName)))
        if(!is.na(vtemp[1,1]) && vtemp[1,1]==1)
        return(TRUE)
        else return(FALSE)
    }
    else{
        ### No table in Aster that stores MetaInfo!
        if(is.Hadoop())
            tableName <- paste0(databaseName,".",tableName)
        vsqlstr <- limitRowsSQL(paste0("SELECT * FROM \n ",
                                        tableName," \n "),1)
        vtemp <- tryCatch(sqlQuery(getFLConnection(),
                        vsqlstr),error=function(e)FALSE)
        if(is.data.frame(vtemp))
            return(TRUE)
        else return(FALSE)
    }
}


#' @export
existsRemoteTable <- function(tableName,
                              databaseName=getOption("ResultDatabaseFL"))
    checkRemoteTableExistence(databaseName,tableName)


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
  vres <- c(vdatabase=vdatabase,
            vtableName=vtableName)
  names(vres) <- c("vdatabase","vtableName")
  vres
}

removeAlias <- function(pName){
    return(changeAlias(pName,"",""))
}
