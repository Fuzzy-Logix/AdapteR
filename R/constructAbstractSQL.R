NULL
## This should take care of all UDT's in all platforms

## But pFuncName and outColnames differ which messes up things
## As the function definition would also be platform dep.
## Eg:- FLPTFMatrixInverse instead of FLPTFMatrixInv?
## can the output table colnames same as input table names?
##     eg:- matrix_id maintained in output instead of partitionID
##     eg:- cell_val kept as cell_val not inverse_val
##     eg:- Aster is a real Mixture :)
##         OutputVal,cell_val,matrix_inv
## can the output table structure and names be same across platforms?
##     eg:- cell_val and inverse_val both exist in hadoop

## Assumptions: Always partition by Matrix_ID.
##              input arguments to udt are matrix

constructMatrixUDTSQL <- function(pObject,
                                  pFuncName,
                                  pOutColnames=list(
                                      rowIdColumn="row_id",
                                      colIdColumn="col_id",
                                      valueColumn="cell_val"),
                                  pWhereConditions="",
                                  pIncludeMID=TRUE,
                                  ...){

    ## Covers case when vector output is needed
    if(pIncludeMID){
        pOutColnames[["MATRIX_ID"]]="'%insertIDhere%'"
    }

    ## Ensure proper ordering for UDT especially
    object <- orderVariables(object,
                  c("MATRIX_ID","rowIdColumn","colIdColumn","valueColumn")
              )

    return(constructUDTSQL( pViewColnames=c(MATRIX_ID="MATRIX_ID",
                                            Row_ID="rowIdColumn",
                                            Col_ID="colIdColumn",
                                            Cell_Val="valueColumn"
                                            ),
                            pFuncName=pFuncName,
                            pOutColnames=pOutColnames,
                            pWhereConditions=pWhereConditions,
                            pSelect=constructSelect(pObject)
                        )
        )
}

constructUDTSQL <- function(pViewColnames,
                            pFuncName,
                            pOutColnames,
                            pWhereConditions="",
                            pSelect,
                            pPartitionBy=names(pViewColnames)[1],
                            pLocalOrderBy=names(pViewColnames)[1],
                            ...){
    if(is.TD()){
        return(paste0("WITH z( ",paste0(names(pViewColnames),
                                        collapse=","),
                            " )",
                       " AS ( SELECT ",paste0(pViewColnames,
                                            collapse=","),
                            " FROM ( ",pSelect," ) a ",
                        " )",
                       "SELECT ",constructVariables(pOutColnames),
                       "FROM TABLE (",
                            pFuncName,"(",paste0("z.",names(pViewColnames),
                                        collapse=","),
                                    ")",
                            " HASH BY ",paste0("z.",pPartitionBy,
                                            collapse=","),
                            " LOCAL ORDER BY ",paste0("z.",pLocalOrderBy,
                                            collapse=","),
                            ") AS a ",
                        constructWhere(pWhereConditions)
                    )
                )
    }
    ## if(names(getVariables(pObject))==pViewColnames)
    ## Then do not nest

    else if(is.Hadoop()){
        return(paste0("SELECT ",constructVariables(pOutColnames),
                      " FROM ",pFuncName,
                            " ( ON ( SELECT ",constructVariables(pViewColnames),
                                    " FROM ( ",pSelect," ) a ",
                                " ) a ",
                            " PARTITION BY ",paste0(pPartitionBy,
                                            collapse=","),
                                paste0("arg",1:length(pViewColnames),
                                    "(",names(pViewColnames),")",
                                    collapse=","
                                    )
                            ,") a ",
                        constructWhere(pWhereConditions)
                    )
                )
    }

    else if(is.TDAster()){
        return(paste0("SELECT ",constructVariables(pOutColnames),
                      " FROM ",pFuncName,
                            " ( ON ( SELECT ",constructVariables(pViewColnames),
                                    " FROM ( ",pSelect," ) a ",
                                " ) a ",
                            " PARTITION BY ",paste0(pPartitionBy,
                                            collapse=","),
                            " TARGET (",paste0("'",setdiff(names(pViewColnames,
                                                        pPartitionBy)),"'",
                                                collapse=","
                                            )
                            ,")) a ",
                        constructWhere(pWhereConditions)
                    )
                )
    }
}


############################## Stored Procs ###########################
constructStoredProcSQL <- function (pConnection,
                                    pFuncName,
                                    pOutputParameter,
                                    ...) {
    UseMethod("constructStoredProcSQL")
}



constructStoredProcSQL.FLConnection <- function(pConnection,
                                                pFuncName,
                                                pOutputParameter,
                                                ...){
    pConnection <- pConnection$connection
    args <- list(...)
    if("pInputParams" %in% names(args))
        args <- args[["pInputParams"]]
    else if(length(args)==1 && is.list(args[[1]]))
        args <- args[[1]]
    ## Setting up input parameter value
    pars <- character()
    ## Construct input params 
    ## NULL in TD == '' in others
<<<<<<< HEAD
    if(is.ODBC(pConnection) || is.character(pConnection)){
=======
    ## gk: refactor conditionals to class methods
    if(class(pConnection)=="RODBC" | class(pConnection)=="character"){
>>>>>>> d1d2ed8a63b4f901df47c3ed4762d3aeb7974961
        pars <- sapply(args,
                    function(a){
                        if(is.character(a)){
                            if(a=="NULL"){
                                if(is.TD())
                                    return("NULL")
                                else return("''")
                            }
                            else
                                return(fquote(a))
                        } 
                        else return(a)
                    })
        # ai <- 1L
        # for(a in unlist(args)){
        #     if(is.character(a)){
        #         if(a=="NULL"){
        #             if(is.TD())
        #             pars[ai] <- "NULL"
        #             else pars[ai] <- "''"
        #         }
        #         else
        #             pars[ai] <- fquote(a)
        #     } else
        #         pars[ai] <- a
        #     ai <- ai+1L
        # }
    } else {
        pars <- rep("?",length(args))
        if(is.TD())
            names(pOutputParameter)<-"?"
    }

    names(pars) <- names(args)

    vCall <- c(TD="CALL ",
                TDAster="SELECT * FROM ",
                Hadoop="SELECT ")
    vCall <- vCall[[getFLPlatform()]]
    if(is.TDAster()){
        pars <- c(pars,
                DSN=fquote(getOption("DSN")))
        return(paste0(vCall," ",pFuncName,
                "( ON (SELECT 1 ) PARTITION BY 1 ",
                    paste0(names(pars),"(",
                            pars,")",
                            collapse=" \n "),
                    ")"
                )
        )
    }
    else
    return(paste0(vCall," ",pFuncName,
                    "(",
                    paste0(pars,
                            collapse=", \n "),
                    ifelse(is.TD(),
                        paste0(",",
                            paste0(names(pOutputParameter), 
                                collapse=", \n ")),
                        ""),
                    ")"
                )
        )
}

constructStoredProcSQL.character <- constructStoredProcSQL.FLConnection
constructStoredProcSQL.RODBC <- constructStoredProcSQL.FLConnection
constructStoredProcSQL.JDBCConnection <- constructStoredProcSQL.FLConnection
############################### Aggregates ############################
## should already work

############################### Scalars ###########################
# SELECT '%insertIDhere%' AS MATRIX_ID,
#         rowIdColumn AS rowIdColumn,
#         colIdColumn AS colIdColumn,
#         pFunc(valueColumn) AS valueColumn
# FROM (constructSelect(object)) a
getOutputColumns <- function(pObject,
                            pFunc,
                            ...){
    if(is.FLVector(pObject))
    vOutCols <- c("vectorIdColumn",
                "vectorIndexColumn",
                "vectorValueColumn")
    else
    vOutCols <- names(getVariables(pObject))

    names(vOutCols) <- vOutCols
    vValueCol <- as.FLAbstractCol(pObject)
    vOutCols[getIdColname(pObject)] <- "'%insertIDhere%'"
    vOutCols[vValueCol@columnName] <- pFunc(vValueCol,...)
    return(vOutCols)
}
constructScalarSQL <- function(pObject,
                                pFunc,
                                ...
                                ){
    if(is.RowFLVector(pObject))
        pObject <- store(pObject)

    if(is.wideFLTable(pObject))
        pObject <- wideToDeep(pObject)[["table"]]

    if(is.FLSelectFrom(pObject@select)){

        if(is.FLMatrix(pObject) || 
            ((is.FLVector(pObject) || 
                is.FLTable(pObject)) && 
                pObject@isDeep)){
            vVariables <- getVariables(pObject)
            vValueCol <- getValueColumn(pObject)

            vVariables[[names(vValueCol)]] <- pFunc(new("FLAbstractColumn",
                                                         columnName=vValueCol),
                                                    ...)
            pObject@select@variables <- vVariables
            return(pObject)
        }
        if(is.FLVector(pObject)){
            vValueCol <- getValueColumn(pObject)
            #names(pObject@select@table_name) <- NULL
            pObject@Dimnames[[2]] <- pFunc(new("FLAbstractColumn",
                                                columnName=vValueCol),
                                                ...)
            return(pObject)
        }
    }
    else{
        vVariables <- getOutputColumns(pObject=pObject,
                                        pFunc=pFunc,
                                        ...)
        vsqlstr <- paste0("SELECT ",
                        paste0(vVariables," AS ",
                                names(vVariables),
                                collapse=","),
                        " FROM (",constructSelect(pObject),
                            ") a ")
        pObject@select@SQLquery <- vsqlstr
        return(pObject)
    }
}

##################################### Aggregate SQL ###########################################
constructAggregateSQL <- function(pFuncName,
                                  pFuncArgs,
                                  pAddSelect="",
                                  pFrom,
                                  pWhereConditions="",
                                  pGroupBy="",
                                  pOrderBy=""){
    vfunCall <- c(OutVal=paste0(pFuncName,"(",paste0(pFuncArgs,collapse=","),")"))
    vSelects <- c(vfunCall,pAddSelect)
    vSelects <- vSelects[vSelects!=""]

    pWhereConditions <- setdiff(pWhereConditions,"")
    pGroupBy <- setdiff(pGroupBy,"")
    pOrderBy <- setdiff(pOrderBy,"")

    vsqlstr <- paste0("SELECT ",
                    paste0(vSelects," AS ",names(vSelects),collapse=", \n ")," \n ",
                    " FROM ",
                    paste0(ifelse(grepl(" ",pFrom),paste0("(",pFrom,")"),pFrom),
                                    " AS ",names(pFrom),collapse=", \n ")," \n ",
                    ifelse(length(pWhereConditions)>0,
                        paste0(" WHERE ",paste0(pWhereConditions,collapse=" AND ")," \n "),
                        ""),
                    ifelse(length(pGroupBy)>0,
                        paste0(" GROUP BY ",paste0(pGroupBy,collapse=",")," \n "),
                        ""),
                    ifelse(length(pOrderBy)>0,
                        paste0(" ORDER BY ",paste0(pOrderBy,collapse=",")," \n "),
                        ""))
    return(vsqlstr)
}


## gk: this needs review for non-consecutive obs-ids/vectorindexcolumns
## gk: probably best way to solve this is by using cbind
## gk: with an option to not recycle values in shorter vectors (would break t.test)
constructUnionSQL <- function(pFrom,
                            pSelect=NULL){
     vFrom <- as.list(pFrom)
     vSelects <- sapply(1:length(vFrom),
                         function(x){
                             if(is.null(pSelect[[names(vFrom)[[x]]]]))
                                 vinnerSelect <- "*"
                             else{
                                 vinnerSelect <- pSelect[[names(vFrom)[[x]]]]
                                 vinnerSelect <- ifelse(!is.null(names(vinnerSelect)),
                                                     paste0(vinnerSelect," AS ",names(vinnerSelect),collapse=","),
                                                     paste0(vinnerSelect,collapse=","))
                             }
                                 return(paste0("SELECT ",vinnerSelect," \n ",
                                               " FROM (",vFrom[[x]],") AS ",
                                                     names(vFrom)[[x]]))
                             })
     return(paste0(vSelects, collapse= " \n UNION ALL \n "))
}
###############################################################################################

############################ DDLs ##########################################
## Set Database
setCurrentDatabase <- function(pDBName){
    if(is.Hadoop())
        vsqlstr <- paste0("USE ",pDBName,";")
    else if(is.TD())
        vsqlstr <- c(paste0("DATABASE ",pDBName,";"),
                    "SET ROLE ALL;")
    else if(is.TDAster()){
        if(tolower(getOption("ResultDatabaseFL"))!=tolower(pDBName))
            stop("use flConnect to set database in Aster \n ")
        else return()
        }

    sqlSendUpdate(getFLConnection(),vsqlstr)
}

getRemoteTableName <- function(databaseName=getOption("ResultDatabaseFL"),
                               tableName,
                               temporaryTable=getOption("temporaryTablesFL")) {
    if(is.null(databaseName) 
        || temporaryTable 
        || databaseName==getOption("ResultDatabaseFL"))
        return(tableName)
    else return(paste0(databaseName,".",tableName))
}

NULL

##' Create table sql.
##' 
##' covers cases where table is created from other tables
##' with and without data , temporary and permanent
##' if usedbSendUpdate arg is passed in ... that is used
##' in place of dbSendQuery
##' @title Create Table
##' @param pTableName 
##' @param pColNames 
##' @param pColTypes 
##' @param pTableOptions 
##' @param pPrimaryKey 
##' @param pFromTableName 
##' @param pWithData 
##' @param pTemporary 
##' @param pDrop 
##' @param pDatabase 
##' @param pSelect 
##' @param ... 
##' @return The fully qualified table name for referring to this table.
createTable <- function(pTableName,
                        pColNames=NULL,
                        pColTypes=NULL,
                        pTableOptions=NULL,
                        pPrimaryKey=pColNames[1],
                        pFromTableName=NULL,
                        pWithData=TRUE,
                        pTemporary=getOption("temporaryTablesFL"),
                        pDrop=FALSE,
                        pDatabase=getOption("ResultDatabaseFL"),
                        pSelect=NULL,
                        ...){
    if(getTablename(pTableName)!=pTableName){
        if(getDatabase(pTableName)!=pDatabase)
            stop(paste0("pTableName specified conflicting database: ", pTableName," =/= ",pDatabase,""))
        pTableName <- getTablename(pTableName)
    }
    pTableName <- getRemoteTableName(databaseName = pDatabase,
                                     tableName = pTableName,
                                     temporaryTable = pTemporary)

    if(pDrop)
        dropTable(pTableName)
    vtempKeyword <- c(TD="VOLATILE",
                      Hadoop="TEMPORARY",
                      TDAster="TEMPORARY")  ##TEMPORARY="TDAster"
    vtempKeyword <- vtempKeyword[getFLPlatform()]

    addColNameType <- function(pColNames,pColTypes){
        return(paste0(" ( ",
                    paste0(pColNames," ",pColTypes,collapse=","),
                    " ) "))
    }
    addSelectFromtbl <- function(psqlstr,
                                pFromTableName,
                                pWithData,
                                pSelect){
        if(is.null(pSelect)){
            pSelect <- paste0("SELECT * FROM ",pFromTableName)
            if(is.TDAster() || is.Hadoop())
            pSelect <- paste0(pSelect,
                            ifelse(pWithData,
                                    " WHERE 1=1 ",
                                    " WHERE 1=0 "))
        }
        if(is.TD())
            paste0(psqlstr," AS ( ",pSelect," ) ",
                        ifelse(pWithData,
                            " WITH DATA ",
                            " WITH NO DATA "))
        else
            psqlstr <- paste0(psqlstr," AS ",pSelect)
        
    }
    ### Temporary tables can be created only within a BEGIN-END
    ### block in Aster.RollBack exists.
    if(pTemporary && !is.TDAster()){
        vsqlstr <- paste0("CREATE ",vtempKeyword,
                          " TABLE ",pTableName, " ")
    } else 
        vsqlstr <- paste0("CREATE ", " TABLE ",pTableName, " ")

    if(is.TD()){
        if(!is.null(pFromTableName) || !is.null(pSelect))
        vsqlstr <- addSelectFromtbl(vsqlstr,pFromTableName,pWithData,pSelect)
        else{
            ## Add tableOptions
            vsqlstr <- paste0(vsqlstr,
                            ifelse(is.null(pTableOptions),"",
                                paste0(",",paste0(pTableOptions,collapse=","))
                            ))
            ## Add columns
            vsqlstr <- paste0(vsqlstr,addColNameType(pColNames,pColTypes))
        }
        ## Add primaryKey
        if(pPrimaryKey!="" && !is.null(pPrimaryKey))
        vsqlstr <- paste0(vsqlstr," PRIMARY INDEX (",
                            paste0(pPrimaryKey,collapse=","),")")
        ## Add ON COMMIT PRESERVE ROWS
        if(pTemporary)
        vsqlstr <- paste0(vsqlstr," ON COMMIT PRESERVE ROWS ")
    }
    else if(is.TDAster()){
        if(!is.null(pFromTableName) || !is.null(pSelect))
        vsqlstr <- addSelectFromtbl(vsqlstr,pFromTableName,pWithData,pSelect)
        else{
            ## Add columns
            vsqlstr <- paste0(vsqlstr,addColNameType(pColNames,pColTypes))
            ## Add primaryKey
            if(pPrimaryKey!="" && !is.null(pPrimaryKey))
            vsqlstr <- paste0(vsqlstr," DISTRIBUTE BY HASH(",
                                paste0(pPrimaryKey[1],collapse=","),")")
        }
    }
    else if(is.Hadoop()){
        if(!is.null(pFromTableName) || !is.null(pSelect))
        vsqlstr <- addSelectFromtbl(vsqlstr,pFromTableName,pWithData,pSelect)
        else{
            ## Add columns
            vsqlstr <- paste0(vsqlstr,addColNameType(pColNames,pColTypes))
            ## Add primaryKey
            if(pPrimaryKey!="" && !is.null(pPrimaryKey))
            vsqlstr <- paste0(vsqlstr," CLUSTERED BY(",
                                paste0(pPrimaryKey[1],collapse=","),")",
                                " INTO 32 BUCKETS ")
            ## Add tableOptions
            vsqlstr <- paste0(vsqlstr,
                            ifelse(is.null(pTableOptions),"",
                                    paste0(pTableOptions,collapse=" ")))
        }
    }
    vsqlstr <- paste0(vsqlstr,";")
    if(!pTemporary & getOption("temporaryTablesFL")){
        if(!pDrop){
            if(checkRemoteTableExistence(tableName=pTableName))
                # stop(pTableName," already exists. Set pDrop input to TRUE to drop it \n ")
                return()
        }
        warning(paste0("Creating non-temporary table in temporary session:",vsqlstr))
    }

    ## gk @ phani: what will this be used for? It never is used actually...
    if("usedbSendUpdate" %in% names(list(...))){
        cat("sending:  ",vsqlstr)
        return(RJDBC::dbSendUpdate(getFLConnection(),vsqlstr))
    }

    sqlSendUpdate(getFLConnection(),vsqlstr)
    return(pTableName)
}

## CREATE VIEW
createView <- function(pViewName,
                       pSelect,
                       pDatabase=getOption("ResultDatabaseFL"),
                       ...){
    if(getTablename(pViewName)!=pViewName){
        if(getDatabase(pViewName)!=pDatabase)
            stop(paste0("pViewName specified conflicting database: ", pViewName," =/= ",pDatabase,""))
        pViewName <- getTablename(pViewName)
    }
    pViewName <- getRemoteTableName(databaseName = pDatabase,
                                    tableName = pViewName,
                                    temporaryTable = FALSE)
    if("pStore" %in% names(list(...)))
        pStore <- list(...)$pStore
    else pStore <- TRUE
    vsqlstr <- paste0("CREATE VIEW ",pViewName,
                        " AS ",pSelect,";")
    res <- sqlSendUpdate(getFLConnection(),vsqlstr)
    if(pStore)
    updateMetaTable(pTableName=pViewName,
                    pType="view",
                    ...)
    if(!all(res)) stop("View could not be created") ##gk @ phani: what was this for?  I moved it into creatView

    return(pViewName) ## previously res was returned
}

## DROP VIEW
dropView <- function(pViewName){
    sqlSendUpdate(getFLConnection(),
                paste0("DROP VIEW ",pViewName,";"))
}

## DROP TABLE
dropTable <- function(pTableName){
    sqlSendUpdate(getFLConnection(),
                paste0("DROP TABLE ",pTableName,";"))
}

## Insert Into Table
insertIntotbl <- function(pTableName,
                          pColNames=NULL,
                          pValues=NULL,
                          pSelect=NULL){

    # if(!grepl(".",pTableName,fixed=TRUE))
    # pTableName <- getRemoteTableName(getOption("ResultDatabaseFL"),
    #                                 pTableName)

    vsqlstr <- paste0("INSERT INTO ",pTableName)

    if(!is.null(pValues)){
        if(!is.null(pColNames))
            vsqlstr <- paste0(vsqlstr,"(",
                        paste0(pColNames,collapse=","),
                        ") ")
        pValues <- sapply(pValues,
                    function(x){
                        if(is.character(x) && !grepl("'",x))
                        return(fquote(x))
                        else return(x)
                    })
        vsqlstr <- paste0(vsqlstr," VALUES (",
                            paste0(pValues,collapse=","),
                            ");")
    }
    else if(!is.null(pSelect)){
        vsqlstr <- paste0(vsqlstr,"  ",pSelect,";")
    }
    ##print(vsqlstr)
    sqlSendUpdate(getFLConnection(),vsqlstr)
}

updateMetaTable <- function(pTableName,
                            pElementID=NULL,
                            pType="NA",
                            ...){
    vtemp <- separateDBName(pTableName)
    vdatabase <- vtemp["vdatabase"]
    pTableName <- vtemp["vtableName"]

    if("pNote" %in% names(list(...)))
        pNote <- list(...)$pNote
    else pNote <- "NA"

    if(is.null(pElementID))
        pElementID <- -1

    insertIntotbl(pTableName="fzzlAdapteRTablesInfo",
                  pColNames=c("TimeInfo","DateInfo",
                            "UserName","DatabaseName",
                            "TableName","ElementID",
                            "ObjType","Comments"),
                  pValues=list(fquote(as.character(as.POSIXlt(Sys.time(),tz="GMT"))),
                            fquote(as.character(Sys.Date())),
                            fquote(ifelse(is.null(getOption("FLUsername")),
                                "default",getOption("FLUsername"))),
                            fquote(vdatabase),
                            fquote(pTableName),
                            as.integer(pElementID),
                            as.character(pType),
                            fquote(pNote)
                        ))
}

limitRowsSQL <- function(pSelect,pRows){
    vlimitKeyword <- c(LIMIT="TDAster",
                        LIMIT="Hadoop",
                        SAMPLE="TD")
    vlimitKeyword <- names(vlimitKeyword)[vlimitKeyword==getFLPlatform()]
    return(paste0(pSelect," ",vlimitKeyword, " ",pRows))
}
