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
                            pFuncName,"(",paste0("z.",names(viewCols),
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

constructStoredProcSQL <- function(pConnection,
                                    pFuncName,
                                    pOutputParameter,
                                    ...){
    args <- list(...)
    ## Setting up input parameter value
    pars <- character()

    ## Construct input params 
    ## NULL in TD == '' in others
    if(class(pConnection)=="RODBC"){
        ai <- 1L
        for(a in args){
            if(is.character(a)){
                if(a=="NULL"){
                    if(is.TD())
                    pars[ai] <- "NULL"
                    else pars[ai] <- "''"
                }
                else
                    pars[ai] <- fquote(a)
            } else
                pars[ai] <- a
            ai <- ai+1L
        }
    }
    else{
        pars <- rep("?",length(args))
        if(is.TD())
        names(pOutputParameter)<-"?"
    }

    names(pars) <- names(args)

    vCall <- c(TD="CALL ",
                TDAster="SELECT * FROM ",
                Hadoop="SELECT ")
    vCall <- vCall[names(vCall)==getOption("FLPlatform")]

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
            pObject@dimnames[[2]] <- pFunc(new("FLAbstractColumn",
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


############################ DDLs ##########################################
## Set Database
setCurrentDatabase <- function(pDBName){
    if(is.Hadoop())
        vsqlstr <- paste0("USE ",pDBName,";")
    else if(is.TD())
        vsqlstr <- c(paste0("SELECT ",pDBName,";"),
                    "SET ROLE ALL;")
    else if(is.TDAster())
    stop("use flConnect to set database in Aster \n ")

    sqlSendUpdate(getOption("connectionFL"),vsqlstr)
}

## CREATE TABLE SQL
## covers cases where table is created from other tables
## with and without data , temporary and permanent
createTable <- function(pTableName,
                        pColNames=NULL,
                        pColTypes=NULL,
                        pTableOptions=NULL,
                        pPrimaryKey=pColNames[1],
                        pFromTableName=NULL,
                        pWithData=TRUE,
                        pTemporary=TRUE,
                        pDrop=FALSE,
                        pDatabase=getOption("ResultDatabaseFL"),
                        pSelect=NULL){

    # if(missing(pDatabase))
    # pTableName <- getRemoteTableName(pDatabase,pTableName)
    if(pDrop)
        dropTable(pTableName)
    vtempKeyword <- c(VOLATILE="TD",
                    TEMPORARY="Hadoop",
                    "TDAster")  ##TEMPORARY="TDAster"
    vtempKeyword <- names(vtempKeyword)[vtempKeyword==getOption("FLPlatform")]

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

    vsqlstr <- paste0("CREATE ",ifelse(pTemporary,vtempKeyword,""),
                            " TABLE ",pTableName, " ")

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
            ## Add primaryKey
            if(pPrimaryKey!="" && !is.null(pPrimaryKey))
            vsqlstr <- paste0(vsqlstr," PRIMARY INDEX (",
                                paste0(pPrimaryKey,collapse=","),")")
            ## Add ON COMMIT PRESERVE ROWS
            if(pTemporary)
            vsqlstr <- paste0(vsqlstr," ON COMMIT PRESERVE ROWS ")
        }
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
    print(vsqlstr)

    sqlSendUpdate(getOption("connectionFL"),vsqlstr)
}

## CREATE VIEW
createView <- function(pViewName,
                       pSelect){
    vsqlstr <- paste0("CREATE VIEW ",pViewName,
                        " AS ",pSelect,";")
    sqlSendUpdate(getOption("connectionFL"),vsqlstr)
}

## DROP VIEW
dropView <- function(pViewName){
    sqlSendUpdate(getOption("connectionFL"),
                paste0("DROP VIEW ",pViewName,";"))
}

## DROP TABLE
dropTable <- function(pTableName){
    sqlSendUpdate(getOption("connectionFL"),
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
    print(vsqlstr)
    sqlSendUpdate(getOption("connectionFL"),vsqlstr)
}

updateMetaTable <- function(pTableName,
                            pElementID=NULL,
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
                            "Comments"),
                  pValues=list(fquote(as.character(as.POSIXlt(Sys.time(),tz="GMT"))),
                            fquote(as.character(Sys.Date())),
                            fquote(ifelse(is.null(getOption("FLUsername")),
                                "default",getOption("FLUsername"))),
                            fquote(vdatabase),
                            fquote(pTableName),
                            as.integer(pElementID),
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
