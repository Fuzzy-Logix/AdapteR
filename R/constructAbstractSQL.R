## This should take care of all UDT's in all platforms

## But pFuncName and outColnames differ which messes up things
## As the function definition would also be platform dep.
## Eg:- FLPTFMatrixInverse instead of FLPTFMatrixInv?
## can the output table colnames same as input table names?
##     eg:- matrix_id maintained in output instead of partitionID
##     eg:- cell_val kept as cell_val not inverse_val
##     eg:- Aster is a real khichidi :)
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
                            pLocalOrderBy=names(pViewColnames)[1]
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
                            ,")) a "
                        constructWhere(pWhereConditions)
                    )
                )
    }
}


############################## Stored Procs ###########################
## Lot of select * 's  in Aster documentation

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
    vCall <- vCall[names(vCall)==getOption("platform")]

    if(is.TDAster())
    return(paste0(vCall," ",pFuncName,
                "( ON (SELECT 1 ) PARTITION BY 1 ",
                    paste0(names(pars),"('",
                            pars,"')",
                            collapse=" \n "),
                    ")"
                )
        )
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
                    ")",
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
constructScalarSQL <- function(pObject,
                                )