
#' k-Nearest Neighbour Indexes
#' For each row of the test set, the k nearest (according to Euclidean distance metric) 
#' training set vectors are found and indices,distances are returned.
#' @param data input deep FLTable
#' @param query input deep FLTable
#' @param k number of neighbours considered.
#' @param algorithm currently only brute is supported
#' @param ... additional parameters like dist Flag to get the distances as well.
#' @return FLVector of indices or a list of dist & index if dist FLag is true.
#' @examples
#' FLdeepTbl <- FLTable(getTestTableName("ARknnDevSmall"),"obsid","varid","num_val")
#' FLknnOutput <- knnx.index(FLdeepTbl,FLdeepTbl,k=2)
#' FLknnOutput
#' @export
knnx.index <- function(data,
                query,
                k=1,
                algorithm="kd_tree",
                ...){
    UseMethod("knnx.index")
}

#' @export
knnx.index.default <- FNN::knnx.index

#' @export
knnx.index.FLTable <- function(data,
                                query,
                                k=1,
                                algorithm="brute",
                                ...){
    if("dist" %in% names(list(...)))
        vDistFLag <- list(...)[["dist"]]
    else vDistFLag <- FALSE
    return(batchIndexCompute(pDataTbl=data,
                            pSearchTbl=query,
                            k=k,
                            pReturnDist=vDistFLag,
                            ...))
}

batchIndexCompute <- function(pDataTbl,
                              pSearchTbl,
                              pNBatches=100,
                              pReturnDist=FALSE,
                              k=1,
                              ...){
    if(!isDeep(pDataTbl))
        pDataTbl <- wideToDeep(pDataTbl,
                                fetchIDs=FALSE)
    if(!isDeep(pSearchTbl))
        pSearchTbl <- wideToDeep(pSearchTbl,
                                fetchIDs=FALSE)

    vBatchSize <- ceiling(nrow(pSearchTbl)/pNBatches)
    vtableNames <- sapply(list(pSearchTbl,pDataTbl),getTableNameSlot)

    vIndexTableName <- gen_unique_table_name(paste0(vtableNames[2],"Index"))

    # pSearchTbl <- setAlias(pSearchTbl,"TreatTbl")
    # pDataTbl <- setAlias(pDataTbl,"DataTbl")

    ## get Column aliases
    vobsidColnames <- sapply(list(pSearchTbl,pDataTbl),getObsIdSQLName)
    vvaridColnames <- sapply(list(pSearchTbl,pDataTbl),getVarIdSQLName)
    vvalueColnames <- sapply(list(pSearchTbl,pDataTbl),getValueSQLName)

    ## Construct WhereClause
    getBatchWhere <- function(pBatchNum){
        pasteOperator <- function(lhs,rhs,op="<>")
            return(paste0(lhs," ",op," ",rhs))
        return(constructWhere(c(pasteOperator(paste0("TreatTbl.",vvaridColnames[1]),
                                            paste0("DataTbl.",vvaridColnames[2]),"="),
                                # where(pDataTbl),
                                # where(pSearchTbl),
                                # pasteOperator(vobsidColnames[1],vobsidColnames[2]),
                                # pasteOperator(vvaridColnames[1],-1),
                                # pasteOperator(vvaridColnames[2],-1),
                                pasteOperator(paste0("TreatTbl.",vvalueColnames[1]),
                                              paste0("DataTbl.",vvalueColnames[2])), ## fails if we match and remove points which are exactly
                                                                                    ## same.. in the where clause in FLKNN
                                pasteOperator(pasteOperator(paste0("TreatTbl.",vobsidColnames[1]),
                                                            vBatchSize,"MOD"),
                                              pBatchNum,
                                             "=")
                            ))
                )
    }

    genResultQuery <- function(pBatchNum){
        paste0("SELECT TreatTbl.",vobsidColnames[1]," AS searchid, \n ",
                        "DataTbl.",vobsidColnames[2]," AS matchid, \n ",
                        "FLEuclideanDist(TreatTbl.",vvalueColnames[1],",DataTbl.",
                                           vvalueColnames[2],") AS dist \n ",
                "FROM (",constructSelect(pSearchTbl),") TreatTbl, \n (",
                        constructSelect(pDataTbl),") DataTbl \n ",
                getBatchWhere(pBatchNum)," \n ",
                " QUALIFY ",k," >= ROW_NUMBER() OVER(PARTITION BY TreatTbl.",
                    vobsidColnames[1]," ORDER BY dist) \n ",
                " GROUP BY TreatTbl.",vobsidColnames[1],",DataTbl.",vobsidColnames[2])
    }

    vres <- createTable(pTableName=vIndexTableName,
                        pColNames=c("searchid","matchid","dist"),
                        pColTypes=c("BIGINT","BIGINT","FLOAT"),
                        pPrimaryKey=c("searchid","matchid"))

    vres <- sapply(0:(vBatchSize-1),
                    function(x){
                        vres <- insertIntotbl(pTableName=vIndexTableName,
                                              pSelect=genResultQuery(x))
                    })

    genResultFLMatrix <- function(vResColname){
        variables <- list(
                    MATRIX_ID="'%insertIDhere%'",
                    rowIdColumn="searchid",
                    colIdColumn="colIdColumn",
                    valueColumn=vResColname)

        sqlstr <- paste0("SELECT DENSE_RANK() OVER(ORDER BY searchid) AS rowIdColumn, \n ",
                                "ROW_NUMBER() OVER(PARTITION BY searchid ORDER BY dist,matchid) AS colIdColumn, \n ",
                                vResColname, " AS valueColumn \n ",
                        " FROM ",vIndexTableName
                        )

        tblfunqueryobj <- new("FLTableFunctionQuery",
                              connectionName = attr(connection,"name"),
                              variables=list(
                                  Matrix_ID="MATRIX_ID",
                                  rowIdColumn="rowIdColumn",
                                  colIdColumn="colIdColumn",
                                  valueColumn="valueColumn"),
                              whereconditions="",
                              order = "",
                              SQLquery=sqlstr)
    
        return(newFLMatrix(
                  select = tblfunqueryobj,
                  dims = c(nrow(pSearchTbl),k),
                  Dimnames = list(NULL,NULL),
                  type="double"))
    }

    vIndexFLMatrix <- genResultFLMatrix("matchid")
    if(pReturnDist)
        vIndexFLMatrix <- list(index=vIndexFLMatrix,
                                dist=genResultFLMatrix("dist"))
    return(vIndexFLMatrix)
}

#' @export
knnx.index.FLMatrix <- knnx.index.FLTable


## single case when rows is FLMatrix and cols
## is a character is implemented for coke use-case.
## Further development needs done.

#' @export
subsetFLIndices <- function(object,rows,cols){
    # browser()
    if(!is.FLMatrix(rows) || !is.FLTable(object)
        || !is.character(cols))
        stop("yet to be implemented.Please raise a request on github. \n ")

    if(!ncol(rows)==1)
        stop("yet to be implemented.Please raise a request on github. \n ")

    vmatDims <- c(getObsIdSQLName(rows),
                  getVarIdSQLName(rows),
                  getValueSQLName(rows))
    vtblobsid <- getObsIdSQLName(object)
    sqlstr <- paste0("SELECT '%insertIDhere%' as vectorIdColumn, ",
                        "a.",vmatDims[1]," as vectorIndexColumn,\n ",
                        "b.",cols," as vectorValueColumn \n ",
                 "FROM (",constructSelect(indicesKNN),") a,\n ",
                        "(",constructSelect(controlKNN),") b \n ",
                 "WHERE a.",vmatDims[3],"=b.",vtblobsid)

    tblfunqueryobj <- new("FLTableFunctionQuery",
                                  connectionName = attr(connection,"name"),
                                  variables = list(
                                      obs_id_colname = "vectorIndexColumn",
                                      cell_val_colname = "vectorValueColumn"),
                                  whereconditions="",
                                  order = "",
                                  SQLquery=sqlstr)
    flv <- newFLVector(
               select = tblfunqueryobj,
               Dimnames = list(NULL,"vectorValueColumn"),
               isDeep = FALSE,
               type=typeof(object))

}


##Deprecated

# knn.index.FLTable <- function(data,
#                             query,
#                             k=1,
#                             algorithm="kd_tree",
#                             ...){
#     if("dist" %in% names(list(...)))
#         vDistFLag <- list(...)[["dist"]]
#     else vDistFLag <- FALSE
#     pDataTbl=data
#     pSearchTbl=query
#     k=k
#     pReturnDist=vDistFLag
#     pNBatches=100

#     if(!isDeep(pDataTbl))
#         pDataTbl <- wideToDeep(pDataTbl,
#                                 fetchIDs=FALSE)
#     if(!isDeep(pSearchTbl))
#         pSearchTbl <- wideToDeep(pSearchTbl,
#                                 fetchIDs=FALSE)

#     vBatchSize <- ceiling(nrow(pSearchTbl)/pNBatches)
#     vtableNames <- sapply(list(pSearchTbl,pDataTbl),getTableNameSlot)

#     vIndexTableName <- gen_unique_table_name(paste0(vtableNames[2],"Index"))
    
#     pSearchTbl <- setAlias(pSearchTbl,"TreatTbl")
#     pDataTbl <- setAlias(pDataTbl,"DataTbl")

#     ## get Column aliases
#     vobsidColnames <- sapply(list(pSearchTbl,pDataTbl),
#                             function(x)paste0(getAlias(x),".",getObsIdSQLName(x)))
#     vvaridColnames <- sapply(list(pSearchTbl,pDataTbl),
#                             function(x)paste0(getAlias(x),".",getVarIdSQLName(x)))
#     vvalueColnames <- sapply(list(pSearchTbl,pDataTbl),
#                             function(x)paste0(getAlias(x),".",getValueSQLName(x)))

#     ## Construct WhereClause
#     getBatchWhere <- function(pBatchNum){
#         pasteOperator <- function(lhs,rhs,op="<>")
#             return(paste0(lhs," ",op," ",rhs))
#         return(constructWhere(c(pasteOperator(vvaridColnames[1],vvaridColnames[2],"="),
#                                 # where(pDataTbl),
#                                 # where(pSearchTbl),
#                                 # pasteOperator(vobsidColnames[1],vobsidColnames[2]),
#                                 # pasteOperator(vvaridColnames[1],-1),
#                                 # pasteOperator(vvaridColnames[2],-1),
#                                 pasteOperator(vvalueColnames[1],vvalueColnames[2]), ## fails if we match and remove points which are exactly
#                                                                                     ## same.. in the where clause in FLKNN
#                                 pasteOperator(pasteOperator(vobsidColnames[1],
#                                                             vBatchSize,"MOD"),
#                                               pBatchNum,
#                                              "=")
#                             ))
#                 )
#     }

#     genResultQuery <- function(pBatchNum){
#         paste0("SELECT ",vobsidColnames[1]," AS searchid, \n ",
#                         vobsidColnames[2]," AS matchid, \n ",
#                         "FLEuclideanDist(",vvalueColnames[1],",",
#                                            vvalueColnames[2],") AS dist \n ",
#                 "FROM (",constructSelect(pSearchTbl),") ",getAlias(pSearchTbl),", \n (",
#                         constructSelect(pDataTbl),") ",getAlias(pDataTbl)," \n ",
#                 getBatchWhere(pBatchNum)," \n ",
#                 " QUALIFY ",k," >= ROW_NUMBER() OVER(PARTITION BY ",
#                     vobsidColnames[1]," ORDER BY dist) \n ",
#                 " GROUP BY ",vobsidColnames[1],",",vobsidColnames[2])
#     }

#     vres <- createTable(pTableName=vIndexTableName,
#                         pColNames=c("searchid","matchid","dist"),
#                         pColTypes=c("BIGINT","BIGINT","FLOAT"),
#                         pPrimaryKey=c("searchid","matchid"))

#     vres <- sapply(0:(vBatchSize-1),
#                     function(x){
#                         vres <- insertIntotbl(pTableName=vIndexTableName,
#                                               pSelect=genResultQuery(x))
#                     })

#     genResultFLMatrix <- function(vResColname){
#         variables <- list(
#                     MATRIX_ID="'%insertIDhere%'",
#                     rowIdColumn="searchid",
#                     colIdColumn="colIdColumn",
#                     valueColumn=vResColname)

#         sqlstr <- paste0("SELECT DENSE_RANK() OVER(ORDER BY searchid) AS rowIdColumn, \n ",
#                                 "ROW_NUMBER() OVER(PARTITION BY searchid ORDER BY dist,matchid) AS colIdColumn, \n ",
#                                 vResColname, " AS valueColumn \n ",
#                         " FROM ",vIndexTableName
#                         )

#         tblfunqueryobj <- new("FLTableFunctionQuery",
#                               connectionName = attr(connection,"name"),
#                               variables=list(
#                                   Matrix_ID="MATRIX_ID",
#                                   rowIdColumn="rowIdColumn",
#                                   colIdColumn="colIdColumn",
#                                   valueColumn="valueColumn"),
#                               whereconditions="",
#                               order = "",
#                               SQLquery=sqlstr)
    
#         return(newFLMatrix(
#                   select = tblfunqueryobj,
#                   dims = c(nrow(pSearchTbl),k),
#                   Dimnames = list(NULL,NULL),
#                   type="double"))
#     }

#     vIndexFLMatrix <- genResultFLMatrix("matchid")
#     if(pReturnDist)
#         vIndexFLMatrix <- list(index=vIndexFLMatrix,
#                                 dist=genResultFLMatrix("dist"))
#     return(vIndexFLMatrix)

# }
