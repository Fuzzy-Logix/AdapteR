#' k-Nearest Neighbour 
#' k-nearest neighbour classification/Regression for test set from training set. 
#' For each row of the test set, the k nearest (according to distance metric speicified) 
#' training set vectors are found, and the classification is decided by 
#' majority vote, with ties broken at random. If there are ties for the 
#' kth nearest vector, all candidates are included in the vote.
#' If classify flag is false, average of k neighbours is returned.
#' @param train input deep FLTable
#' @param test input deep FLTable
#' @param cl ColumnName of true classifications of training set
#' @param k number of neighbours considered.
#' @param prob If this is true, 
#' the proportion of the votes for the winning class are returned as attribute prob.
#' @param classify logical if classification/regression is solved
#' @param metric distance metric to be used. euclidean,manhattan supported.
#' @return FLVector of classifications of test set.
#' @examples
#' FLdeepTbl <- FLTable(getTestTableName("ARknnDevSmall"),"obsid","varid","num_val")
#' FLknnOutput <- knn(FLdeepTbl,k=3,prob=TRUE)
#' FLknnOutput
#' attributes(FLknnOutput)$prob
#' @export
knn <- function(train,
                test=train,
                cl="NULL",
                k=1,
                l=0,
                prob=FALSE,
                use.all=FALSE,
                ...){
    UseMethod("knn")
}

#' @export
knn.default <- class::knn

#' @export
knn.FLTable <- function(train,
                        test=NULL,
                        cl="NULL",
                        k=1,
                        l=0,
                        prob=FALSE,
                        use.all=FALSE,
                        classify=TRUE,
                        metric="euclidean",
                        ...){
    vupper <- TRUE
    vdiag <- FALSE

    if(is.vector(cl) || is.factor(cl))
        if(cl!="NULL")
            cl <- as.FL(c(cl))

    if(!isDeep(train))
        train <- FLRegrDataPrep(train,depCol=cl)
    if(is.null(test)){
        test <- train
        vupper <- FALSE
        vdiag <- FALSE
    }
    if(!isDeep(test))
        test <- FLRegrDataPrep(test,depCol=cl)

    vtableNames <- sapply(list(train,test),getTableNameSlot)


    ## Calculate Dist Matrix
    vDistTableName <- gen_unique_table_name(paste0(vtableNames[1],"Dist"))

    vDistMatrix <- FLgetDistMatrix(test,train,
                                    metric=metric,
                                    outTableName=vDistTableName,
                                    upper=vupper,
                                    diag=vdiag
                                    )

    ## get Column aliases
    vobsidColnames <- sapply(list(train,test,vDistMatrix),getObsIdSQLName)
    vvaridColnames <- sapply(list(train,test,vDistMatrix),getVarIdSQLName)
    vvalueColnames <- sapply(list(train,test,vDistMatrix),getValueSQLName)
    vtableNames <- c(vtableNames,getTableNameSlot(vDistMatrix))

    ## Populate the matrix
    if(!vupper){
    vsqlstr <- paste0("SELECT ",vvaridColnames[3],", \n ",
                                vobsidColnames[3],", \n ",
                                vvalueColnames[3]," \n ",
                        " FROM (",constructSelect(vDistMatrix),") a ")

    vtempResult <- insertIntotbl(pTableName=getTableNameSlot(vDistMatrix),
                                pSelect=vsqlstr)
    }

    genFLVector <- function(pQuery){
        tblfunqueryobj <- new("FLTableFunctionQuery",
                                  connectionName = attr(getFLConnection(),"name"),
                                  variables = list(
                                      obs_id_colname = "vectorIndexColumn",
                                      cell_val_colname = "vectorValueColumn"),
                                  whereconditions="",
                                  order = "",
                                  SQLquery=pQuery)
        vrownames <- rownames(test)
        if(length(vrownames)==0)
            vrownames <- 1:nrow(test)
        flv <- newFLVector(
                   select = tblfunqueryobj,
                   Dimnames = list(vrownames,"vectorValueColumn"),
                   isDeep = FALSE,
                   type="double")
        flv
    }

    genResultQuery <- function(pColname=NULL,
                                classify=classify,
                                cl=cl){
        if(is.character(cl) && cl=="NULL")
            vAddWhereClause <- paste0(" \n AND b.",vvaridColnames[1]," = -1 \n ")
        else if(is.FLVector(cl)){
            train <- cl
            vobsidColnames[1] <- getIndexSQLName(cl)[2]
            vvalueColnames[1] <- getValueSQLName(cl)
            vAddWhereClause <- ""
        }
        else vAddWhereClause <- ""
        
        if(classify){
            if(is.null(pColname))
                pColname <- "predClass"
            return(paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                                    "a.obsid AS vectorIndexColumn, \n ",
                                    "a.",pColname," AS vectorValueColumn \n ",
                        " FROM \n ",
                            "(SELECT DISTINCT a.obsidX AS obsid,\n ",
                                    " b.",vvalueColnames[1]," AS predClass, \n ",
                                    "CASE WHEN a.simIndex=0 THEN 1 ELSE ",
                                    " ((FLSUM(a.simIndex) OVER(PARTITION BY a.obsidX,",
                                        "b.",vvalueColnames[1],"))/(FLSUM(a.simIndex) ",
                                        "OVER(PARTITION BY a.obsidx))) END AS prob \n ",
                            " FROM( \n ",
                                " SELECT ",vobsidColnames[3]," AS obsidX, \n ",
                                        vvaridColnames[3]," AS obsidY, \n ",
                                        "CASE WHEN ",vvalueColnames[3],"=0 THEN 0 ELSE 1/",
                                        vvalueColnames[3]," END AS simIndex \n ",
                                " FROM (",constructSelect(vDistMatrix),") a \n ",
                                " QUALIFY RANK()OVER(PARTITION BY ",vobsidColnames[3]," ORDER BY ",vvalueColnames[3],")<=",k,
                                " \n ) a, \n (",constructSelect(train),") b \n ",
                            " WHERE a.obsidY = b.",vobsidColnames[1],vAddWhereClause,
                            " GROUP BY a.obsidx,b.",vvalueColnames[1],",a.simIndex) a \n ",
                        " QUALIFY ROW_NUMBER()OVER(PARTITION BY a.obsid ORDER BY a.prob DESC) <=1"))
        }
        else{
            if(is.null(pColname))
                pColname <- "pred"
            return(paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                                    "a.obsid AS vectorIndexColumn, \n ",
                                    "a.",pColname," AS vectorValueColumn \n ",
                        " FROM \n ",
                            "(SELECT DISTINCT a.obsidX AS obsid,\n ",
                                    " FLMean(b.",vvalueColnames[1],") AS pred \n ",
                            " FROM( \n ",
                                " SELECT ",vobsidColnames[3]," AS obsidX,",
                                        vvaridColnames[3]," AS obsidY,",
                                        vvalueColnames[3]," AS dist \n ",
                                " FROM (",constructSelect(vDistMatrix),") a \n ",
                                " QUALIFY RANK()OVER(PARTITION BY ",vobsidColnames[3]," ORDER BY ",vvalueColnames[3],")<=",k,
                                " \n ) a, \n (",constructSelect(train),") b \n ",
                            " WHERE a.obsidY = b.",vobsidColnames[1],vAddWhereClause,
                            " GROUP BY a.obsidx) a \n "))
        }
    }
            

    vKNNResult <- genFLVector(pQuery=genResultQuery(classify=classify,cl=cl))

    if(prob && classify){
        vprob <- genFLVector(pQuery=genResultQuery(pColname="prob",classify=classify,cl=cl))
        attr(vKNNResult,"prob") <- vprob
    }
    return(vKNNResult)
}

#' @export
knn.FLMatrix <- knn.FLTable


## pMultiplier to populate the table vertically and
## horizontally
## Max allowed pMultipler=c(150,30)
benchMarkFLKNN <- function(pMultiplier=c(1,1),
                            ...
                            ){
    ## base case (1x) (100*5)
    if(pMultiplier[1]>150)
        pMultiplier[1] <- 1
    if(pMultiplier[2]>30)
        pMultiplier[2] <- 1

    vrows <- 100*pMultiplier[1]
    vcols <- 5*pMultiplier[2]

    select <- new("FLSelectFrom",
                connectionName = getFLConnectionName(),
                table_name = c(mtrx="ARTestMatrixTable"),
                variables=list(MATRIX_ID="'%insertIDhere%'",
                               rowIdColumn=paste0("mtrx.rowIdColumn"),
                               colIdColumn=paste0("mtrx.colIdColumn"),
                               valueColumn=paste0("mtrx.valueColumn")),
                whereconditions=c(paste0("mtrx.rowIdColumn < ",vrows+1),
                                  paste0("mtrx.colIdColumn < ",vcols+1)),
                order = "")
  
    flm <- newFLMatrix(select = select,
                       dims = as.integer(c(vrows,vcols)),
                       Dimnames = list(NULL,NULL),
                       dimColumns=c("MATRIX_ID","rowIdColumn",
                                    "colIdColumn","valueColumn"))
    
    rm <- matrix(rnorm(vrows*vcols),vrows)
    
    cl <- sample(1:3,nrow(flm),replace=TRUE)
    FLcl <- as.FL(c(cl))

    require(plyr)
    vbenchmarkResults <- ldply(list(flm,rm),
                                function(x){
                                    vtime <- system.time(
                                                    FLknnOutput <- tryCatch(knn(x,cl=FLcl,k=3,...),
                                                                        error=function(e)
                                                                            return(knn(x,x,cl=cl,k=3,...)))
                                                    )
                                    return(data.frame(rows=vrows,cols=vcols,
                                            dim=vrows*vcols,
                                            platform=ifelse(is.FL(x),"FL","R"),
                                            BenchmarkTime=vtime["elapsed"]))
                                })

    return(vbenchmarkResults)
}


benchMarkFLgetUpperDistMatrix <- function(pMultiplier=c(1,1),
                                        ...
                                        ){
    ## base case (1x) (100*5)
    if(pMultiplier[1]>150)
        pMultiplier[1] <- 1
    if(pMultiplier[2]>30)
        pMultiplier[2] <- 1

    vrows <- 100*pMultiplier[1]
    vcols <- 5*pMultiplier[2]


    FLdeepTbl <- FLTable(getTestTableName("tblLinRegr"),
                        "obsid","varid","num_val",
                        whereconditions=c(paste0("obsid < ",vrows+1),
                                        paste0("varid < ",vcols+1))
                        )

    require(plyr)
    vbenchmarkResults <- ldply(c("euclidean","manhattan"),
                                function(x){
                                    vtime <- system.time(
                                                    distMatrix <- FLgetUpperDistMatrix(pObj1=FLdeepTbl,
                                                                                        pObj2=FLdeepTbl,
                                                                                        metric=x,
                                                                                        temporary=TRUE,
                                                                                        outTableName=NULL,
                                                                                        ...))
                                    return(data.frame(rows=vrows,cols=vcols,
                                            dim=vrows*vcols,
                                            DistanceMetric=x,
                                            BenchmarkTime=vtime["elapsed"]))
                                })

    return(vbenchmarkResults)
}


runbenchMarkFLgetUpperDistMatrix <- function(pMultiplierLimit=c(10,5)){
    vincreaseLimit <- 5
    vrows <- seq(1,pMultiplierLimit[1],vincreaseLimit)
    vcols <- seq(1,pMultiplierLimit[2],vincreaseLimit)

    vcomb <- expand.grid(vrows,vcols)
    vres <- apply(vcomb,1,benchMarkFLgetUpperDistMatrix)
    vres <- ldply(vres,rbind)
    p1 <- ggplot(vres,aes(x=rows,y=BenchmarkTime,colour=DistanceMetric))+
                facet_grid(.~cols)+geom_line()+geom_point()+
                ylab("time(sec)")
    plot(p1)
    return(vres)
}

runbenchMarkFLKNN <- function(pMultiplierLimit=c(10,5),
                              pIncreaseLimit=5){
    vincreaseLimit <- pIncreaseLimit
    vrows <- seq(1,pMultiplierLimit[1],vincreaseLimit)
    vcols <- seq(1,pMultiplierLimit[2],vincreaseLimit)

    vcomb <- expand.grid(vrows,vcols)
    vres <- apply(vcomb,1,benchMarkFLKNN)
    vres <- ldply(vres,rbind)
    p1 <- ggplot(vres,aes(x=rows,y=BenchmarkTime,colour=platform))+
                facet_grid(.~cols)+geom_line()+geom_point()+
                ylab("time(sec)")
    plot(p1)
    return(vres)
}


#' k-Nearest Neighbour Indexes
#' For each row of the test set, the k nearest (according to distance metric speicified) 
#' training set vectors are found and indices,distances are returned.
#' @param data input deep FLTable
#' @param query input deep FLTable
#' @param k number of neighbours considered.
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
                                algorithm="kd_tree",
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

    pSearchTbl <- setAlias(pSearchTbl,"a")
    pDataTbl <- setAlias(pDataTbl,"b")

    ## get Column aliases
    vobsidColnames <- sapply(list(pSearchTbl,pDataTbl),getObsIdSQLExpression)
    vvaridColnames <- sapply(list(pSearchTbl,pDataTbl),getVarIdSQLExpression)
    vvalueColnames <- sapply(list(pSearchTbl,pDataTbl),getValueSQLExpression)

    ## Construct WhereClause
    getBatchWhere <- function(pBatchNum){
        pasteOperator <- function(lhs,rhs,op="<>")
            return(paste0(lhs," ",op," ",rhs))
        return(constructWhere(c(pasteOperator(vvaridColnames[1],vvaridColnames[2],"="),
                                where(pDataTbl),
                                where(pSearchTbl),
                                # pasteOperator(vobsidColnames[1],vobsidColnames[2]),
                                # pasteOperator(vvaridColnames[1],-1),
                                # pasteOperator(vvaridColnames[2],-1),
                                # pasteOperator(vvalueColnames[1],vvalueColnames[2]), ## fails if we match and remove points which are exactly
                                                                                    ## same.. in the where clause in FLKNN
                                pasteOperator(pasteOperator(vobsidColnames[1],
                                                            vBatchSize,"MOD"),
                                              pBatchNum,
                                             "=")
                            ))
                )
    }

    genResultQuery <- function(pBatchNum){
        paste0("SELECT ",vobsidColnames[1]," AS searchid, \n ",
                        vobsidColnames[2]," AS matchid, \n ",
                        "FLEuclideanDist(",vvalueColnames[1],",",
                                           vvalueColnames[2],") AS dist \n ",
                "FROM ",vtableNames[1]," a, \n ",
                        vtableNames[2]," b \n ",
                getBatchWhere(pBatchNum)," \n ",
                " QUALIFY ",k," >= ROW_NUMBER() OVER(PARTITION BY ",
                    vobsidColnames[1]," ORDER BY dist) \n ",
                " GROUP BY ",vobsidColnames[1],",",vobsidColnames[2])
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
