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
#' @param metric distance metric to be used. Euclidean,Manhattan supported.
#' @return FLVector of classifications of test set.
#' @examples
#' FLdeepTbl <- FLTable(getTestTableName("ARknnDevSmall"),"obsid","varid","num_val")
#' FLknnOutput <- knn(FLdeepTbl,FLdeepTbl,k=3,prob=TRUE)
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
                        metric="Euclidean",
                        ...){
    if(!isDeep(train))
        train <- FLRegrDataPrep(train,depCol=cl)
    if(is.null(test))
        test <- train
    if(!isDeep(test))
        test <- FLRegrDataPrep(test,depCol=cl)

    vtableNames <- sapply(list(train,test),getTableNameSlot)

    ## Calculate Dist Matrix
    vDistTableName <- gen_unique_table_name(paste0(vtableNames[1],"Dist"))

    vDistMatrix <- FLgetDistMatrix(test,train,
                                    metric=metric,
                                    outTableName=vDistTableName
                                    )

    ## get Column aliases
    vobsidColnames <- sapply(list(train,test,vDistMatrix),getObsIdSQLName)
    vvaridColnames <- sapply(list(train,test,vDistMatrix),getVarIdSQLName)
    vvalueColnames <- sapply(list(train,test,vDistMatrix),getValueSQLName)
    vtableNames <- c(vtableNames,getTableNameSlot(vDistMatrix))

    genFLVector <- function(pQuery){
        tblfunqueryobj <- new("FLTableFunctionQuery",
                                  connectionName = attr(getFLConnection(),"name"),
                                  variables = list(
                                      obs_id_colname = "vectorIndexColumn",
                                      cell_val_colname = "vectorValueColumn"),
                                  whereconditions="",
                                  order = "",
                                  SQLquery=pQuery)
        flv <- newFLVector(
                   select = tblfunqueryobj,
                   Dimnames = list(rownames(test),"vectorValueColumn"),
                   isDeep = FALSE,
                   type="double")
        flv
    }

    genResultQuery <- function(pColname=NULL,classify=classify){
        if(classify){
            if(is.null(pColname))
                pColname <- "predClass"
            return(paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                                    "a.obsid AS vectorIndexColumn, \n ",
                                    "a.",pColname," AS vectorValueColumn \n ",
                        " FROM \n ",
                            "(SELECT DISTINCT a.obsidX AS obsid,\n ",
                                    " b.",vvalueColnames[1]," AS predClass, \n ",
                                    " ((FLSUM(a.simIndex) OVER(PARTITION BY a.obsidX,",
                                        "b.",vvalueColnames[1],"))/(FLSUM(a.simIndex) ",
                                        "OVER(PARTITION BY a.obsidx))) AS prob \n ",
                            " FROM( \n ",
                                " SELECT ",vobsidColnames[3]," AS obsidX,",
                                        vvaridColnames[3]," AS obsidY, 1/",
                                        vvalueColnames[3]," AS simIndex \n ",
                                " FROM (",constructSelect(vDistMatrix),") a \n ",
                                " QUALIFY RANK()OVER(PARTITION BY ",vobsidColnames[3]," ORDER BY ",vvalueColnames[3],")<=",k,
                                " \n ) a, \n (",constructSelect(train),") b \n ",
                            " WHERE a.obsidY = b.",vobsidColnames[1]," \n AND b.",vvaridColnames[1]," = -1 \n ",
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
                                        vvaridColnames[3]," AS obsidY, 1/",
                                        vvalueColnames[3]," AS simIndex \n ",
                                " FROM (",constructSelect(vDistMatrix),") a \n ",
                                " QUALIFY RANK()OVER(PARTITION BY ",vobsidColnames[3]," ORDER BY ",vvalueColnames[3],")<=",k,
                                " \n ) a, \n (",constructSelect(train),") b \n ",
                            " WHERE a.obsidY = b.",vobsidColnames[1]," \n AND b.",vvaridColnames[1]," = -1 \n ",
                            " GROUP BY a.obsidx) a \n "))
        }
    }
            

    vKNNResult <- genFLVector(pQuery=genResultQuery(classify=classify))

    if(prob && classify){
        vprob <- genFLVector(pQuery=genResultQuery(pColname="prob",classify=classify))
        attr(vKNNResult,"prob") <- vprob
    }
    return(vKNNResult)
}

FLgetDistMatrix <- function(pObj1,
                            pObj2,
                            metric="Euclidean",
                            temporary=TRUE,
                            outTableName=NULL,
                            ...){
    flm <- FLgetUpperDistMatrix(pObj1=pObj1,
                                pObj2=pObj2,
                                metric=metric,
                                temporary=temporary,
                                outTableName=outTableName,
                                ...)

    vDimColumns <- c(getObsIdSQLName(flm),
                    getVarIdSQLName(flm),
                    getValueSQLName(flm))

    vsqlstr <- paste0("SELECT ",vDimColumns[2],",",vDimColumns[1],",",vDimColumns[3]," \n ",
                        " FROM (",constructSelect(flm),") a ")

    vtempResult <- insertIntotbl(pTableName=getTableNameSlot(flm),
                                pSelect=vsqlstr)
    return(flm)
}

FLgetUpperDistMatrix <- function(pObj1,
                                pObj2,
                                metric="Euclidean",
                                temporary=TRUE,
                                outTableName=NULL,
                                ...){

    if(is.FLTable(pObj1) && !isDeep(pObj1))
        pObj1 <- wideToDeep(pObj1)
    if(is.FLTable(pObj2) && !isDeep(pObj2))
        pObj2 <- wideToDeep(pObj2)

    vtableNames <- sapply(list(pObj1,pObj2),getTableNameSlot)
    vobsidColnames <- sapply(list(pObj1,pObj2),getObsIdSQLName)
    vvaridColnames <- sapply(list(pObj1,pObj2),getVarIdSQLName)
    vvalueColnames <- sapply(list(pObj1,pObj2),getValueSQLName)

    ## Create a distance matrix
    metric <- c(Euclidean="FLEuclideanDist",
                Manhattan="FLManhattanDist")[metric]

    if(!is.null(outTableName))
        vDistTableName <- outTableName
    else 
        vDistTableName <- gen_unique_table_name(paste0(vtableNames[1],"Dist"))

    vsqlstr <- paste0("SELECT a.",vobsidColnames[1]," AS rowIdColumn, \n ",
                            " b.",vobsidColnames[2]," AS colIdColumn, \n ",
                            metric,"(a.",vvalueColnames[1],",b.",vvalueColnames[2],
                                    ") AS valueColumn \n ",
                    " FROM(",constructSelect(pObj1),")a, \n ",
                         "(",constructSelect(pObj2),")b \n ",
                    " WHERE a.",vvaridColnames[1],"=b.",vvaridColnames[2],
                    " \n AND a.",vobsidColnames[1]," < b.",vobsidColnames[2]," \n ",
                    " AND a.",vvaridColnames[1]," <> -1 AND b.",vvaridColnames[2]," <> -1 \n ",
                    " GROUP BY a.",vobsidColnames[1],",b.",vobsidColnames[2])

    vtempResult <- createTable(pTableName=vDistTableName,
                                pSelect=vsqlstr,
                                temporary=temporary)

    select <- new("FLSelectFrom",
                  connectionName = attr(getFLConnection(),"name"),
                  table_name = vDistTableName,
                  variables=list(
                              Matrix_ID="'%insertIDhere%'",
                              rowIdColumn="rowIdColumn",
                              colIdColumn="colIdColumn",
                              valueColumn="valueColumn"),
                  whereconditions="",
                  order = "")
    
    flm  <- newFLMatrix(
                  select = select,
                  dims = c(nrow(pObj1),nrow(pObj2)),
                  Dimnames = list(rownames(pObj1),
                                rownames(pObj2)),
                  type="double")
    return(flm)
}

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


    FLdeepTbl <- FLTable(getTestTableName("tblLinRegr"),
                        "obsid","varid","num_val",
                        whereconditions=c(paste0("obsid < ",vrows+1),
                                        paste0("varid < ",vcols+1))
                        )

    require(plyr)
    vbenchmarkResults <- ldply(c("Euclidean","Manhattan"),
                                function(x){
                                    vtime <- system.time(
                                                    FLknnOutput <- knn(FLdeepTbl,
                                                                        FLdeepTbl,
                                                                        metric=x,
                                                                        ...)
                                                )
                                    return(data.frame(rows=vrows,cols=vcols,
                                            dim=vrows*vcols,
                                            DistanceMetric=x,
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
    vbenchmarkResults <- ldply(c("Euclidean","Manhattan"),
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

