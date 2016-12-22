#' @include FLMatrix.R
#' @include apply.R
NULL

FLStatsDist <- function(x,method="euclidean",
                        diag=FALSE,
                        upper=FALSE,
                        p=2,
                        vobsidCol=NULL,
                        vvaridCol=NULL,
                        vvalueCol=NULL,
                        functionName){
    if(!is.null(vvaridCol))
    vwhereConditions <- paste0("a.",vvaridCol," = b.",vvaridCol)
    # if(diag && !upper)
    # vwhereConditions <- c(vwhereConditions,
    #     paste0("a.",vobsidCol," >= b.",vobsidCol))
    # else if(upper && !diag)
    # vwhereConditions <- c(vwhereConditions,
    #     paste0("a.",vobsidCol," <> b.",vobsidCol))
    # else if(!diag && !upper)
    # vwhereConditions <- c(vwhereConditions,
    #     paste0("a.",vobsidCol," > b.",vobsidCol))

    sqlstr <- paste0("SELECT '%insertIDhere%' AS matrixIdColumn,\n",
                            "a.",vobsidCol," AS rowIdColumn,\n",
                            "b.",vobsidCol," AS colIdColumn,\n",
                            functionName,"(a.",vvalueCol,",b.",vvalueCol,") AS valueColumn\n",
                        "FROM (",constructSelect(x),") a,(",
                                constructSelect(x),") b \n",
                        constructWhere(vwhereConditions),"\n",
                        " GROUP BY a.",vobsidCol,",b.",vobsidCol)

    tblfunqueryobj <- new("FLTableFunctionQuery",
                        connectionName = getFLConnectionName(),
                        variables=list(
                            rowIdColumn="rowIdColumn",
                            colIdColumn="colIdColumn",
                            valueColumn="valueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)
        flm <- newFLMatrix(
                           select= tblfunqueryobj,
                           dims=as.integer(c(x@dims[1],x@dims[1])),
                           Dimnames=list(rownames(x),rownames(x)))

        return(ensureQuerySize(pResult=flm,
                        pInput=list(x,method,
                                    diag,upper,
                                    p,vobsidCol,
                                    vvaridCol,
                                    vvalueCol),
                        pOperator="FLStatsDist"))
    }

NULL
#' Distance Matrix Computation
#'
#' @section Constraints:
#' only manhattan and euclidean are supported currently.
#' 
#' @examples
#' flmatrix <- FLMatrix("FL_DEM.tblMatrixMulti", 1,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' dist(flmatrix)
#' dist(flmatrix,diag=TRUE)
#' dist(flmatrix,upper=TRUE)
#' dist(flmatrix,diag=TRUE,upper=TRUE)
#' dist(flmatrix,"manhattan",TRUE,TRUE)
#' @export
setMethod("dist",signature(x="FLMatrix"),
    function(x,method="euclidean",
            diag=TRUE,
            upper=TRUE,
            p=2){
    if(method=="euclidean")
    functionName <- "FLEuclideanDist"
    else if(method=="manhattan")
    functionName <- "FLManhattanDist"
    else stop("euclidean and manhattan methods are only supported")

    return(FLStatsDist(x=x,method=method,
                        diag=diag,
                        upper=upper,
                        p=p,
                        vobsidCol="rowIdColumn",
                        vvaridCol="colIdColumn",
                        vvalueCol="valueColumn",
                        functionName=functionName))
        })

#' @export
setMethod("dist",signature(x="FLVector"),
    function(x,method="euclidean",
            diag=TRUE,
            upper=TRUE,
            p=2){
    if(method=="euclidean")
    functionName <- "FLEuclideanDist"
    else if(method=="manhattan")
    functionName <- "FLManhattanDist"
    else stop("euclidean and manhattan methods are only supported")

    return(FLStatsDist(x=x,method=method,
                        diag=diag,
                        upper=upper,
                        p=p,
                        vobsidCol="vectorIndexColumn",
                        vvalueCol="vectorValueColumn",
                        functionName=functionName))
        })

#' @export
setMethod("dist",signature(x="FLTable"),
    function(x,method="euclidean",
            diag=TRUE,
            upper=TRUE,
            p=2){
    if(method=="euclidean")
    functionName <- "FLEuclideanDist"
    else if(method=="manhattan")
    functionName <- "FLManhattanDist"
    else stop("euclidean and manhattan methods are only supported")

    return(FLStatsDist(x=x,method=method,
                        diag=diag,
                        upper=upper,
                        p=p,
                        vobsidCol="obs_id_colname",
                        vvaridCol="var_id_colname",
                        vvalueCol="cell_val_colname",
                        functionName=functionName))
        })


##################### weighted mean,sd,var #################################
FLWtGeneric <- function(x,w,functionName){
    if(length(w)!=length(x))
        stop("length of x and w should be same")
    else if(is.vector(w))
        w <- as.FLVector(w)

    sqlstr <- paste0("SELECT ",functionName,"(b.vectorValueColumn,a.vectorValueColumn) \n",
                        " FROM (",constructSelect(x)," ) a,\n",
                            "(",constructSelect(w),") b \n",
                        " WHERE a.vectorIndexColumn = b.vectorIndexColumn")
    return(sqlQuery(getFLConnection(),sqlstr)[[1]])
}

#' @export
weighted.mean.FLVector <- function(x,w=rep(1/length(x),length(x)),...){
    if(missing(w))
        return(mean(x))
    else return(FLWtGeneric(x=x,w=w,functionName="FLWtAvg"))
}

#' @export
setGeneric("wt.sd",function(x,wt,...)
                standardGeneric("wt.sd"))
setMethod("wt.sd",signature(x="FLVector"),
    function(x,
            wt = rep(1/length(x),length(x)),...){
        if(missing(wt))
        return(sd(x))
        else return(FLWtGeneric(x=x,w=wt,functionName="FLWtStdDev"))
        })
setMethod("wt.sd",signature(x="ANY"),
        function(x,
                wt = rep(1/length(x),length(x)),...){
                    if (!requireNamespace("SDMTools", quietly = TRUE)){
                        stop("SDMTools needed for wt.sd. Please install it.",
                        call. = FALSE)
                        }
                    else return(SDMTools::wt.sd(x,wt,...))
        })

#' @export
setGeneric("wt.var",function(x,wt,...)
                standardGeneric("wt.var"))
setMethod("wt.var",signature(x="FLVector"),
    function(x,
            wt = rep(1/length(x),length(x)),...){
        if(missing(wt))
        return(sd(x))
        else return(FLWtGeneric(x=x,w=wt,functionName="FLWtVar"))
        })
setMethod("wt.var",signature(x="ANY"),
        function(x,
                wt = rep(1/length(x),length(x)),...){
                    if (!requireNamespace("SDMTools", quietly = TRUE)){
                        stop("SDMTools needed for wt.var. Please install it.",
                        call. = FALSE)
                        }
                    else return(SDMTools::wt.var(x,wt,...))
        })

#' @export
setGeneric("wt.mean",function(x,wt,...)
                standardGeneric("wt.mean"))
setMethod("wt.mean",signature(x="FLVector"),
    function(x,
            wt = rep(1/length(x),length(x)),...){
        if(missing(wt))
        return(sd(x))
        else return(FLWtGeneric(x=x,w=wt,functionName="FLWtAvg"))
        })
setMethod("wt.mean",signature(x="ANY"),
        function(x,
                wt = rep(1/length(x),length(x)),...){
                    if (!requireNamespace("SDMTools", quietly = TRUE)){
                        stop("SDMTools needed for wt.mean. Please install it.",
                        call. = FALSE)
                        }
                    else return(SDMTools::wt.mean(x,wt,...))
        })

######################### sd ###########################################
#' @export
setGeneric("sd",function(x,na.rm=TRUE)
                standardGeneric("sd"))
FLaggregate <- function(x,na.rm=TRUE,FLfunc){
    return(paste0(" ",FLfunc,"(", getValueSQLExpression(x),") "))
}
setMethod("sd",signature(x="FLIndexedValues"),
    function(x,na.rm=TRUE){
        return(genAggregateFunCall(object=x, func=FLaggregate,FLfunc="FLStdDev"))})

########################## FLSdP ##################################
#' @export
setGeneric("FLSdP",function(x,na.rm=TRUE)
                standardGeneric("FLSdP"))
setMethod("FLSdP",signature(x="FLIndexedValues"),
    function(x,na.rm=TRUE){
        return(genAggregateFunCall(object=x,func=FLaggregate,FLfunc="FLStdDevP"))})

######################## skewness #####################################
#' @export
setGeneric("skewness",function(x,na.rm=FALSE)
                standardGeneric("skewness"))
setMethod("skewness",signature(x="FLIndexedValues"),
    function(x,na.rm=TRUE){
        return(genAggregateFunCall(object=x, func=FLaggregate,FLfunc="FLSkewness"))})
setMethod("skewness",signature(x="ANY"),
    function(x,na.rm=TRUE){
        return(moments::skewness(x=x,na.rm=na.rm))})

##################### kurtosis ##########################################
#' @export
setGeneric("kurtosis",function(x,na.rm=FALSE)
                standardGeneric("kurtosis"))
setMethod("kurtosis",signature(x="FLIndexedValues"),
    function(x,na.rm=FALSE){
        return(genAggregateFunCall(object=x, func=FLaggregate,FLfunc="FLKurtosis"))})
setMethod("kurtosis",signature(x="ANY"),
    function(x,na.rm=FALSE){
        return(moments::kurtosis(x=x,na.rm=na.rm))})

## gk: refactor sum and prod to a expression object
######################### prod ############################################
mixedAggregate <- function(...,Rfun,FLfun,na.rm=FALSE){
    nums <- do.call(c,lapply(list(...), function(x){
        if(is.numeric(x)) return(x)}))
    vtemp <- lapply(list(...), function(x){
        if(inherits(x,"FLIndexedValues")){
            return(genAggregateFunCall(object=x,
                                       func=FLaggregate,FLfun=FLfun))
        }
    })
    vtemp <- vtemp[!sapply(vtemp,is.null)]
    if(length(nums)>0){
        fn <- strsplit(Rfun, "::")[[1]]
        myfun <- if (length(fn)==1) fn[[1]] else get(fn[[2]], asNamespace(fn[[1]]))
        vtemp[[length(vtemp)+1]] <- do.call(myfun,list(nums,na.rm=na.rm))
    }
    if(length(vtemp)==1)
        return(vtemp[[1]])
    else
        return(new("FLSkalarAggregate",func=Rfun,arguments=vtemp))
}

#' @export
prod <- function(...,na.rm=FALSE) mixedAggregate(...,Rfun="base::prod",FLfun="FLProd")
####################### sum ###############################################
#' @export
sum <- function(...,na.rm=FALSE) mixedAggregate(...,Rfun="base::sum",FLfun="FLSum")

######################### max #############################################
FLMax <- function(x,na.rm=FALSE,...)
    UseMethod("FLMax")
FLMax.FLAbstractColumn <- function(x,na.rm=FALSE,...){
    return(paste0(" FLMax(",getValueSQLExpression(x),") "))
}

FLMax.FLIndexedValues <- function(x,...){
    x <- genAggregateFunCall(x,func=FLaggregate,
                               FLfun="FLMax")
    return(x)
}


FLMax.FLMatrix <- function(x,...){
    return(genAggregateFunCall(x,func=FLMax))
}

#' @export
max <- function(...,na.rm=FALSE){
    mixedAggregate(...,Rfun="base::max",FLfun="FLMax")
    # vlist <- list(...)
    # vtemp <- unlist(lapply(vlist,function(x)is.FL(x)))
    # if(!any(vtemp))
    # return(base::max(...,na.rm=na.rm))

    # vprod <- lapply(list(...),function(x){
    #             if(is.FL(x)){
    #                 return(FLMax(x,na.rm=na.rm))
    #             }
    #             else return(base::max(x,na.rm=na.rm))
    #         })
    # return(vprod[[1]])
    # vprod <- lapply(vprod,as.vector)
    # return(do.call(base::max,vprod))
}

####################### min ##################################################
min.FLAbstractColumn <- function(x,na.rm=FALSE){
    return(paste0(" FLMin(",getValueSQLExpression(x),") "))
}

#' @export
min <- function(...,na.rm=FALSE){
    vlist <- list(...)
    vtemp <- unlist(lapply(vlist,function(x)is.FL(x)))
    if(!any(vtemp))
    return(base::min(...,na.rm=na.rm))

    vprod <- sapply(list(...),function(x){
                if(is.FLAbstractColumn(x)){
                    return(min.FLAbstractColumn(x=x,
                                    na.rm=na.rm))
                }
                else if(is.FL(x)){
                    return(genAggregateFunCall(object=x,
                                func=min.FLAbstractColumn))
                }
                else return(base::min(x,na.rm=na.rm))
            })
    return(base::min(vprod,na.rm=na.rm))
}

#################### which.max #################################################
#' @export
setGeneric("which.max",function(x)
                standardGeneric("which.max"))
which.max.FLAbstractColumn <- function(x){
    return(paste0(" FLMaxAt(",
                paste0(x@columnName,collapse=","),") "))
}
setMethod("which.max",signature(x="FLVector"),
    function(x){
        return(genAggregateFunCall(object=x,
                                func=which.max.FLAbstractColumn,
                                indexCol=TRUE))})
setMethod("which.max",signature(x="FLAbstractColumn"),
    function(x){
        return(which.max.FLAbstractColumn(x))})
setMethod("which.max",signature(x="FLMatrix"),
    function(x){
        return(genAggregateFunCall(object=x,
                                func=which.max.FLAbstractColumn,
                                indexCol=TRUE))})
setMethod("which.max",signature(x="FLTable"),
    function(x){
        return(genAggregateFunCall(object=x,
                                func=which.max.FLAbstractColumn,
                                indexCol=TRUE))})

##################### which.min ##################################################
#' @export
setGeneric("which.min",function(x)
                standardGeneric("which.min"))
which.min.FLAbstractColumn <- function(x){
    return(paste0(" FLMinAt(",
                paste0(x@columnName,collapse=","),") "))
}
setMethod("which.min",signature(x="FLVector"),
    function(x){
        return(genAggregateFunCall(object=x,
                                func=which.min.FLAbstractColumn,
                                indexCol=TRUE))})
setMethod("which.min",signature(x="FLAbstractColumn"),
    function(x){
        return(which.min.FLAbstractColumn(x))})
setMethod("which.min",signature(x="FLMatrix"),
    function(x){
        return(genAggregateFunCall(object=x,
                                func=which.min.FLAbstractColumn,
                                indexCol=TRUE))})
setMethod("which.min",signature(x="FLTable"),
    function(x){
        return(genAggregateFunCall(object=x,
                                func=which.min.FLAbstractColumn,
                                indexCol=TRUE))})


################ geometric.mean #############################################
#' @export
setGeneric("geometric.mean",function(x,na.rm=TRUE)
                standardGeneric("geometric.mean"))
setMethod("geometric.mean",signature(x="FLIndexedValues"),
    function(x,na.rm=FALSE){
    return(genAggregateFunCall(object=x,
                               func=FLaggregate,FLfunc="FLGeoMean"))})
setMethod("geometric.mean",signature(x="ANY"),
    function(x,na.rm=FALSE){
        if (!requireNamespace("psych", quietly = TRUE)){
            stop("psych package needed for geometric.mean. Please install it.",
            call. = FALSE)
            }
        else return(psych::geometric.mean(x=x,na.rm=na.rm))
    })

############################## harmonic.mean #####################################
#' @export
setGeneric("harmonic.mean",function(x,na.rm=TRUE)
                standardGeneric("harmonic.mean"))
setMethod("harmonic.mean",signature(x="FLIndexedValues"),
    function(x,na.rm=FALSE){
        return(genAggregateFunCall(object=x,
                                func=FLaggregate,FLfunc="FLHarMean"))})
setMethod("harmonic.mean",signature(x="ANY"),
    function(x,na.rm=FALSE){
        if (!requireNamespace("psych", quietly = TRUE)){
            stop("psych package needed for harmonic.mean. Please install it.",
            call. = FALSE)
            }
        else return(psych::harmonic.mean(x=x,na.rm=na.rm))
    })

###################### UDT ##########################################################
getDescStatsUDT <- function(object,
                            functionName,
                            outCol,
                            viewCols,
                            outFLVector=FALSE){
    if(is.FLTable(object) && !isDeep(object))
        object <- wideToDeep(object)
    # sqlstr <- paste0("WITH z (",paste0(names(viewCols),collapse=","),") AS ( \n ",
    #                 " SELECT ",paste0(viewCols,collapse=",")," \n ",
    #                 " FROM(",constructSelect(object,order=FALSE),") a) \n ",
    #                 " SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
    #                     paste0("a.",outCol," AS ",names(outCol),collapse=",")," \n ",
    #                 " FROM \n ",
    #                 " TABLE (",functionName,"(",paste0("z.",names(viewCols),collapse=","),") \n ",
    #                 " HASH BY ",paste0("z.",names(viewCols)[1])," \n ",
    #                 " LOCAL ORDER BY ",paste0("z.",names(viewCols)[1]),") AS a \n ")
    vMap <- getMatrixUDTMapping(functionName)
    pOutColnames <- vMap$argsPlatform
    if(!"vectorIdColumn" %in% names(pOutColnames))
        pOutColnames <- c("vectorIdColumn"="'%insertIDhere%'",pOutColnames)
    pOutColnames <- as.list(pOutColnames)
    pFuncName <- vMap$funcNamePlatform
    
    sqlstr <- constructUDTSQL(pViewColnames=as.list(viewCols),
                              pFuncName=pFuncName,
                              pSelect=constructSelect(object,order=FALSE),
                              pOutColnames=pOutColnames,
                              pNest=TRUE
                              )

    if(!outFLVector){
        vresult <- sqlQuery(getFLConnection(),sqlstr)
        colnames(vresult) <- tolower(colnames(vresult))
        return(vresult[["vectorvaluecolumn"]])
    }
    else{
        tblfunqueryobj <- new("FLTableFunctionQuery",
                        connectionName = attr(connection,"name"),
                        variables = list(
                        obs_id_colname = "vectorIndexColumn",
                        cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)
        vlength <- ifelse(is.FLVector(object),
                        length(object),
                        nrow(object)*ncol(object))

        return(store(newFLVector(
                select=tblfunqueryobj,
                Dimnames=list(1:vlength,"vectorValueColumn"),
                isDeep=FALSE)))
    }
}

######################### mode ##################################################
#' @export
setGeneric("mode",function(x,na.rm=TRUE)
                standardGeneric("mode"))

setMethod("mode",signature(x="FLIndexedValues"),
    function(x,na.rm=FALSE){
        return(getDescStatsUDT(object=x,
                                functionName="FLModeUdt",
                                outCol=c(vectorValueColumn="oMode"),
                                viewCols=c(pGroupID=1,
                                        pValue=getValueSQLName(x))))
    })

setMethod("mode",signature(x="FLVector.Hadoop"),
    function(x,na.rm=FALSE){
        return(getDescStatsUDT(object=x,
                                functionName="FLModeUdt",
                                outCol=c(vectorValueColumn="oMode"),
                                viewCols=c(pGroupID=1,
                                        pValue=paste0("CAST(",getValueSQLName(x)," AS DOUBLE)"))))
    })
setMethod("mode",signature(x="FLMatrix.Hadoop"),
    function(x,na.rm=FALSE){
        return(getDescStatsUDT(object=x,
                                functionName="FLModeUdt",
                                outCol=c(vectorValueColumn="oMode"),
                                viewCols=c(pGroupID=1,
                                        pValue=paste0("CAST(",getValueSQLName(x)," AS DOUBLE)"))))
    })

setMethod("mode",signature(x="FLTable.Hadoop"),
    function(x,na.rm=FALSE){
        return(getDescStatsUDT(object=x,
                                functionName="FLModeUdt",
                                outCol=c(vectorValueColumn="oMode"),
                                viewCols=c(pGroupID=1,
                                    pValue="CAST(cell_val_colname AS DOUBLE)")))})

setMethod("mode",signature(x="FLTable"),
    function(x,na.rm=FALSE){
        return(getDescStatsUDT(object=x,
                                functionName="FLModeUdt",
                                outCol=c(vectorValueColumn="oMode"),
                                viewCols=c(pGroupID=1,
                                    pValue="cell_val_colname")))})

setMethod("mode",signature(x="ANY"),
    function(x,na.rm=FALSE){
        x <- x[!is.na(x)]
        vcount <- plyr::count(df=x)
        vmaxCount <- max(vcount[,"freq"])
        return(vcount[vcount[,"freq"]==vmaxCount,"x"])
        })

######################### median ################################################
#' @export
setGeneric("median",function(x,na.rm=TRUE)
                standardGeneric("median"))

setMethod("median",signature(x="FLIndexedValues"),
    function(x,na.rm=FALSE){
        return(getDescStatsUDT(object=x,
                                functionName="FLMedianUdt",
                                outCol=c(vectorValueColumn="oMedian"),
                                viewCols=c(pGroupID=1,
                                    pValue=getValueSQLName(x))))})

setMethod("median",signature(x="FLVector.Hadoop"),
    function(x,na.rm=FALSE){
        return(getDescStatsUDT(object=x,
                                functionName="FLMedianUdt",
                                outCol=c(vectorValueColumn="oMedian"),
                                viewCols=c(pGroupID=1,
                                        pValue=paste0("CAST(",getValueSQLName(x)," AS DOUBLE)"))))
    })
setMethod("median",signature(x="FLMatrix.Hadoop"),
    function(x,na.rm=FALSE){
        return(getDescStatsUDT(object=x,
                                functionName="FLMedianUdt",
                                outCol=c(vectorValueColumn="oMedian"),
                                viewCols=c(pGroupID=1,
                                        pValue=paste0("CAST(",getValueSQLName(x)," AS DOUBLE)"))))
    })

setMethod("median",signature(x="FLTable.Hadoop"),
    function(x,na.rm=FALSE){
        return(getDescStatsUDT(object=x,
                                functionName="FLMedianUdt",
                                outCol=c(vectorValueColumn="oMedian"),
                                viewCols=c(pGroupID=1,
                                            pValue="CAST(cell_val_colname AS DOUBLE)")))})

setMethod("median",signature(x="FLTable"),
    function(x,na.rm=FALSE){
        return(getDescStatsUDT(object=x,
                                functionName="FLMedianUdt",
                                outCol=c(vectorValueColumn="oMedian"),
                                viewCols=c(pGroupID=1,
                                            pValue="cell_val_colname")))})

########################### quantile ###########################################
#' @export
setGeneric("quantile",function(x,probs=c(0,0.25,0.5,0.75,1),
                                na.rm=FALSE,names=TRUE,
                                type=7,...)
                standardGeneric("quantile"))

setMethod("quantile",signature(x="FLVector"),
    function(x,probs=c(0,0.25,0.5,0.75,1),
                na.rm=FALSE,names=TRUE,
                type=7,...){
        probs <- as.vector(probs)
        vtemp <- sapply(probs,function(y){
                    if(is.na(y)) return(NA)
                    getDescStatsUDT(object=x,
                                    functionName="FLPercUdt",
                                    outCol=c(vectorValueColumn="oPercVal"),
                                    viewCols=c(pGroupID=1,
                                                pValue=getValueSQLName(x),
                                                pPerc=y)
                                    )
                    })
        names(vtemp) <- sapply(probs,function(y)
                        ifelse(is.na(y),"",
                            paste0(y*100,"%")))
        return(vtemp)
        })
setMethod("quantile",signature(x="FLVector.Hadoop"),
    function(x,probs=c(0,0.25,0.5,0.75,1),
                na.rm=FALSE,names=TRUE,
                type=7,...){
        probs <- as.vector(probs)
        vtemp <- sapply(probs,function(y){
                    if(is.na(y)) return(NA)
                    getDescStatsUDT(object=x,
                                    functionName="FLPercUdt",
                                    outCol=c(vectorValueColumn="oPercVal"),
                                    viewCols=c(pGroupID=1,
                                                pValue=paste0("CAST( ",getValueSQLName(x)," AS DOUBLE)"),
                                                pPerc=paste0("CAST(",y," AS DOUBLE)"))
                                    )
                    })
        names(vtemp) <- sapply(probs,function(y)
                        ifelse(is.na(y),"",
                            paste0(y*100,"%")))
        return(vtemp)
        })
setMethod("quantile",signature(x="FLMatrix"),
    function(x,probs=c(0,0.25,0.5,0.75,1),
                na.rm=FALSE,names=TRUE,
                type=7,...){
        probs <- as.vector(probs)
        vtemp <- sapply(probs[!is.na(probs)],function(y){
                    if(is.na(y)) return(NA)
                    getDescStatsUDT(object=x,
                                    functionName="FLPercUdt",
                                    outCol=c(vectorValueColumn="oPercVal"),
                                    viewCols=c(pGroupID=1,
                                                pValue=getValueSQLName(x),
                                                pPerc=y)
                                    )
                    })
        names(vtemp) <- sapply(probs,function(y)
                                        ifelse(is.na(y),"",
                                                paste0(y*100,"%")))
        return(vtemp)
        })

setMethod("quantile",signature(x="FLMatrix.Hadoop"),
    function(x,probs=c(0,0.25,0.5,0.75,1),
                na.rm=FALSE,names=TRUE,
                type=7,...){
        probs <- as.vector(probs)
        vtemp <- sapply(probs[!is.na(probs)],function(y){
                    if(is.na(y)) return(NA)
                    getDescStatsUDT(object=x,
                                    functionName="FLPercUdt",
                                    outCol=c(vectorValueColumn="oPercVal"),
                                    viewCols=c(pGroupID=1,
                                                pValue=paste0("CAST( ",getValueSQLName(x)," AS DOUBLE)"),
                                                pPerc=paste0("CAST(",y," AS DOUBLE)"))
                                    )
                    })
        names(vtemp) <- sapply(probs,function(y)
                                        ifelse(is.na(y),"",
                                                paste0(y*100,"%")))
        return(vtemp)
        })

setMethod("quantile",signature(x="FLTable"),
    function(x,probs=c(0,0.25,0.5,0.75,1),
                na.rm=FALSE,names=TRUE,
                type=7,...){
        probs <- as.vector(probs)
        vtemp <- sapply(probs[!is.na(probs)],function(y){
                    if(is.na(y)) return(NA)
                    getDescStatsUDT(object=x,
                                    functionName="FLPercUdt",
                                    outCol=c(vectorValueColumn="oPercVal"),
                                    viewCols=c(pGroupID=1,
                                                pValue="cell_val_colname",
                                                pPerc=y)
                                    )
                    })
        names(vtemp) <- sapply(probs,function(y)
                        ifelse(is.na(y),"",
                            paste0(y*100,"%")))
        return(vtemp)
        })
setMethod("quantile",signature(x="FLTable.Hadoop"),
    function(x,probs=c(0,0.25,0.5,0.75,1),
                na.rm=FALSE,names=TRUE,
                type=7,...){
        probs <- as.vector(probs)
        vtemp <- sapply(probs[!is.na(probs)],function(y){
                    if(is.na(y)) return(NA)
                    getDescStatsUDT(object=x,
                                    functionName="FLPercUdt",
                                    outCol=c(vectorValueColumn="oPercVal"),
                                    viewCols=c(pGroupID=1,
                                                pValue="CAST(cell_val_colname AS DOUBLE)",
                                                pPerc=paste0("CAST(",y," AS DOUBLE)"))
                                    )
                    })
        names(vtemp) <- sapply(probs,function(y)
                        ifelse(is.na(y),"",
                            paste0(y*100,"%")))
        return(vtemp)
        })

setMethod("quantile",signature(x="ANY"),
    function(x,probs=c(0,0.25,0.5,0.75,1),
                na.rm=FALSE,names=TRUE,
                type=7,...){
        return(stats::quantile(x=x,probs=probs,
                na.rm=na.rm,names=names,
                type=type,...))
        })


################################ percent #####################################
#' @export
setGeneric("percent",function(x,...)
                standardGeneric("percent"))

setMethod("percent",signature(x="FLVector"),
    function(x,...){
           return(getDescStatsUDT(object=x,
                                functionName="FLPercentUdt",
                                outCol=c(vectorIndexColumn="oObsID",
                                        vectorValueColumn="oPercentVal"),
                                viewCols=c(pGroupID=1,
                                            pObsID="vectorIndexColumn",
                                            pVal="vectorValueColumn"),
                                outFLVector=TRUE)
                )
        })

setMethod("percent",signature(x="FLMatrix"),
    function(x,...){
           return(getDescStatsUDT(object=x,
                    functionName="FLPercentUdt",
                    outCol=c(vectorIndexColumn="oObsID",
                            vectorValueColumn="oPercentVal"),
                    viewCols=c(pGroupID=1,
                        pObsID="ROW_NUMBER()OVER(ORDER BY colIdColumn,rowIdColumn)",
                        pVal="valueColumn"),
                    outFLVector=TRUE))
        })

setMethod("percent",signature(x="FLTable"),
    function(x,...){
           return(getDescStatsUDT(object=x,
                    functionName="FLPercentUdt",
                    outCol=c(vectorIndexColumn="oObsID",
                            vectorValueColumn="oPercentVal"),
                    viewCols=c(pGroupID=1,
                        pObsID="ROW_NUMBER()OVER(ORDER BY obs_id_colname,var_id_colname)",
                        pVal="cell_val_colname"),
                    outFLVector=TRUE))
        })

setMethod("percent",signature(x="ANY"),
    function(x,...){
        if(any(suppressWarnings(is.na(as.numeric(x)))))
        stop("x must be numeric \n ")
        return(as.vector(x)/sum(x))})


############################### deviation #########################################
## method can be c("mean-abs","median-abs","mean-square")
#' @export
setGeneric("deviation",function(x,
                            method="mean-abs",
                            average=TRUE)
                standardGeneric("deviation"))

selectDeviationMethod <- function(method){
    vmethod <- c("mean-abs","median-abs","mean-square")
    if(!method %in% vmethod)
    stop("method should be in ",vmethod," \n ")

    if(base::grepl("mean",method)){
        vfunction <- "FLMeanAbsDevUdt"
        voutcol <- "oMeanAbsDev"
    }
    else {
        vfunction <- "FLMedianAbsDevUdt"
        voutcol <- "oMedianAbsDev"
    }
    return(c(vfunction=vfunction,
            voutcol=voutcol))
}

setMethod("deviation",signature(x="FLVector"),
    function(x,
            method="mean-abs",
            average=TRUE){

        vtemp <- selectDeviationMethod(method=method)
        vfunction <- vtemp["vfunction"]
        voutcol <- vtemp["voutcol"]
        names(vfunction) <- NULL
        names(voutcol) <- NULL

        if(method=="mean-square")
        return(ifelse(average,
                FLDevSq(x=x)/length(x),
                FLDevSq(x=x)))

        vlength <- ifelse(average,1,length(x))
        return(getDescStatsUDT(object=x,
                                functionName=vfunction,
                                outCol=c(vectorValueColumn=voutcol),
                                viewCols=c(pGroupID=1,
                                    pValue=getValueSQLName(x)))*vlength)
        })
setMethod("deviation",signature(x="FLVector.Hadoop"),
    function(x,
            method="mean-abs",
            average=TRUE){

        vtemp <- selectDeviationMethod(method=method)
        vfunction <- vtemp["vfunction"]
        voutcol <- vtemp["voutcol"]
        names(vfunction) <- NULL
        names(voutcol) <- NULL

        if(method=="mean-square")
        return(ifelse(average,
                FLDevSq(x=x)/length(x),
                FLDevSq(x=x)))

        vlength <- ifelse(average,1,length(x))
        return(getDescStatsUDT(object=x,
                                functionName=vfunction,
                                outCol=c(vectorValueColumn=voutcol),
                                viewCols=c(pGroupID=1,
                                        pValue=paste0("CAST(",getValueSQLName(x)," AS DOUBLE)"))
                                )*vlength
                )
        })
setMethod("deviation",signature(x="FLMatrix"),
    function(x,
            method="mean-abs",
            average=TRUE){
        vtemp <- selectDeviationMethod(method=method)
        vfunction <- vtemp["vfunction"]
        voutcol <- vtemp["voutcol"]
        names(vfunction) <- NULL
        names(voutcol) <- NULL

        if(method=="mean-square")
        return(ifelse(average,
                FLDevSq(x=x)/length(x),
                FLDevSq(x=x)))

        vlength <- ifelse(average,1,length(x))
        return(getDescStatsUDT(object=x,
                                functionName=vfunction,
                                outCol=c(vectorValueColumn=voutcol),
                                viewCols=c(pGroupID=1,
                                    pValue=getValueSQLName(x))
                    )*vlength)
        })
setMethod("deviation",signature(x="FLMatrix.Hadoop"),
    function(x,
            method="mean-abs",
            average=TRUE){
        vtemp <- selectDeviationMethod(method=method)
        vfunction <- vtemp["vfunction"]
        voutcol <- vtemp["voutcol"]
        names(vfunction) <- NULL
        names(voutcol) <- NULL

        if(method=="mean-square")
        return(ifelse(average,
                FLDevSq(x=x)/length(x),
                FLDevSq(x=x)))

        vlength <- ifelse(average,1,length(x))
        return(getDescStatsUDT(object=x,
                                functionName=vfunction,
                                outCol=c(vectorValueColumn=voutcol),
                                viewCols=c(pGroupID=1,
                                    pValue=paste0("CAST(",getValueSQLName(x)," AS DOUBLE)"))
                                )
                    *vlength)
        })

setMethod("deviation",signature(x="FLTable"),
    function(x,
            method="mean-abs",
            average=TRUE){
        vtemp <- selectDeviationMethod(method=method)
        vfunction <- vtemp["vfunction"]
        voutcol <- vtemp["voutcol"]
        names(vfunction) <- NULL
        names(voutcol) <- NULL

        if(method=="mean-square")
        return(ifelse(average,
                FLDevSq(x=x)/(nrow(x)*ncol(x)),
                FLDevSq(x=x)))

        vlength <- ifelse(average,1,(nrow(x)*ncol(x)))
        return(getDescStatsUDT(object=x,
                                functionName=vfunction,
                                outCol=c(vectorValueColumn=voutcol),
                                viewCols=c(pGroupID=1,
                                    pValue="cell_val_colname"))*vlength)
        })
setMethod("deviation",signature(x="FLTable.Hadoop"),
    function(x,
            method="mean-abs",
            average=TRUE){
        vtemp <- selectDeviationMethod(method=method)
        vfunction <- vtemp["vfunction"]
        voutcol <- vtemp["voutcol"]
        names(vfunction) <- NULL
        names(voutcol) <- NULL

        if(method=="mean-square")
        return(ifelse(average,
                FLDevSq(x=x)/(nrow(x)*ncol(x)),
                FLDevSq(x=x)))

        vlength <- ifelse(average,1,(nrow(x)*ncol(x)))
        return(getDescStatsUDT(object=x,
                                functionName=vfunction,
                                outCol=c(vectorValueColumn=voutcol),
                                viewCols=c(pGroupID=1,
                                    pValue="CAST(cell_val_colname AS DOUBLE)"))*vlength)
        })

setMethod("deviation",signature(x="ANY"),
    function(x,
            method="mean-abs",
            average=TRUE){
        if(is.data.frame(x))
        x <- as.vector(as.matrix(d))
        if(any(suppressWarnings(is.na(as.numeric(x)))))
        stop("x must be numeric \n ")
        x <- as.vector(x)

        if(average) vlength <- length(x)
        else vlength <- 1

        if(method=="mean-abs")
        return(sum(abs(x-mean(x)))/vlength)
        else if(method=="median-abs")
        return(sum(abs(x-median(x)))/vlength)
        else if(method=="mean-square")
        return(sum((x-mean(x))^2)/vlength)
        })

############################# FLDevSq #################################################
#' @export
setGeneric("FLDevSq",function(x,na.rm=FALSE)
                standardGeneric("FLDevSq"))
FLDevSq.FLAbstractColumn <- function(x,na.rm=FALSE){
    return(paste0(" FLDevSq(",
                paste0(x@columnName,collapse=","),") "))
}
setMethod("FLDevSq",signature(x="FLVector"),
    function(x,na.rm=FALSE){
        return(genAggregateFunCall(object=x,
                                func=FLDevSq.FLAbstractColumn))})
setMethod("FLDevSq",signature(x="FLAbstractColumn"),
    function(x,na.rm=FALSE){
        return(FLDevSq.FLAbstractColumn(object=x,na.rm=na.rm))})
setMethod("FLDevSq",signature(x="FLMatrix"),
    function(x,na.rm=FALSE){
        return(genAggregateFunCall(object=x,
                                func=FLDevSq.FLAbstractColumn))})
setMethod("FLDevSq",signature(x="FLTable"),
    function(x,na.rm=FALSE){
        return(genAggregateFunCall(object=x,
                                func=FLDevSq.FLAbstractColumn))})

###############################################################################################
getDescStatsUDTjoin <- function(object,
                            functionName,
                            outCol,
                            viewCols){
    
    if(is.FLTable(object) && !isDeep(object))
    object <- wideToDeep(object)

    # sqlstr <- paste0("WITH z (",paste0(names(viewCols),collapse=","),") AS ( \n ",
    #                 " SELECT ",paste0(viewCols,collapse=",")," \n ",
    #                 " FROM(",constructSelect(object),") a) \n ",
    #                 " SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
    #                     "z.",names(viewCols)[4]," AS vectorIndexColumn, \n ",
    #                     paste0("a.",outCol," AS ",names(outCol),collapse=",")," \n ",
    #                 " FROM \n ",
    #                 " TABLE (",functionName,"(",paste0("z.",names(viewCols)[1:3],collapse=","),") \n ",
    #                 " HASH BY ",paste0("z.",names(viewCols)[1])," \n ",
    #                 " LOCAL ORDER BY ",paste0("z.",names(viewCols)[1]),") AS a,z \n ",
    #                 " WHERE z.",names(viewCols)[2]," = a.oValue")

    vMap <- getMatrixUDTMapping(functionName)
    pOutColnames <- vMap$argsPlatform
    pOutColnames["vectorIdColumn"] <- "'%insertIDhere%'"
    outputValueColumn <- pOutColnames["outputValueColumn"]
    pOutColnames <- pOutColnames[setdiff(names(pOutColnames),"outputValueColumn")]
    pOutColnames <- as.list(pOutColnames)
    pFuncName <- vMap$funcNamePlatform
    
    if(is.TD()){
        pOutColnames[["vectorIndexColumn"]] <- "z.pObsID"
        sqlstr <- constructUDTSQL(pViewColnames=as.list(viewCols),
                              pFuncName=pFuncName,
                              pSelect=constructSelect(object,order=FALSE),
                              pOutColnames=pOutColnames,
                              pNest=TRUE,
                              UDTInputSubset=1:3
                              )
        sqlstr <- paste0(sqlstr,",z WHERE z.pValue = a.",outputValueColumn)
    }
    else if(is.Hadoop()){
        pOutColnames[["outputValueColumn"]] <- outputValueColumn
        sqlstr <- constructUDTSQL(pViewColnames=as.list(viewCols),
                              pFuncName=pFuncName,
                              pSelect=constructSelect(object,order=FALSE),
                              pOutColnames=pOutColnames,
                              pNest=TRUE,
                              UDTInputSubset=1:3
                              )
        sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                                "b.pObsID AS vectorIndexColumn, \n ",
                                "a.vectorValueColumn AS vectorValueColumn \n ",
                        " FROM (",sqlstr,") a, \n ",
                                "(",paste0("SELECT ",constructVariables(viewCols),
                                            " FROM (",constructSelect(object)," ) a "),
                                ") b \n ",
                        " WHERE a.outputValueColumn=b.pValue ")
    }
    
    tblfunqueryobj <- new("FLTableFunctionQuery",
                    connectionName = getFLConnectionName(),
                    variables = list(
                    obs_id_colname = "vectorIndexColumn",
                    cell_val_colname = "vectorValueColumn"),
                    whereconditions="",
                    order = "",
                    SQLquery=sqlstr)
    vlength <- ifelse(is.FLVector(object),
                    length(object),
                    nrow(object)*ncol(object))

    if(is.FLVector(object) && !is.null(names(object)))
    vnames <- names(object)
    else vnames <- 1:vlength

    return(store(newFLVector(
            select=tblfunqueryobj,
            Dimnames=list(vnames,"vectorValueColumn"),
            isDeep=FALSE)))
}

######################### percRank, Rank, fracRank ###########################################
## only ties.method should be average,duplicate,perc.
#' @export
setGeneric("rank",function(x,na.last=TRUE,
                            ties.method="average",
                            ...)
                standardGeneric("rank"))

selectRankMethod <- function(rankOrder,type){
    if(!rankOrder %in% c("A","D"))
        stop("rankOrder must be A or D \n ")
        vtemp <- c(FLRankUdt="duplicate",
                    FLFracRankUdt="average",
                    FLPercRankUdt="perc")
        vfunction <- names(vtemp)[vtemp==type]
        if(length(vfunction)==0)
        stop("type should be c(average,duplicate,perc) for FL objects \n ")

        vtemp <- c(oRank="duplicate",
                    oFracRank="average",
                    opercRank="perc")
        voutcol <- names(vtemp)[vtemp==type]

        return(c(vfunction=vfunction,
            voutcol=voutcol))
}

setMethod("rank",signature(x="FLVector"),
    function(x,na.last=TRUE,
            ties.method="average",
            rankOrder="A",
            ...){
        vtemp <- selectRankMethod(rankOrder=rankOrder,
                        type=ties.method)
        vfunction <- vtemp["vfunction"]
        voutcol <- vtemp["voutcol"]
        names(vfunction) <- NULL
        names(voutcol) <- NULL

        return(getDescStatsUDTjoin(object=x,
                                functionName=vfunction,
                                outCol=c(vectorValueColumn=voutcol),
                                viewCols=c(pGroupID=1,
                                        pValue=getValueSQLName(x),
                                        pRankOrder=fquote(rankOrder),
                                        pObsID="vectorIndexColumn")
                                )
            )
        })

setMethod("rank",signature(x="FLVector.Hadoop"),
    function(x,na.last=TRUE,
            ties.method="average",
            rankOrder="A",
            ...){
        vtemp <- selectRankMethod(rankOrder=rankOrder,
                        type=ties.method)
        vfunction <- vtemp["vfunction"]
        voutcol <- vtemp["voutcol"]
        names(vfunction) <- NULL
        names(voutcol) <- NULL

        return(getDescStatsUDTjoin(object=x,
                                functionName=vfunction,
                                outCol=c(vectorValueColumn=voutcol),
                                viewCols=c(pGroupID=1,
                                        pValue=paste0("CAST(",getValueSQLName(x)," AS DOUBLE)"),
                                        pRankOrder=fquote(rankOrder),
                                        pObsID="vectorIndexColumn")
                                )
            )
        })

setMethod("rank",signature(x="ANY"),
    function(x,na.last=TRUE,
            ties.method="average",
            rankOrder="A",
            ...){
        return(base::rank(x=x,
                    na.last=na.last,
                    ties.method=ties.method,
                    ...))
        })

############################### FLNtile ###############################################
#' @export
setGeneric("FLNtile",function(x,n,...)
                standardGeneric("FLNtile"))

setMethod("FLNtile",signature(x="FLVector"),
    function(x,n,...){

        return(getDescStatsUDTjoin(object=x,
                functionName="FLNtileUDT",
                outCol=c(
                        vectorValueColumn="oNTile"),
                viewCols=c(pGroupID=1,
                    pValue="vectorValueColumn",
                    pRequiredNtile=n,
                    pObsID="vectorIndexColumn")
                ))
        })
