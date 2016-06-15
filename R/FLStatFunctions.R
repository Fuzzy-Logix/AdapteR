#' @include utilities.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLTable.R
#' @include FLIs.R
#' @include FLDims.R
#' @include FLPrint.R
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
    if(diag && !upper)
    vwhereConditions <- c(vwhereConditions,
        paste0("a.",vobsidCol," >= b.",vobsidCol))
    else if(upper && !diag)
    vwhereConditions <- c(vwhereConditions,
        paste0("a.",vobsidCol," <> b.",vobsidCol))
    else if(!diag && !upper)
    vwhereConditions <- c(vwhereConditions,
        paste0("a.",vobsidCol," > b.",vobsidCol))

    sqlstr <- paste0("SELECT '%insertIDhere%' AS matrixIdColumn,\n",
                            "a.",vobsidCol," AS rowIdColumn,\n",
                            "b.",vobsidCol," AS colIdColumn,\n",
                            functionName,"(a.",vvalueCol,",b.",vvalueCol,") AS valueColumn\n",
                        "FROM (",constructSelect(x),") a,(",
                                constructSelect(x),") b \n",
                        constructWhere(vwhereConditions),"\n",
                        " GROUP BY 1,2,3")

    tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = getOption("connectionFL"),
                        variables=list(
                            rowIdColumn="rowIdColumn",
                            colIdColumn="colIdColumn",
                            valueColumn="valueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)
        flm <- new("FLMatrix",
                           select= tblfunqueryobj,
                           dim=c(x@dim[1],x@dim[1]),
                           dimnames=list(rownames(x),rownames(x)))

        return(ensureQuerySize(pResult=flm,
                        pInput=list(x,method,
                                    diag,upper,
                                    p,vobsidCol,
                                    vvaridCol,
                                    vvalueCol),
                        pOperator="FLStatsDist"))
    }


#' computes distance between
#'
#' @Section Constraints
#' only manhattan and euclidean are supported currently.
#' @examples
#' flmatrix <- FLMatrix("FL_DEMO",
#' "tblMatrixMulti", 1,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' dist(flmatrix)
#' dist(flmatrix,diag=TRUE)
#' dist(flmatrix,upper=TRUE)
#' dist(flmatrix,diag=TRUE,upper=TRUE)
#' dist(flmatrix,"manhattan",TRUE,TRUE)
setMethod("dist",signature(x="FLMatrix"),
    function(x,method="euclidean",
            diag=FALSE,
            upper=FALSE,
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

setMethod("dist",signature(x="FLVector"),
    function(x,method="euclidean",
            diag=FALSE,
            upper=FALSE,
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

setMethod("dist",signature(x="FLTable"),
    function(x,method="euclidean",
            diag=FALSE,
            upper=FALSE,
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

sd.FLAbstractColumn <- function(x,na.rm=FALSE){
    return(paste0(" FLStdDev(",
                paste0(x@columnName,collapse=","),") "))
}
setMethod("sd",signature(x="FLVector"),
    function(x,na.rm=FALSE){
        return(genScalarFunCall(object=x,
                                func=sd.FLAbstractColumn))})
setMethod("sd",signature(x="FLAbstractColumn"),
    function(x,na.rm=FALSE){
        return(sd.FLAbstractColumn(object=x,na.rm=na.rm))})
setMethod("sd",signature(x="FLMatrix"),
    function(x,na.rm=FALSE){
        return(genScalarFunCall(object=x,
                                func=sd.FLAbstractColumn))})
setMethod("sd",signature(x="FLTable"),
    function(x,na.rm=FALSE){
        return(genScalarFunCall(object=x,
                                func=sd.FLAbstractColumn))})


setGeneric("FLSdP",function(x,na.rm=FALSE)
                standardGeneric("FLSdP"))
FLSdP.FLAbstractColumn <- function(x,na.rm=FALSE){
    return(paste0(" FLStdDevP(",
                paste0(x@columnName,collapse=","),") "))
}
setMethod("FLSdP",signature(x="FLVector"),
    function(x,na.rm=FALSE){
        return(genScalarFunCall(object=x,
                                func=FLSdP.FLAbstractColumn))})
setMethod("FLSdP",signature(x="FLAbstractColumn"),
    function(x,na.rm=FALSE){
        return(FLSdP.FLAbstractColumn(object=x,na.rm=na.rm))})
setMethod("FLSdP",signature(x="FLMatrix"),
    function(x,na.rm=FALSE){
        return(genScalarFunCall(object=x,
                                func=FLSdP.FLAbstractColumn))})
setMethod("FLSdP",signature(x="FLTable"),
    function(x,na.rm=FALSE){
        if(!x@isDeep)
        stop("convert to deep format using wideToDeep \n")
        return(genScalarFunCall(object=x,
                                func=FLSdP.FLAbstractColumn))})


FLWtGeneric <- function(x,w,functionName){
    if(length(w)!=length(x))
        stop("length of x and w should be same")
    else if(is.vector(w))
        w <- as.FLVector(w)

    sqlstr <- paste0("SELECT ",functionName,"(b.vectorValueColumn,a.vectorValueColumn) \n",
                        " FROM (",constructSelect(x)," ) a,\n",
                            "(",constructSelect(w),") b \n",
                        " WHERE a.vectorIndexColumn = b.vectorIndexColumn")
    return(sqlQuery(getOption("connectionFL"),sqlstr)[[1]])
}
weighted.mean.FLVector <- function(x,w=rep(1/length(x),length(x)),...){
    if(missing(w))
        return(mean(x))
    else return(FLWtGeneric(x=x,w=w,functionName="FLWtAvg"))
}


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
        SDMTools::wt.sd)

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
        SDMTools::wt.var)

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
        SDMTools::wt.mean)
