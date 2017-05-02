#' @export
setClass("FLGarch",
         slots = list(results = "list"))

#' @export
garch <- function (data=list(),order = c(1,1), ...) {
    UseMethod("garch", data)
}

#' @export
garch.default<-function (object,order = c(1,1),...) {
    if (!requireNamespace("tseries", quietly = TRUE)){
        stop("tseries package needed for Augmented Dickey Fuller test. Please install it.",
             call. = FALSE)
    }
    else return(tseries::garch(object,...))
}


## test case in test_FLdifference.
##Example:
#'rv <- sqlQuery(connection, "SELECT stockreturn FROM tblbac_return")
#'flv <- as.FL(rv$stockreturn)
#' flmod <- garch.FLVector(flv, order = c(1,1))
#' ARCHqUDT Example.
#' rv <- sqlQuery(connection, "SELECT stockprice  FROM tblbac")
#' flv <- as.FL(rv$stockprice)
#' flmod <- garch.FLVector(flv, order = c(0,1))
#' @export
garch.FLVector <- function(data,order = c(1,1),...)
{
    callObject <- match.call()
    if(!is.FLVector(data)){
        stop("only applicable on FLVector")
    }
    browser()
    if(is.null(list(...)[["ValueType"]]))
        Valtype = "R"
    else
        Valtype = list(...)[["ValueType"]]
    
    if(order[1] == 0){
        functionName <- "FLARCHqUDT"
        q <- order[2]
        vColname <- c(GroupID = 1,
                      q = q,
                      ValType = fquote(Valtype),
                      Val = "vectorValuecolumn")
        
        vsubset <- c("GroupID","q","ValType","Val")}

    if(order[1] >= 1){
        functionName <- "FLGARCHpqUdt"
        p <- order[1]
        q <- order[2]
        vColname <- c(GroupID = 1,
                      q = q,
                      p = p,
                      ValType = fquote(Valtype),
                      Val = "vectorValuecolumn")
        vsubset <- c("GroupID","q","p","ValType","Val")
    }
    
    ##pArg <- c(pD = degree)
    str <- constructUDTSQL(pViewColname = vColname,
                         pFuncName = functionName,
                         pOutColnames = c("a.*"),
                         pSelect = constructSelect(data),
                         ##pArgs = pArg,
                         pLocalOrderBy=c("GroupID", "val"),
                         pNest = TRUE,
                         pFromTableFlag = FALSE,
                         UDTInputSubset = vsubset)
    vdf <- sqlQuery(connection, str)
    vdf <- vdf[, -1]
        return(new("FLGarch",
                   results=list(call=callObject,
                                q = q,
                                vout = vdf )))   
}

`$.FLGarch`<-function(object,property){
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

    voutput <- object@results$vout
    if(property == "Alpha"){
        return(voutput$oParmValue[voutput["oParmName"] == "Alpha1"])}
    if(property == "Beta"){
        return(voutput$oParmValue[voutput["oParmName"] == "Beta1"])}
    if(property == "Gamma"){
        return(voutput$oParmValue[voutput["oParmName"] == "Gamma"])}
    if(property == "Omega"){
        return(voutput$oParmValue[voutput["oParmName"] == "Omega"])}
    if(property == "AIC"){
        return(voutput$oParmValue[voutput["oParmName"] == "AIC"])}
    if(property == "SBC"){
        return(voutput$oParmValue[voutput["oParmName"] == "SBC"])}
    if(property == "Variance"){
        return(voutput$oParmValue[voutput["oParmName"] == "Variance"])}
    if(property == "ConvCrit"){
        return(voutput$oParmValue[voutput["oParmName"] == "ConvCrit"])}
}
 






#' @export
regimeshift <- function (data=list(),regimes = 2, ...) {
    UseMethod("regimeshift", data)
}

## test case in test_FLdifference.
#' @export
#' \code{regimeshift} performs helps in identifying the parameters of the
#' Gaussian distribution and the probability that a given observation has been
#' drawn from a given distribution on FLVector objects.

#' The DB Lytix function called is FLRegimeShiftUdt. 
#' @param data An object of class FLVector.
#' @param regimes Total number of Regimes.
#' @return \code{regimeshift} returns a data.frame
#' @examples
#'vdf <-  sqlQuery(connection, "SELECT Num_Val FROM tblRegimeShift WHERE Groupid = 1 AND Obsid <500")
#'flv <- as.FL(vdf$Num_Val)
#' flmod <- regimeshift(flv, regimes = 3)
#' @export
regimeshift.FLVector <- function(data,regimes = 2, ...)
{
    if(!is.FLVector(data)){
        stop("only applicable on FLVector")
    }
##    browser()
    functionName <- "FLRegimeShiftUdt"
    pArg <- c(regimes = regimes)
    str <- constructUDTSQL(pViewColname = c(GroupID = 1,
                                            Val = "vectorValuecolumn"),
                         pFuncName = functionName,
                         pOutColnames = c("a.*"),
                         pSelect = constructSelect(data),
                         pArgs = pArg,
                         pLocalOrderBy=c("GroupID"),
                         pNest = TRUE,
                         pFromTableFlag = FALSE,
                         UDTInputSubset = c("GroupID", "Val"))
    vdf <- sqlQuery(connection, str)
    vdf <- vdf[,2:length(vdf)]
    names(vdf) <- c("Regime", "Mean", "StdDev", "Prob")
               
    return(vdf)
}


