
#' @export
setClass("FLGarch",
         slots = list(results = "list"))

#' @export
garch <- function (data=list(),degree, ...) {
    UseMethod("garch", data)
}


## test case in test_FLdifference.
##Example:
#'rv <- sqlQuery(connection, "SELECT stockreturn FROM tblbac_return")
#'flv <- as.FL(rv$stockreturn)
#' garch(flv)
#' @export
garch.FLVector <- function(data,degree = 2, ...)
{
    if(!is.FLVector(data)){
        stop("only applicable on FLVector")
    }
    browser()
    functionName <- "FLIGarchUdt"
    ##pArg <- c(pD = degree)
    str <- constructUDTSQL(pViewColname = c(GroupID = 1,
                                            q = 1,
                                            p = 1,
                                            Val = "vectorValuecolumn"),
                         pFuncName = functionName,
                         pOutColnames = c("a.*"),
                         pSelect = constructSelect(data),
                         ##pArgs = pArg,
                         pLocalOrderBy=c("GroupID", "val"), pNest = TRUE, pFromTableFlag = FALSE)
    vdf <- sqlQuery(connection, str)
    return(vdf)
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


