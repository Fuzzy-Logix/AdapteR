## test case in test_FLdifference.
#' @export
#' \code{FLdiff} performs difference operator on FLVector objects.
#'
#' The DB Lytix function called is FLDifferenceUdt. Performs Difference operator and 
#' stores the results in data frame.
#'
#' @param data An object of class FLVector.
#' @param degree The degree of the difference operator.
#' @return \code{FLdiff} returns a data.frame
#' @examples
#'rv <- sqlQuery(connection, "SELECT NUM_VAL FROM tblTimeSeriesW1")
#'flv <- as.FL(rv$NUM_VAL)
#' FLdiff(flv)

#' @export
FLdiff <- function (data=list(),degree, ...) {
    UseMethod("FLdiff", data)
}

#' @export
FLdiff.FLVector <- function(data,degree = 2, ...)
{
    if(!is.FLVector(data)){
        stop("only applicable on FLVector")
    }
    ##browser()
    functionName <- "FLDifferenceUdt"
    pArg <- c(pD = degree)
    str <- constructUDTSQL(pViewColname = c(GroupID = 1,
                                          Val = "vectorValuecolumn"),
                         pFuncName = functionName,
                         pOutColnames = c("a.*"),
                         pSelect = constructSelect(data),
                         pArgs = pArg,
                         pLocalOrderBy=c("GroupID", "val"),
                         pNest = TRUE,
                         pFromTableFlag = FALSE,
                         UDTInputSubset = c("GroupID", "Val")
                         )
    tblfunqueryobj <- new("FLTableFunctionQuery",
                          connectionName = getFLConnectionName(),
                          variables = list(
                              obs_id_colname = "oPeriodID",
                              cell_val_colname = "oDifference"),
                          whereconditions="",
                          order = "",
                          SQLquery=str)
        val <- new("FLSimpleVector",
               select = tblfunqueryobj,
               dimColumns = c("oPeriodID", "oDifference"),
               Dimnames = list(row = 1:length(data) ),
               dims = as.integer(nrow(data)),
               type = "integer"
               )
    return(val)
}

## test case in test_FLdifference.
#' @export
#' \code{FLEWMA} performs Exponentially Weighted Moving Average on FLVector objects.
#'
#' The DB Lytix function called is FLEWMAUdt. Performs volatility based on most recent volatility and the most recent return (change) in a times series.
#' stores the results in data frame.
#'
#' @param data An object of class FLVector.
#' @param ValueType Is the input actual time series values or changes
#' (deltas/Returns)?The degree of the difference operator.
#' @return \code{FLEWMA} returns a data.frame
#' @examples
#'rv <- sqlQuery(connection, "SELECT stockreturn FROM finstockreturns WHERE TICKERID =3")
#'flv <- as.FL(rv$STOCKRETURN)
#' FLEWMA(flv)

#' @export
FLEWMA <- function (data=list(),ValueType = "R", ...) {
    UseMethod("FLEWMA", data)
}

#' @export
FLEWMA.FLVector <- function(data,ValueType = "R", ...)
{
    if(!is.FLVector(data)){
        stop("only applicable on FLVector")
    }
    ##browser()
    functionName <- "FLEWMAUdt"
    ##pArg <- c(pD = degree)
    str <- constructUDTSQL(pViewColname = c(GroupID = 1,
                                            ValType = fquote(ValueType),
                                            Val = "vectorValuecolumn"),
                           pFuncName = functionName,
                           pOutColnames = c("a.*"),
                           pSelect = constructSelect(data),
                           pLocalOrderBy=c("GroupID"),
                           pNest = TRUE,
                           pFromTableFlag = FALSE,
                           UDTInputSubset = c("GroupID","ValType","Val") )
    vdf <- sqlQuery(connection, str)
    vdf <- vdf[,-c(1,9)]
    names(vdf) <- gsub( "^.", "", x = names(vdf))
    return(vdf)
}
