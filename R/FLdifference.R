## test case in test_FLdifference.
#' \code{FLdiff} performs difference operator on FLVector objects.
#'
#' The DB Lytix function called is FLDifferenceUdt. Performs Difference operator and 
#' stores the results in data frame.
#'
#' @param data An object of class FLVector.
#' @param degree The degree of the difference operator.
#' @return \code{FLdiff} returns a data.frame
#' @examples
#' rv <- sqlQuery(connection, "SELECT NUM_VAL as num_val FROM tblTimeSeriesW1")
#' flv <- as.FL(rv$num_val)
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
    functionName <- "FLDifferenceUdt"
    vOutCols <- getMatrixUDTMapping(functionName)$argsPlatform
    vperiodid <- vOutCols["vectorIndexColumn"]
    vdifference <- vOutCols["vectorValueColumn"]

    str <- constructUDTSQL(pViewColname = c(groupid = 1,
                                            val = "vectorValuecolumn",
                                            idx = "vectorIndexColumn",
                                            pd=degree),
                           pFuncName = functionName,
                           pOutColnames = vOutCols,
                           pSelect = constructSelect(data),
                           pLocalOrderBy=c("groupid","idx"),
                           pNest = TRUE,
                           pFromTableFlag = FALSE,
                           UDTInputSubset = c("groupid","val","pd")
                           )

    # tblfunqueryobj <- new("FLTableFunctionQuery",
    #                       connectionName = getFLConnectionName(),
    #                       variables = list(
    #                           obs_id_colname = vperiodid,
    #                           cell_val_colname = vdifference),
    #                       whereconditions="",
    #                       order = "",
    #                       SQLquery=str)
    # val <- new("FLSimpleVector",
    #            select = tblfunqueryobj,
    #            dimColumns = c(vperiodid,vdifference),
    #            Dimnames = list(row = 1:length(data) ),
    #            dims = as.integer(nrow(data)),
    #            type = "integer"
    #            )
    tblfunqueryobj <- new("FLTableFunctionQuery",
                          connectionName = attr(connection,"name"),
                          variables = list(
                              obs_id_colname = "vectorIndexColumn",
                              cell_val_colname = "vectorValueColumn"),
                          whereconditions="",
                          order = "",
                          SQLquery=str)
    val <- newFLVector(
               select = tblfunqueryobj,
               Dimnames = list(1:length(data),
                            "vectorValueColumn"),
               isDeep = FALSE,
               type="integer")
    return(val)
}

## test case in test_FLdifference.
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
#'rv <- sqlQuery(connection, "SELECT stockreturn as stockreturn FROM finstockreturns WHERE TICKERID =3")
#'flv <- as.FL(rv$stockreturn)
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
    vUDTInputSubset <- c("groupid","valtype","val")
    str <- constructUDTSQL(pViewColname = c(groupid = 1,
                                            valtype = fquote(ValueType),
                                            val = "vectorValueColumn",
                                            idx = "vectorIndexColumn"),
                           pFuncName = functionName,
                           pOutColnames = c("a.*"),
                           pSelect = constructSelect(data),
                           pLocalOrderBy=c("groupid","idx"),
                           pNest = TRUE,
                           pFromTableFlag = FALSE,
                           UDTInputSubset=vUDTInputSubset
                        )
    vdf <- sqlQuery(connection, str)
    vdf <- vdf[,-c(1,9)]
    # names(vdf) <- gsub( "^.", "", x = names(vdf))
    names(vdf) <- c("numobs","lambda","variance",
                    "logL","SBC","AIC","ConvergenceCriteria"
                    )
    return(as.list(vdf))
}
