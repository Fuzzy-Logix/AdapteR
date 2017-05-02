#' @export
FLdiff <- function (data=list(),degree, ...) {
    UseMethod("FLdiff", data)
}


##
##Example:
#'rv <- sqlQuery(connection, "SELECT NUM_VAL FROM tblTimeSeriesW1")
#'flv <- as.FL(rv$NUM_VAL)
#' FLdiff.FLVector(flv)
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
                         pLocalOrderBy=c("GroupID", "val"), pNest = TRUE, pFromTableFlag = FALSE)
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






#' @export
FLEWMA <- function (data=list(),degree, ...) {
    UseMethod("FLEWMA", data)
}

##
##Example:
#'rv <- sqlQuery(connection, "SELECT stockreturn FROM finstockreturns WHERE TICKERID =3")
#'flv <- as.FL(rv$STOCKRETURN)
#' FLEWMA(flv)
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
                                            Valtype = fquote(ValueType),
                                            Val = "vectorValuecolumn"),
                           pFuncName = functionName,
                           pOutColnames = c("a.*"),
                           pSelect = constructSelect(data),
                           pLocalOrderBy=c("GroupID"),
                           pNest = TRUE,
                           pFromTableFlag = FALSE)
    vdf <- sqlQuery(connection, str)
    vdf <- vdf[,-c(1,9)]
    names(vdf) <- gsub( "^.", "", x = names(vdf))
    return(vdf)
}
