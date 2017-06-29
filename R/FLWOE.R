#' @export
setClass("FLWOE",
         slots = list(table = "FLTable",
                      results = "list") )

#' \code{woe} Performs woe on FLTable objects.It is an aggregate function which
#' Calculates the statistical Weight of Evidence (WOE). WOE is a measure of how much
#' an attribute in the data is related to statuses. This analysis can be used to
#' assess the overall predictive power of the variables being considered.
#' @param data An object of class FLTable.
#' @param events  Events or element of good distribution.
#' @param nonevents Non-Events or element of bad distribution.
#' @param n number of ID's for which information is to be calculated.
#' fltbl <- FLTable(table = "tblinfoval", obs_id_colname="BinID")
#' flmod <- woe.FLTable(event = "Events", nonevents = "NonEvents", data = fltbl,n = 5)
#' @export
woe <- function (events,nonevents,n = 2, data=list(),...) {
    UseMethod("woe", data)
}

#' @export
woe.default <- function(...){
    woe::woe(...) }

#' @export
woe.FLTable <- function(events,nonevents,n = 2, data, ...)
{
    ##    browser()
    vcallObject <- match.call()
    vtbl <- data@select@table_name
    vfun <- "FLWOE"  
    vquery <- paste0("SELECT b.SerialVal AS BinID, FLWOE(a.BinID, a.",events,", a.",nonevents,", b.SerialVal) AS WOE FROM ",vtbl," a, fzzlSerial b WHERE b.SerialVal <= ",n," GROUP BY b.SerialVal ORDER BY 1;")
    vclass <- "FLWOE"
    vdf <- sqlQuery(connection, vquery)
    print(vdf)
    return(new(vclass,
               table = data,
               results = list(otable = vdf))) }

#' @export
print.FLWOE <- function(object){

    return(object@results$otable)
}

#' @export
setMethod("show", signature("FLWOE"), function(object) {print.FLWOE(object)})
