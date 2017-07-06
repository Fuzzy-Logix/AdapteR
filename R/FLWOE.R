#' @export
setClass("FLWOE",
         slots = list(table = "FLTable",
                      results = "list") )
#' @export
setClass("FLInfoVal",
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
#' flmod <- InfoVal.FLTable(event = "Events", nonevents = "NonEvents", data = fltbl,n = 4)
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
    vstr <- vQuery(vtbl, events, nonevents,n, vfun)

    vclass <- "FLWOE"
    vdf <- sqlQuery(connection, vstr)
    ##print(vdf)
    return(new(vclass,
               table = data,
               results = list(otable = vdf))) }


#' \code{InfoVal} Performs  on FLTable objects.It is an aggregate function which
#' which measures the information value of a data set. Information value is a
#' concept used for variable selection during model building. This analysis can be used
#' assess the overall predictive power of the variables being considered.
#' @param data An object of class FLTable.
#' @param events  Events or element of good distribution.
#' @param nonevents Non-Events or element of bad distribution.
#' @param n number of ID's for which information is to be calculated.
#' fltbl <- FLTable(table = "tblinfoval", obs_id_colname="BinID")
#' flmod <- InfoVal(event = "Events", nonevents = "NonEvents", data = fltbl,n = 4)
#' @export
InfoVal <- function (events,nonevents,n = 2, data=list(),...) {
    UseMethod("InfoVal", data) }

#' @export
InfoVal.FLTable <- function(events,nonevents,n = 2, data, ...)
{
    ##    browser()
    vcallObject <- match.call()
    vtbl <- data@select@table_name
    vfun <- "FLInfoVal"
    vstr <- vQuery(vtbl, events, nonevents,n, vfun)
    vclass <- "FLInfoVal"
    vdf <- sqlQuery(connection, vstr)
    ##print(vdf)
    return(new(vclass,
               table = data,
               results = list(otable = vdf))) }



vQuery <- function(table_name,events,nonevents,n,fName = "FLWOE",...){
    if(fName == "FLWOE"){
        var <- "b.SerialVal"
        varname <- "WOE"
    }
    else{
        var <- "b.SerialVal - 1"
        varname <- "InfoVal"
    }
    
    vquery <- paste0("SELECT ",var," AS BinID, ",fName,"(a.BinID, a.",events,", a.",nonevents,", ",var,") AS ",varname," FROM ",table_name," a, fzzlSerial b WHERE b.SerialVal <= ",n," GROUP BY b.SerialVal ORDER BY 1;")
    return(vquery)
}





#' @export
print.FLWOE <- function(object){
    print(object@results$otable)
}

#' @export
print.FLInfoVal <- function(object){
    print(object@results$otable)
}

#' @export
setMethod("show", signature("FLWOE"), function(object) {print.FLWOE(object)})

#' @export
setMethod("show", signature("FLInfoVal"), function(object) { print.FLInfoVal(object)})










