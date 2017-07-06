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
#' flmod <- woe.FLTable(event = "vEvents", nonevents = "vNonEvents", data = fltbl,n = 4)
#' TO-DO: find examples and R implementation.
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




vdf <- data.frame(BinID = 1:4, vEvents =c(206,357,776,183) , vNonEvents = c(4615,9909,32150,12605))
fltbl <- as.FL(vdf)






"SELECT b.SerialVal AS BinID, FLWOE(a.BinID, a.vEvents, a.vNonEvents, b.SerialVal) AS WOE FROM ARBaseaW1499339697 a, fzzlSerial b WHERE b.SerialVal <= 4 GROUP BY b.SerialVal ORDER BY 1;"




sqlQuery(connection, "SELECT b.SerialVal AS BinID,
FLWOE(a.BinID, a.vEvents, a.vNonEvents, b.SerialVal) AS WOE
FROM ARBaseaW1499339697 a,
fzzlSerial b
WHERE b.SerialVal <= 4
GROUP BY b.SerialVal
ORDER BY 1;")


