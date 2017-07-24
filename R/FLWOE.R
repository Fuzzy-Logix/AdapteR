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
#' fltbl <- FLTable(table = getTestTableName("tblinfoval"), obs_id_colname="BinID")
#' flmod <- woe(event = "Events", nonevents = "NonEvents", data = fltbl,n = 4)
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
    vbinid <- getObsIdSQLName(data)
    vstr <- vQuery(data, vtbl, vbinid ,events, nonevents,n, vfun,...)

    vclass <- "FLWOE"
    vdf <- sqlQuery(connection, vstr)
    ##print(vdf)
    return(new(vclass,
               table = data,
               results = list(otable = vdf))) }

#' @export
woe.FLTable.TDAster <- function(events,nonevents,n = 2, data, ...){
    return(woe.FLTable(events=events,nonevents=nonevents,
                            n=n,data=data,UDT=TRUE,...))
}


#' \code{InfoVal} Performs  on FLTable objects.It is an aggregate function which
#' which measures the information value of a data set. Information value is a
#' concept used for variable selection during model building. This analysis can be used
#' assess the overall predictive power of the variables being considered.
#' @param data An object of class FLTable.
#' @param events  Events or element of good distribution.
#' @param nonevents Non-Events or element of bad distribution.
#' @param n number of ID's for which information is to be calculated.
#' fltbl <- FLTable(table = getTestTableName("tblinfoval"), obs_id_colname="BinID")
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
    vbinid <- getObsIdSQLName(data)
    vfun <- "FLInfoVal"
    vstr <- vQuery(data, vtbl, vbinid, events, nonevents,n, vfun,...)
    vclass <- "FLInfoVal"
    vdf <- sqlQuery(connection, vstr)
    ##print(vdf)
    return(new(vclass,
               table = data,
               results = list(otable = vdf)))
}

#' @export
InfoVal.FLTable.TDAster <- function(events,nonevents,n = 2, data, ...){
    return(InfoVal.FLTable(events=events,nonevents=nonevents,
                            n=n,data=data,UDT=TRUE,...))
}



vQuery <- function(data, table_name,binid, events,nonevents,n,
                    fName = "FLWOE",UDT=FALSE,...){
    if(fName == "FLWOE"){
        var <- "b.SerialVal"
        varname <- "woe"
    }
    else{
        var <- "b.SerialVal - 1"
        varname <- "infoval"
    }
    
    if(!UDT)
        vquery <- paste0("SELECT ",var," AS BinID, ",fName,"(a.",binid,", a.",events,", a.",nonevents,", ",var,") AS ",
                        varname," FROM (",constructSelect(data),") a, fzzlSerial b WHERE b.SerialVal <= ",n," GROUP BY b.SerialVal ORDER BY 1;")
    else {
        if(fName == "FLWOE")
        vquery <- paste0("SELECT a.Partition1 AS BinID, a.",varname," AS ",varname,
                         " \n FROM ",fName,"Udt(ON(SELECT a.",binid," AS binid,a.",events," AS events ,a.",nonevents," AS nonevents,",var," AS serialval ",
                                                    " \n FROM (",constructSelect(data),") a, fzzlSerial b \n ",
                                                " WHERE b.SerialVal <= ",n,") PARTITION BY serialval \n ",
                                    " TARGET('binid', 'events', 'nonevents', 'serialval')) AS a \n ",
                        " ORDER BY 1;")
        else if(fName == "FLInfoVal")
        vquery <- paste0("SELECT a.Partition1 AS BinID, a.",varname," AS ",varname,
                         " \n FROM ",fName,"Udt(ON(SELECT ",var," AS outputbinid ,a.",binid," AS binid,a.",events," AS events ,a.",nonevents," AS nonevents,",var," AS reqbinid ",
                                                    " \n FROM (",constructSelect(data),") a, fzzlSerial b \n ",
                                                " WHERE b.SerialVal <= ",n,") PARTITION BY outputbinid \n ",
                                    " TARGET('binid', 'events', 'nonevents', 'reqbinid')) AS a \n ",
                        " ORDER BY 1;")
    }
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










