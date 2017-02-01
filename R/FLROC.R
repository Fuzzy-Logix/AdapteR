## http://people.inf.elte.hu/kiss/11dwhdm/roc.pdf
#' tbl <- FLTable("tblROCCurve", "ObsID")
#' mod <- roc(tbl$ActualVal, tbl$ProbVal)
#'Example 2:
#' data(aSAH)
#'fltbl <- data.frame(res = aSAH$outcome, pred = aSAH$s100b)
#'fltbl$res <- as.numeric(fltbl$res)
#'fltbl$res <- fltbl$res - 1
#' fltbl <- fltbl[-55, ]
#' head(fltbl)
#'fltbl <- as.FLTable(fltbl)
#'flmod <- roc(fltbl$res, fltbl$pred)
#' 
#' @export
##roc.default <- function (response, predictor,...) {
##    if (!requireNamespace("pROC", quietly = TRUE)){
##        stop("pROC package needed for roc. Please install it.",
##             call. = FALSE)
##    }
##    else return(pROC::roc(response, predictor,...))
##}
##
## to-do : work on formula aspect of function, print function, $ operator[(levels)].


#' @export
setClass(
    "FLROC",
    contains="FLRegr",
    slots=list(otbl="character"))


#' @export
roc.FLVector <- function (response, predictor, ...)
{
    vcallObject <- match.call()
    if(!is.FLVector(predictor))
    {predictor <- as.FL(predictor)}
    return(rocgeneric(response = response,
                      predictor = predictor,
                      callobject = vcallObject,
                      ...))}

#' @export
roc.FLTable <- roc.FLVector


#' @export
roc.FLTableMD <- roc.FLVector


## setMethod("show","auc",print.FLROC)

rocgeneric <- function(response, predictor,callobject,  ...)
{
    ##browser()
    vvolName <- gen_view_name("roccurve")
    vselect <- paste0(" SELECT a.vectorIndexColumn AS OBSID, a.vectorValueColumn as res, b.vectorValueColumn AS pred
                          FROM (",constructSelect(response),") AS a ,
                               (",constructSelect(predictor),") AS b
                          WHERE  a.vectorIndexColumn = b.vectorIndexColumn")
    tbl <- createTable(pTableName = vvolName,
                       pWithData = TRUE,
                       pTemporary = TRUE,
                       pSelect = vselect )
    vrw <- nrow(predictor)
    rnames <- rownames(predictor)
    cnames <- c("ObsID", colnames(response), colnames(predictor))
    ret <- sqlStoredProc(connection,
                         "FLROCCurve",
                         InputTable = vvolName,
                         RecID = "ObsID",
                         ActualColName = "res",
                         ProbColName = "pred",
                         WhereClause =NULL ,
                         TableOutput = 1,
                         outputParameter = c(OutTable = 'a') 
                         )
    vclass <- "FLROC"
    quer <- paste0("SELECT COUNT(res) AS val FROM ",vvolName," GROUP BY res")
    df <- sqlQuery(connection, quer)
    
    return(new(vclass,
               otbl = as.character(ret[[1]]),
               results = list(call = callobject,
                              itable = vvolName,
                              Dimnames = list(row = rnames, col = cnames),
                              dims = c(vrw, 3),
                              vals = c(controls = df$val[2], cases =df$val[1] )
                              )
               ))
    
}


#' @export
`$.FLROC`<-function(object,property){
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]
    
    flvgeneric <- function(object, tblname,var,whereconditions,vorder, dcolumn,
                           dnames = object@results$Dimnames$row,
                           vdims = object@results$dims[[1]])
    {
        val <- new("FLSimpleVector",
                   select= new("FLSelectFrom",
                               table_name=tblname,
                               connectionName=getFLConnectionName(),
                               variables=var,
                               whereconditions=whereconditions,
                               order=vorder),
                   dimColumns = dcolumn,
                   ##names=NULL,
                   Dimnames = list(dnames),
                   dims    = as.integer(vdims),
                   type       = "integer"
                   )
        return(val) }

    if(property == "sensitivities"){
        return(flvgeneric(object,
                          tblname = object@otbl,
                          var = list(OBSID = "OBSID", sen = "TPR"),
                          whereconditions = "" ,
                          vorder = "TPR",
                          dcolumn = c("OBSID", "sen")))}
    
    else if(property == "specificities"){
        return(flvgeneric(object,
                          tblname = object@otbl,
                          var = list(OBSID = "OBSID", spec = "(1 - FPR)"),
                          whereconditions = "" ,
                          vorder = "FPR DESC",
                          dcolumn = c("OBSID","spec")))}
    else if(property == "auc") {
        return(auc(object)) }
    
    else if(property == "original.predictor"){
        return(flvgeneric(object,
                          tblname = object@results$itable,
                          var = list("pred"),
                          whereconditions = "" ,
                          vorder = "OBSID",
                          dcolumn = c("OBSID", "pred")))
    }

    else if(property == "original.response"){
        return(flvgeneric(object,
                          tblname = object@results$itable,
                          var = list("res"),
                          whereconditions = "" ,
                          vorder = "OBSID",
                          dcolumn = c("OBSID", "res"))) }

    else if(property == "levels"){print("need to do:")}

    else if(property == "controls"){
        return(flvgeneric(object,
                          tblname = object@results$itable,
                          var = list("pred"),
                          whereconditions = "res = 0",
                          vorder = "OBSID",
                          dcolumn = c("OBSID", "pred"),
                          dnames = (1:object@results$vals[1]),
                          vdims  = object@results$vals[1]
                          ))
    }
    
    else if(property == "cases"){
        return(flvgeneric(object,
                          tblname = object@results$itable,
                          var = list("pred"),
                          whereconditions = "res = 1",
                          vorder = "OBSID",
                          dcolumn = c("OBSID", "pred"),
                          dnames = (1:object@results$vals[2]),
                          vdims  = object@results$vals[2]))
    }
    
    else if(property == "call"){
        return(object@results$call)}
    
    else if(property == "percent"){
        return(FALSE)}
}

#' @export
auc <- function(object,...) UseMethod("auc")

#' @export
auc.default <- function(object,...) {
    if (!requireNamespace("pROC", quietly = TRUE)){
        stop("pROC package needed for auc. Please install it.",
             call. = FALSE)
    }
    else return(pROC::auc(object,...))
}

#' @export
roc <- function(object,limit = 1000,...) UseMethod("roc")

#' @export
auc.FLROC <- function(object,limit = 1000,...)
    return(as.roc(object, limit,auc=TRUE)$auc)

#' @export
plot.FLROC <- function(object,limit = 1000,  ...)
    return(plot(as.roc(object, limit=limit), ...))

#' @export
print.FLROC <- function(object, ...)
    return(print(as.roc(object, auc=TRUE, ...)))


as.roc <- function(object,limit = 1000, auc=TRUE, ... ){
    p <- min(limit,object@results$dims[[1]])/(object@results$dims[[1]])
    vfrom1 <- gsub("ORDER BY FPR DESC", "", constructSelect(object$specificities))
    vfrom2 <- gsub("ORDER BY TPR","", constructSelect(object$sensitivities) )

    str1 <- paste0("SELECT  b.spec AS spec, a.sen AS sen FROM (",vfrom1,") AS b, (",vfrom2,") AS a WHERE a.ObSID = b.ObsID AND FLSimUniform(RANDOM(1,10000), 0.0, 1.0) < ",p," ")
    df <- sqlQuery(connection, str1)
    sen <- sort(as.numeric(df$sen), decreasing = TRUE)
    spec <- sort(as.numeric(df$spec))

    reqList <- structure(
        list(call = object$call,
             cases = object$cases,
             controls = object$controls,
             percent = object$percent,
             sensitivities =sen,
             specificities = spec
             ),
        class="roc")
    if(auc) reqList$auc <- auc(reqList)
    return(reqList)
}
