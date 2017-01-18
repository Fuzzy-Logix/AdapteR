
## http://people.inf.elte.hu/kiss/11dwhdm/roc.pdf
#' tbl <- FLTable("tblROCCurve", "ObsID")
#' mod <- roc(tbl$ActualVal, tbl$ProbVal)
#'Example 2:
#' data(aSAH)
#'fltbl <- data.frame(res = aSAH$outcome, pred = aSAH$s100b)
#'fltbl$res <- as.numeric(fltbl$res)
#' fltbl$res <- fltbl$res - 1
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
## to-do : work on formula aspect of function, print function, $ operator[(levels),convert numeric to FLVector] .


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
                      ...))
}

#' @export
roc.FLTable <- roc.FLVector


#' @export
roc.FLTableMD <- roc.FLVector

rocgeneric <- function(response, predictor,callobject,  ...)
{
    vvolName <- gen_view_name("roccurve")
    vselect <- paste0(" SELECT a.vectorIndexColumn AS OBSID, a.vectorValueColumn as res, b.vectorValueColumn AS pred
                          FROM (",constructSelect(response),") AS a ,
                               (",constructSelect(predictor),") AS b
                          WHERE  a.vectorIndexColumn = b.vectorIndexColumn")
    tbl <- createTable(pTableName = vvolName,
                       pWithData = TRUE,
                       pTemporary = TRUE,
                       pSelect = vselect )
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
    return(new(vclass,
               otbl = ret$ResultTable,
               results = list(call = callobject,
                              itable = vvolName)
               ))
    
}


`$.FLROC`<-function(object,property){
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]
    if(property == "sensitivities"){
        ##TPR
        vquery <- paste0("SELECT TPR FROM ",object@otbl," ORDER BY TPR DESC ")
        df <- sqlQuery(connection, vquery)
        val <- as.numeric(df$TPR)
        return(val)
        ##return(as.numeric(df$TPR))
        
        
    }
    else if(property == "specificities"){
        vquery <- paste0("SELECT FPR FROM ",object@otbl," ORDER BY FPR DESC ")
        df <- sqlQuery(connection, vquery)
        val <- as.numeric(df$FPR)
        val <- 1 - val
        return(val)
    }
    else if(property == "auc")
    {
        return(auc(object))
    }
    else if(property == "original.predictors"){
        vquery <- paste0("SELECT pred FROM ",object@results$itable," ORDER BY OBSID")
        df <- sqlQuery(connection, vquery)
        return(as.numeric(df$pred))
    }
    else if(property == "original.response"){
        vquery <- paste0("SELECT res FROM ",object@results$itable," ORDER BY OBSID")
        df <- sqlQuery(connection, vquery)
        return(as.numeric(df$res))

    }
    else if(property == "levels"){print("need to do:")}
    else if(property == "controls"){
        vquery <- paste0("SELECT pred FROM ",object@results$itable," WHERE res = 0 ORDER BY OBSID")
        df <- sqlQuery(connection, vquery)
        return(as.numeric(df$pred)) }
    else if(property == "cases"){
        vquery <- paste0("SELECT pred FROM ",object@results$itable," WHERE res = 1 ORDER BY OBSID")
        df <- sqlQuery(connection, vquery)
        return(as.numeric(df$pred))}
    else if(property == "call"){
        return(object@results$call)}
    else if(property == "percent"){
        return(FALSE)}

}


auc.FLROC <- function(object, ...){
    reqList <- list(call = object$call,
                    cases = object$cases,
                    controls = object$controls,
                    original.predictor = object$original.predictor,
                    original.response = object$original.response,
                    percent = object$percent,
                    sensitivities =object$sensitivities ,
                    specificities = object$specificities
                    )
    class(reqList) <- "roc"
    return(auc(reqList, ...))  
}



plot.FLROC <- function(object, ...)
{
    reqList <- list(call = object$call,
                    cases = object$cases,
                    controls = object$controls,
                    original.predictor = object$original.predictor,
                    original.response = object$original.response,
                    percent = object$percent,
                    sensitivities =object$sensitivities ,
                    specificities = object$specificities,
                    auc = object$auc
                    )

    class(reqList) <- "roc"
    return(plot(reqList, ...))
}




