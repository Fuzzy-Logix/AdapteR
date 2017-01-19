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
                      ...))}

#' @export
roc.FLTable <- roc.FLVector


#' @export
roc.FLTableMD <- roc.FLVector

rocgeneric <- function(response, predictor,callobject,  ...)
{
    browser()
    vvolName <- gen_view_name("roccurve")
    vselect <- paste0(" SELECT a.vectorIndexColumn AS OBSID, a.vectorValueColumn as res, b.vectorValueColumn AS pred
                          FROM (",constructSelect(response),") AS a ,
                               (",constructSelect(predictor),") AS b
                          WHERE  a.vectorIndexColumn = b.vectorIndexColumn")
    tbl <- createTable(pTableName = vvolName,
                       pWithData = TRUE,
                       pTemporary = TRUE,
                       pSelect = vselect )
    vrw <- nrow(response)
    rnames <- rownames(response)
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
    return(new(vclass,
               otbl = ret$ResultTable,
               results = list(call = callobject,
                              itable = vvolName,
                              Dimnames = list(row = rnames, col = cnames),
                              dims = c(vrw, 3))
               ))
    
}


`$.FLROC`<-function(object,property){
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]
    
    flvgeneric <- function(object, vquery){
        tblfunqueryobj <- new("FLTableFunctionQuery",
                              connectionName = getFLConnectionName(),
                              variables = list(
                                  obs_id_colname = "vectorIndexColumn",
                                  cell_val_colname = "vectorValueColumn"),
                              whereconditions="",
                              order = "",
                              SQLquery=vquery)

        yvector <- newFLVector(
            select = tblfunqueryobj,
            Dimnames = list(object@results$Dimnames$row,
                            "vectorValueColumn"),
            dims = as.integer(c(object@results$dims[[1]],1)),
            isDeep = FALSE)
        return(yvector)}

    if(property == "sensitivities"){
        sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,
                                  a.OBSID AS vectorIndexColumn,
                                  a.TPR  AS vectorValueColumn
                          FROM ",object@otbl," AS a  ORDER BY TPR DESC")
        return(flvgeneric(object, sqlstr))
    }
    
    else if(property == "specificities"){
        sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,
                                  a.OBSID AS vectorIndexColumn,
                                  (1-a.FPR)  AS vectorValueColumn
                          FROM ",object@otbl," AS a  ORDER BY FPR DESC")
        return(flvgeneric(object, sqlstr))
    }
    else if(property == "auc") {
        return(auc(object)) }
    
    else if(property == "original.predictors"){
        sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,
                                  a.OBSID AS vectorIndexColumn,
                                  a.pred  AS vectorValueColumn
                         FROM ",object@results$itable," AS a ORDER BY OBSID")

        return(flvgeneric(object, sqlstr))}

    else if(property == "original.response"){
        sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,
                                  a.OBSID AS vectorIndexColumn,
                                  a.res  AS vectorValueColumn
                         FROM ",object@results$itable," AS a ORDER BY OBSID")
        return(flvgeneric(object, sqlstr))
    }

    else if(property == "levels"){print("need to do:")}

    else if(property == "controls"){
        sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,
                                  a.OBSID AS vectorIndexColumn,
                                  a.pred  AS vectorValueColumn
                         FROM ",object@results$itable," AS a WHERE res = 0  ORDER BY OBSID")
        return(flvgeneric(object, sqlstr)) }

    else if(property == "cases"){
        vquery <- paste0("SELECT pred FROM ",object@results$itable," WHERE res = 1 ORDER BY OBSID")
            sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,
                                  a.OBSID AS vectorIndexColumn,
                                  a.pred  AS vectorValueColumn
                         FROM ",object@results$itable," AS a WHERE res = 1  ORDER BY OBSID")
        return(flvgeneric(object, sqlstr)) }
    
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
                    sensitivities =as.vector(object$sensitivities) ,
                    specificities = as.vector(object$specificities)
                    )
    class(reqList) <- "roc"
    return(auc(reqList, ...))  
}



plot.FLROC <- function(object, ...)
{
    p <- min(limit,length(object$fitted.values))/length(object$fitted.values)

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


print.FLROC <- function(object, ...){
    reqList <- list(call = object$call,
                    auc = object$auc)
    class(reqList) <- "roc"
    return(print(reqList, ...))
}
