
## http://people.inf.elte.hu/kiss/11dwhdm/roc.pdf
#' tbl <- FLTable("tblROCCurve", "ObsID")
#' mod <- roc(tbl$ActualVal, tbl$ProbVal)
#'
#'
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
    browser()
    vvolName <- gen_view_name("roccurve")

    ##    sqlstr <- paste0("CREATE VOLATILE TABLE ",vvolName," SELECT a.vectorValueColumn as res, b.vectorValueColumn AS pred
    ##                          FROM ",response@select@table_name," AS a ,
    ##                               ",predictor@select@table_name," AS b
    ##                          WHERE ",ifelse(length(response@select@whereconditions),
    ##                                         response@select@whereconditions," "),"",var,"
    ##                                ",ifelse(length(predictor@select@whereconditions),
    ##                                         predictor@select@whereconditions," " ),"
    ##WITH DATA
    ##ON COMMIT PRESERVE ROWS;")
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
               results = list(call = callobject)
               ))
    
}


`$.FLROC`<-function(object,property){
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]
    if(property == "sensitivities"){
        ##TPR
        vquery <- paste0("SELECT TPR FROM ",object@otbl," ORDER BY TPR DESC ")
        df <- sqlQuery(connection, vquery)
        return(as.numeric(df$TPR))
        
    }
    else if(property == "specificities"){
        vquery <- paste0("SELECT FPR FROM ",object@otbl," ORDER BY FPR ASC ")
        df <- sqlQuery(connection, vquery)
        return(as.numeric(df$FPR))
    }
    else if(property == "auc")
    {
        return(auc(object, ...))
    }
    
    
}


auc.FLROC <- function(object, ...){
    reqList <- list(call = NULL ,
                    percent = FALSE,
                    sensitivities =object$sensitivities ,
                    specificities = object$specificities
                    )
    class(reqList) <- "roc"
    return(auc(reqList, ...))  
}



plot.FLROC <- function(object, ...)
{
##    reqList <- list(call = NULL ,
##                    percent = FALSE,
##                    sensitivities =object$sensitivities ,
##                    specificities = object$specificities
##                    )
##    class(reqList) <- "roc"
    ##    return(plot(reqList, ...))
    vquery <- paste0("SELECT TPR, FPR FROM ",object@otbl," ")
    df <- sqlQuery(connection, vquery)
    return(plot(val$FPR, val$TPR))
}
