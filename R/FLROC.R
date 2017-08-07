NULL
## http://people.inf.elte.hu/kiss/11dwhdm/roc.pdf
#' Build a ROC Curve
#' 
#' \code{roc} is a diagnostic test which plots the true positive rate
#' against the false positive rate for a given test set.
#'
#' The wrapper overloads roc from pROC package and implicitly calls DB-Lytix roc function.
#' @param response a FLvector of responses, typically encoded with 0
#' (controls) and 1 (cases). Only two classes can be used in a ROC curve.
#' @param predictor a FLvector of the same length than response, containing the 
#' predicted value of each observation.
#' @return \code{roc} returns a FLROC class object which replicates equivalent R output
#' from \code{roc} in pROC package.
#' @seealso \code{\link[pROC]{roc}} for corresponding R function reference.
#' @examples
#' tbl <- FLTable(getTestTableName("tblROCCurve"),
#'               "ObsID")
#' mod <- roc(tbl$ActualVal, tbl$ProbVal)
#' print(mod)
#' plot(mod)
#' auc(mod)
#' mod$levels
#' 
#' roctbl <- FLTable(getTestTableName("tblROCcurve"), obs_id_colname = "ObsID")
#' rocmod <- roc.FLTable(ActualVal~ProbVal, data = roctbl)
#' @export
roc <- function (formula,data=list(),...) {
	UseMethod("roc", data)
}

#' @export
roc.default <- function (response, predictor,...) {
   if (!requireNamespace("pROC", quietly = TRUE)){
       stop("pROC package needed for roc. Please install it.",
            call. = FALSE)
   }
   else return(pROC::roc(response, predictor,...))
}



#' An S4 class to represent output from ROC on in-database Objects
setClass(
    "FLROC",
    contains="FLRegr",
    slots=list(otbl="character"))


#' ROC.
#' \code{roc} performs ROC  on FLvectors and FLTable objects.
#'
#' The DB Lytix function called is FLROC. Performs ROC and stores the results in
#' predefined tables. This function's main job is to build a ROC object.
#' Data can be provided as 'response', 'predictor', where the predictor is
#' the numeric (or ordered) level of the evaluated signal, and the response encodes the observation class (control or case).

#' @seealso \code{\link[pROC]{roc}} for R reference implementation.
#' @param formula A symbolic description of model to be made.
#' @param data An object of class FLTable.
#' @slot  results cache list of results computed
#' @slot votbl stores the table names of output ROC table.
#' @method print FLROC
#' @method plot FLROC
#' @method auc  FLLinRegr
#' @return \code{roc} returns an object of class \code{FLROC}
#' @examples
#' tbl <- FLTable("tblROCCurve", "ObsID")
#' mod <- roc(tbl$ActualVal, tbl$ProbVal)
#' @export
roc.FLVector <- function (response, predictor, ...)
{
    vcallObject <- match.call()
 ##   if(!is.FLVector(predictor))
 ##   {predictor <- as.FL(predictor)}
    return(rocgeneric(response = response,
                      predictor = predictor,
                      callobject = vcallObject,
                      ...))}


#' TO-DO:- implementation in rocgeneric
#' roctbl <- FLTable("tblROCcurve", obs_id_colname = "ObsID")
#' rocmod <- roc.FLTable(ActualVal~ProbVal, data = roctbl)
#' @export
roc.FLTable <- function(formula,data,... ){
  ## https://fuzzyl.atlassian.net/browse/FHD-80
    vcallObject <- match.call()
    var <- all.vars(formula)
    pId <- gsub("flt.","" ,data@select@variables$obs_id_colname)

    tname <- getTableNameSlot(data)[[1]]
    otbl <- gen_wide_table_name("roc")
    ret <- sqlStoredProc(connection,
                         "FLROCCurve",
                         InputTable = tname,
                         RecID = pId,
                         ActualColName = var[1],
                         ProbColName = var[2],
                         WhereClause = "NULL" ,
                         TableOutput = as.integer(1),
                         outputParameter = c(OutTable = 'a'),
                         ResultTable=otbl
                         )

    if(is.TD())
    otbl <- as.character(ret[[1]])

    if(is.TDAster()){
      vtemp <- as.FLTable(ret,tableName=otbl,uniqueIdColumn=1,batchSize=30)
    }

    rnames <- rownames(data)
    cnames <- colnames(data)
    vrw <- nrow(data)
    vclass <- "FLROC"
    quer <- paste0("SELECT COUNT(",var[1],") AS val FROM ",tname," GROUP BY ",var[1],"")
    df <- sqlQuery(connection, quer)

    return(new(vclass,
               otbl = otbl,
               results = list(call = vcallObject,
                              itable = tname,
                              Dimnames = list(row = rnames, col = cnames),
                              dims = c(vrw, 3),
                              vals = c(controls = df$val[2], cases =df$val[1] ),
                              doperator = list(Var = list(var[1], var[2]),
                                               Vorder = pId) )
               )
           )}

rocgeneric <- function(response, predictor,callobject,  ...)
{
    predCol <- "vectorValueColumn"
    predObs <- "vectorIndexColumn"
    vvolName <- gen_view_name("roccurve")
    if(strsplit(class(predictor), split = ".", fixed = TRUE)[[1]][1] == "FLMatrix"){
        predCol <- "valueColumn"
        predObs <- "rowIdColumn" }

    vselect <- paste0(" SELECT a.vectorIndexColumn AS OBSID, a.vectorValueColumn as res, b.",predCol," AS pred
                          FROM (",constructSelect(response),") AS a ,
                               (",constructSelect(predictor),") AS b
                          WHERE  a.vectorIndexColumn = b.",predObs,"")
    tbl <- createTable(pTableName = vvolName,
                       pWithData = TRUE,
                       pTemporary = TRUE,
                       pSelect = vselect,
                       pPrimaryKey="OBSID" )
    vrw <- nrow(response)
    rnames <- rownames(response)
    cnames <- c("ObsID", colnames(response), colnames(predictor))
    otbl <- gen_wide_table_name("roc")
    ret <- sqlStoredProc(connection,
                         "FLROCCurve",
                         InputTable = vvolName,
                         RecID = "ObsID",
                         ActualColName = "res",
                         ProbColName = "pred",
                         WhereClause = "NULL" ,
                         TableOutput = as.integer(1),
                         outputParameter = c(OutTable = 'a'),
                         ResultTable=otbl
                        )

    if(is.TD())
    otbl <- as.character(ret[[1]])
    
    if(is.TDAster()){
      vtemp <- as.FLTable(ret,tableName=otbl,uniqueIdColumn=1,batchSize=30)
    }

    vclass <- "FLROC"
    quer <- paste0("SELECT COUNT(res) AS val FROM ",vvolName," GROUP BY res")
    df <- sqlQuery(connection, quer)

    return(new(vclass,
               otbl = otbl,
               results = list(call = callobject,
                              itable = vvolName,
                              Dimnames = list(row = rnames, col = cnames),
                              dims = c(vrw, 3),
                              vals = c(controls = df$val[2], cases =df$val[1] ),
                              doperator = list(Var = list("res", "pred"),
                                               Vorder = "OBSID") )

               )
           )

}


setMethod("names", signature("FLROC"), function(x) c("sensitivities","specificities",
                                                          "original.predictor","original.response",
                                                          "levels",
                                                          "controls","cases",
                                                          "call",
                                                          "percent"
                                                          ))


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
                               variables=as.list(var),
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

    ## dname <- c(object@results$doperator$Vorder, object@results$doperator$Var[[2]])

    else if(property == "original.predictor"){
        return(flvgeneric(object,
                          tblname = object@results$itable,
                          var = list(OBSID= "ObsID", pred = object@results$doperator$Var[[2]]),
                          whereconditions = "" ,
                          vorder = object@results$doperator$Vorder,
                          dcolumn =c("OBSID", "pred") ))
    }
    ## var <- res, vorder <- "obsId", dcolumn <-c("obsID",res)
    else if(property == "original.response"){
        return(flvgeneric(object,
                          tblname = object@results$itable,
                          var = list(object@results$doperator$Var[[1]]),
                          whereconditions = "" ,
                          vorder = object@results$doperator$Vorder,
                          dcolumn = c(object@results$doperator$Vorder,object@results$doperator$Var[[1]] ))) }

    else if(property == "levels"){ return(0:1)} ## need to generalize

    else if(property == "controls"){
        return(flvgeneric(object,
                          tblname = object@results$itable,
                          var = list(OBSID = "ObsID", controls = object@results$doperator$Var[[2]]),
                          whereconditions = paste0(object@results$doperator$Var[[1]], "= 0"),
                          vorder = object@results$doperator$Vorder,
                          dcolumn = c("OBSID", "controls"),
                          dnames = (1:object@results$vals[1]),
                          vdims  = object@results$vals[1]
                          ))
    }

    else if(property == "cases"){
        return(flvgeneric(object,
                          tblname = object@results$itable,
                          var = list(OBSID = "ObsID", cases = object@results$doperator$Var[[2]]),
                          whereconditions = paste0(object@results$doperator$Var[[1]], "= 1"),
                          vorder = object@results$doperator$Vorder,
                          dcolumn = c("OBSID", "cases"),
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
    return(as.roc(object, limit,auc=TRUE,...)$auc)


#' @export
plot.FLROC <- function(object,limit = 1000,method = 1, ...)
    return(plot(as.roc(object, limit=limit, method = method), ...))

## from package pROC
print.roc.dataline <- function (x)
{
    if ("cases" %in% names(x$call) && "controls" %in% names(x$call)) {
        cat("Data: ", length(x$controls), " controls ", x$direction,
            " ", length(x$cases), " cases.\n", sep = "")
    }
    else {
        if ("predictor" %in% names(x$call))
            predictor.name <- as.character(x$call[match("predictor",
                names(x$call))])
        else if (!is.null(x$call$formula))
            predictor.name <- attr(terms(as.formula(x$call$formula)),
                "term.labels")
        else return()
        if ("response" %in% names(x$call))
            response.name <- as.character(x$call[match("response",
                names(x$call))])
        else if (!is.null(x$call$formula)) {
            formula.attrs <- attributes(terms(as.formula(x$call$formula)))
            response.name <- rownames(formula.attrs$factors)[formula.attrs$response]
        }
        else if ("x" %in% names(x$call))
            response.name <- as.character(x$call[match("x", names(x$call))])
        else return()
        cat("Data: ", predictor.name, " in ", length(x$controls),
            " controls (", response.name, " ", x$levels[1], ") ",
            x$direction, " ", length(x$cases), " cases (", response.name,
            " ", x$levels[2], ").\n", sep = "")
    }
}

## from package pROC
#' @export
print.FLROC <- function(x,method = 1, digits = max(3, getOption("digits") - 3), call = TRUE, ...) {
    if (call)
        cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    print.roc.dataline(x)
    if (!is.null(x$auc)) {
        print(x$auc, digits = digits, ...)
    }
    else cat("Area under the curve not computed.\n")
    if (!is.null(x$ci)) {
        print(x$ci, digits = digits, ...)
    }
    invisible(x)
}

setMethod("show", signature("FLROC"), function(object) print.FLROC(object,method=0))

#' @export
as.roc <- function(object,limit = 1000, auc=TRUE,method = 1, ... ){
    p <- min(limit,object@results$dims[[1]])/(object@results$dims[[1]])
    if(method)
    {
        vfrom1 <- gsub("ORDER BY FPR DESC", "", constructSelect(object$specificities))
        vfrom2 <- gsub("ORDER BY TPR","", constructSelect(object$sensitivities) )

        if(!is.Hadoop())
        str1 <- paste0("SELECT  b.spec AS spec, a.sen AS sen FROM (",vfrom1,") AS b, (",
                      vfrom2,") AS a WHERE a.ObSID = b.ObsID AND FLSimUniform(",getNativeRandFunction(pArg1=1,pArg2=10000),", 0.0, 1.0) < ",p," ")
        else str1 <- paste0("SELECT  b.spec AS spec, a.sen AS sen FROM (",vfrom1,") AS b, (",
                      vfrom2,") AS a, fzzlserial c WHERE a.ObSID = b.ObsID AND a.ObSID=c.serialval AND RAND(c.serialval) < ",p)
    } else {
        val <- object@results$vals
        neg <-1/val[[1]]
        pos <- 1/val[[2]]

        if(!is.Hadoop())
        str1 <- paste0("SELECT  TruePositives*",pos," AS sen , 1-(TRUENegatives*",neg,") AS spec FROM ",object@otbl,
                    " WHERE FLSimUniform(",getNativeRandFunction(pArg1=1,pArg2=10000),", 0.0, 1.0) < ",p," ORDER BY sen ASC")
        else str1 <- paste0("SELECT  a.TruePositives*",pos," AS sen , 1-(a.TRUENegatives*",neg,") AS spec FROM ",object@otbl,
                    " a, fzzlserial b WHERE a.ObSID=c.serialval AND RAND(c.serialval) < ",p, " ORDER BY sen ASC")
    }
    df <- sqlQuery(connection, str1)
    sen <- sort(as.numeric(df$sen), decreasing = TRUE)
    spec <- sort(as.numeric(df$spec))

    reqList <- structure(
        list(call = object$call,
##             cases = object$cases,
##             controls = object$controls,
             percent = object$percent,
             sensitivities =sen,
             specificities = spec,
             levels=0:1
             ),
        class="roc")
    if(auc) reqList$auc <- auc(reqList)
    return(reqList)
}

