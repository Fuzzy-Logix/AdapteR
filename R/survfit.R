NULL
#' Compute a Survival Curve for Censored Data
#'
#' Computes an estimate of a survival curve for censored data
#'
#' @param formula a formula object, which must have a Surv object 
#' as the response on the left of the ~ operator and 1 on right side(Single survival curve)
#' @param data Wide FLTable or FLTableMD objects
#' @param weights Not currently used
#' @param subset Not currently used
#' @param na.action na values omitted always
#' @param etype Not currently used
#' @param id Not currently used
#' @param istate Not currently used
#' @param ... The additional arguments used by FL function:
#' \code{conf.int} the level for a two-sided confidence interval on the survival curve(s). Default is 0.95.
#' \code{whereconditions} WhereConditions to subset data
#' \code{GroupBy} Column names defining the different groups in data, if any.
#' @section Constraints:
#' Only Single survival curve supoorted currently.(RHS of formula is 1)
#' Only conf.type='plain' supported.
#' @return A list with class \code{survfit}.
#' A list of \code{survfit} objects if the input is a FLTableMD object.
#' @examples
#' fltMD <- FLTableMD("vwWHAS100","DataSetID","ObsID")
#' resultList <- survfit(Surv(TIME_VAL,STATUS)~1,data=fltMD)
#' print(resultList[[1]])
#' summary(resultList[[1]])
#' plot(resultList[[1]])
#' @export
survfit <- function(...) UseMethod("survfit")

#' @export
survfit.formula <- function(formula,data,...){
    if(!is.FL(data))
        if (!requireNamespace("survival", quietly = TRUE)){
            stop("survival package needed for survfit. Please install it.",
            call. = FALSE)
            }
        else
            return(survival::survfit.formula(formula=formula,
                                        data=data,
                                        ...))
    else return(FLsurvfit(formula=formula,data=data,...))
}


#' @export
FLsurvfit <- function(formula, data, weights, 
                            subset, na.action,
                            etype, id, istate,...){
    
        data <- setAlias(data,"")
        connection <- getFLConnection()
        if(isDeep(data))
            stop("input table must be wide \n ")
        vtemp <- prepareSurvivalFormula(data=data,
                                        formula=formula)
        for(i in names(vtemp))
        assign(i,vtemp[[i]])

        vAlpha <- 0.05
        if("conf.int" %in% names(list(...)))
            vAlpha <- (1-list(...)[["conf.int"]])

        vcall <- match.call()
        vobsIDCol <- getVariables(data)[["obs_id_colname"]]
        # vgroupCols <- unique(c(vobsIDCol,list(...)[["GroupBy"]]))
        vgroupCols <- unique(c(getVariables(data)[["group_id_colname"]],
                                list(...)[["GroupBy"]]))
        if(any(!setdiff(vgroupCols,vobsIDCol) %in% colnames(data)))
            stop("columns specified in GroupBy not in data:Hint:check Case-Sensitivity \n ")
        vgrp <- paste0(vgroupCols,collapse=",")
        if(!length(vgroupCols)>0)
            vgrp <- "NULL"

        vwhereClause <- list(...)[["whereconditions"]]
        if(is.null(vwhereClause))
            vwhereClause <- "NULL"

        ret <- sqlStoredProc(connection,
                             "FLKaplanMeier",
                             TableName = getTableNameSlot(data),
                             TimeColName= vTimeVal,
                             StatusColName = vStatus,
                             Alpha = vAlpha,
                             WhereClause = vwhereClause,
                             GroupBy = vgrp,
                             TableOutput = 1,
                             outputParameter = c(ResultTable = 'a')
                            )
        colnames(ret) <- tolower(colnames(ret))
        if("resulttable" %in% colnames(ret))
            ret <- as.character(ret[1,1])
        else{
            ret <- as.FLTable(ret,uniqueIdColumn=1,
                            temporary=TRUE,
                            tableName="ARKaplanMeierResults",
                            drop=TRUE)
            ret <- "ARKaplanMeierResults"
        }

        if(vwhereClause=="NULL")
            vwhereClause <- NULL
        if(vgrp=="NULL")
            vgrp <- NULL
        vgrpframe <- sqlQuery(connection,
                            paste0("SELECT DISTINCT ",
                                        ifelse(length(setdiff(vgrp,""))>0,
                                        paste0(vgrp,","),""),
                                        " COUNT(1) AS cnt \n ",
                                   " FROM ",getTableNameSlot(data)," \n ",
                                   constructWhere(list(...)[["whereconditions"]]),
                                    ifelse(length(setdiff(vgrp,""))>0,
                                        paste0(" GROUP BY ",vgrp," \n "),""),
                                    ifelse(length(setdiff(vgrp,""))>0,
                                        paste0(" ORDER BY ",vgrp," \n "),"")
                                    )
                            )
        colnames(vgrpframe) <- c(vgroupCols,"cnt")

        fGenFLVector <- function(colName,grpValues,ObsID){
            VarID <- c(names(grpValues),"TimeIndex",vTimeVal,
                        "NumAtRisk","NumEvents","Censored",
                        "CumEvents","CumCensored","KaplanMeier","StdErr",
                        "PetoEst","LowerLimit","UpperLimit"
                        )
            vwhereConds <- ""
            if(length(grpValues)>0)
                vwhereConds <- paste0(names(grpValues),
                                        " IN (",fquote(grpValues),
                                        ")")
            vselect <- new("FLSelectFrom",
                          connectionName = attr(connection,"name"), 
                          table_name = ret,
                          variables = list(
                              obs_id_colname = "TimeIndex"),
                          whereconditions=vwhereConds,
                          order = "")
            vFLtbl <- newFLTable(
                          select=vselect,
                          Dimnames=list(ObsID,VarID),
                          dims=as.integer(c(length(ObsID),length(VarID))),
                          type="double",
                          isDeep=FALSE)
            return(vFLtbl[,colName])
        }
        # fGenFLVector("TIME_VAL",apply(vgrpframe[1,-3],1,function(x)x)[,1],ObsID=1:62)
        vresList <- apply(vgrpframe,1,
                        function(x){
                            vcnt <- x["cnt"]
                            names(vcnt) <- NULL
                            x = x[setdiff(names(x),"cnt")]
                            vresList <- list(
                                            n = vcnt,
                                            time = fGenFLVector(ifelse(is.TDAster(),
                                                                        "Time_Val",
                                                                        vTimeVal),
                                                                x,ObsID=1:vcnt),
                                            n.risk = fGenFLVector("NumAtRisk",x,ObsID=1:vcnt),
                                            n.event = fGenFLVector("NumEvents",x,ObsID=1:vcnt),
                                            n.censor = fGenFLVector("Censored",x,ObsID=1:vcnt),
                                            surv = fGenFLVector("KaplanMeier",x,ObsID=1:vcnt),
                                            type = "right",
                                            strata = NULL,
                                            std.err = fGenFLVector("StdErr",x,ObsID=1:vcnt),
                                            upper = fGenFLVector("UpperLimit",x,ObsID=1:vcnt),
                                            lower = fGenFLVector("LowerLimit",x,ObsID=1:vcnt),
                                            conf.type = "plain",
                                            conf.int = 1-vAlpha,
                                            call = vcall,
                                            PetoEstimate = fGenFLVector("PetoEst",x,ObsID=1:vcnt)
                                            )
                            class(vresList) <- "FLSurvfit"
                            return(vresList)
                        })
        
        ## Names of List similar to dlply
        vgrpframe[["cnt"]] <- NULL
        names(vresList) <- apply(vgrpframe,1,
                                paste0,collapse=".")

        if(length(vresList)==1)
            vresList <- vresList[[1]]
        return(vresList)
    }


#' @export
fFetchFLSurvfit <- function(pObject){
    vresList <- lapply(pObject,
                    function(x){
                        as.vector(x)
                })
    class(vresList) <- "survfit"
    return(vresList)
}

#' @export
print.FLSurvfit <- function(x,...){
    vresList <- fFetchFLSurvfit(pObject=x)
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
                        "(",fixed=T))[2],",",fixed=T))[1]
    assign(parentObject,vresList,envir=parent.frame())
    print(vresList)
}
#' @export
plot.FLSurvfit <- function(x,...){
    vresList <- fFetchFLSurvfit(pObject=x)
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
                        "(",fixed=T))[2],",",fixed=T))[1]
    assign(parentObject,vresList,envir=parent.frame())
    plot(vresList)
}
#' @export
summary.FLSurvfit <- function(x,...){
    vresList <- fFetchFLSurvfit(pObject=x)
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
                        "(",fixed=T))[2],",",fixed=T))[1]
    assign(parentObject,vresList,envir=parent.frame())
    summary(vresList)
}
