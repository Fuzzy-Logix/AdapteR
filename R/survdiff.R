## Overloading problems..
## Cannot call default R function
NULL

#' Test Survival Curve Differences
#'
#' Tests if there is a difference between two or more 
#' survival curves using the G-rho family of tests, 
#' or for a single curve against a known alternative.
#'
#' @param formula a formula expression as for other 
#' survival models, of the form Surv(time, status) ~ predictor.
#' @param data Wide FLTable or FLTableMD objects
#' @param subset Not currently used
#' @param na.action na values omitted always
#' @param rho a scalar parameter that controls the type of test.
#' For FL objects, supported values are 0(LogRank),1(Peto-Prentice),
#' LogRank,Peto-Prentice,Modified Peto-Prentice,Wilcoxon,Taron-Ware.
#' @param ... The additional arguments used by FL function:
#' \code{conf.int} the level for a two-sided confidence interval on the survival curve(s). Default is 0.95.
#' \code{whereconditions} WhereConditions to subset data
#' \code{GroupBy} Column names defining the different groups in data, if any.
#' @section Constraints:
#' Only single predictor is supported in formula.
#' \code{var} is not available in the output.
#' rho values supported are mentioned above.
#' @return A list with class \code{survdiff}.
#' A list of \code{survdiff} objects if the input is a FLTableMD object.
#' @examples
#' fltMD <- FLTableMD("vwWHAS100","DataSetID","ObsID")
#' resultList <- survdiff(Surv(TIME_VAL,STATUS)~Gender,data=fltMD)
#' print(resultList[[1]])
#' @export
setGeneric("survdiff",
    function(formula, data,
            subset=TRUE, 
            na.action=getOption("na.action"), 
            rho = 0,
            ...)
        standardGeneric("survdiff"))

setMethod("survdiff",
        signature(formula="formula", 
                  data="ANY"),
        function(formula, data,
                subset=TRUE, 
                na.action=getOption("na.action"), 
                rho = 0,
                ...){
                    return(survival::survdiff(formula=formula,
                                              data=data,
                                              subset=subset,
                                              na.action=na.action,
                                              rho=rho,
                                              ...))
                })

setMethod("survdiff",
        signature(formula="formula", 
                  data="FLTable"),
        function(formula, data,
                subset=TRUE, 
                na.action=getOption("na.action"), 
                rho = 0,
                ...){
                    rho <- rho[1]
                    vRhoMap <- c("0"="LogRank",
                                 "1"="PetoPrentice",
                                 "LOGRANK"="LogRank",
                                 "PETO-PRENTICE"="PetoPrentice",
                                 "MODIFIED PETO-PRENTICE"="ModPetoPrent",
                                 "WILCOXON"="Wilcoxon",
                                 "TARON-WARE"="TaronWare")
                    vRho <- vRhoMap[toupper(as.character(rho))]
                    if(is.na(vRho))
                        stop("Allowed rho values are:",unique(c(vRhoMap,0,1))," \n ")
                    data <- setAlias(data,"")
                    connection <- getOption("connectionFL")
                    if(data@isDeep)
                        stop("input table must be wide \n ")
                    vtemp <- prepareSurvivalFormula(data=data,
                                                    formula=formula)
                    for(i in names(vtemp))
                    assign(i,vtemp[[i]])

                    if(!length(vIndepVars)==1)
                        stop("Invalid formula:check function documentation for constraints on formula \n ")

                    vAlpha <- 0.05
                    if("conf.int" %in% names(list(...)))
                        vAlpha <- (1-list(...)[["conf.int"]])
                    vcall <- match.call()
                    vobsIDCol <- getVariables(data)[["obs_id_colname"]]
                    # vgroupCols <- unique(c(vobsIDCol,list(...)[["GroupBy"]]))
                    vgroupCols <- unique(c(getVariables(data)[["group_id_colname"]],
                                list(...)[["GroupBy"]]))
                    if(any(!setdiff(vgroupCols,vobsIDCol) %in% colnames(data)))
                        stop("columns specified in GroupBy not in data \n ")
                    vgrp <- paste0(vgroupCols,collapse=",")
                    if(!length(vgroupCols)>0)
                        vgrp <- NULL

                    ret <- sqlStoredProc(connection,
                                         "FLKMHypoTest",
                                         TableName = getTableNameSlot(data),
                                         TimeColName= vTimeVal,
                                         StatusColName = vStatus,
                                         SampleIDColname = vIndepVars,
                                         Alpha = vAlpha,
                                         WhereClause = list(...)[["whereconditions"]],
                                         GroupBy = vgrp,
                                         TableOutput = 1,
                                         outputParameter = c(ResultTable = 'a')
                                        )
                    ret <- as.character(ret[1,1])

                    VarID <- c(vIndepVars,
                                "Obs","NumEvents",
                                "Expected","ChiSqApprox",
                                "ChiSq","Prob"
                               )
                    vres <- sqlQuery(connection,
                                        paste0("SELECT ",
                                                    ifelse(length(setdiff(vgrp,""))>0,
                                                        paste0("DENSE_RANK()OVER(ORDER BY ",
                                                                vgrp,")"),1)," AS groupID, \n ",
                                                paste0(VarID,collapse=",")," \n ",
                                               " FROM ",ret," \n ",
                                               " WHERE TestType IN(",fquote(vRho),")",
                                                " ORDER BY groupID,",vIndepVars
                                            )
                                    )
                    colnames(vres) <- c("groupID",VarID)
                    vresList <- dlply(vres,"groupID",
                                    function(x){
                                        n <- x[["Obs"]]
                                        names(n) <- paste0(vIndepVars,"=",
                                                            x[[vIndepVars]])
                                        vtemp <- list(n=as.table(n),
                                                      obs=x[["NumEvents"]],
                                                      exp=x[["Expected"]],
                                                      chisq=x[["ChiSq"]][1],
                                                      call=vcall,
                                                      p.value=x[["Prob"]][1],
                                                      chisqApprox=x[["ChiSqApprox"]],
                                                      var=matrix(NA,length(n),length(n))
                                                      )
                                        class(vtemp) <- "survdiff"
                                        return(vtemp)
                                    })
                    names(vresList) <- 1:length(vresList)
                    if(length(vresList)==1)
                        vresList <- vresList[[1]]
                    return(vresList)
})

setMethod("survdiff",
        signature(formula="formula", 
                  data="FLTableMD"),
        function(formula, data,
                subset=TRUE, 
                na.action=getOption("na.action"), 
                rho = 0,
                ...){
                    class(data) <- "FLTable"
                    return(survdiff(formula=formula,
                                    data=data,
                                    subset=subset,
                                    na.action=na.action,
                                    rho=rho,
                                    ...))
                    })