## Overloading problems..
## Cannot call default R function
NULL

#' Cochran's Q test
#'
#' Performs the Cochran's Q test for unreplicated 
#' randomized block design experiments with a binary 
#' response variable and paired data.
#'
#' @param formula a formula of the form a ~ b | c, where a, b and c 
#' give the data values and corresponding groups and blocks, respectively.
#' @param data Wide FLTable or FLTableMD objects
#' @param alpha significance level to compute pairwise comparisons. Always 0.05.
#' @param p.method method for p-values correction. Not used currently.
#' @param ... The additional arguments used by FL function:
#' \code{whereconditions} WhereConditions to subset data
#' \code{GroupBy} Column names defining the different groups in data, if any.
#' @section Constraints:
#' p.value.multcomp is NA in result
#' @return A list with class \code{RVtest}.
#' A list of \code{RVtest} objects if the input is a FLTableMD object.
#' @examples
#' response <- c(0,1,1,0,0,1,0,1,1,1,1,1,0,0,1,1,0,1,0,1,1,0,0,1,0,1,1,0,0,1)
#' fact <- gl(3,1,30,labels=LETTERS[1:3])
#' block <- gl(10,3,labels=letters[1:10])
#' Rdata <- data.frame(response=response,block=block,fact=fact)
#' dropFLTestTable()
#' flt <- as.FLTable(Rdata,tableName="ARBaseTestTempTable")
#' RVtestObject <- cochran.qtest(response~fact|block,data=flt)
#' print(RVtestObject)
#' @export
setGeneric("cochran.qtest",
    function(formula, data=NULL,
            alpha=0.05,
            p.method="fdr",
            ...)
        standardGeneric("cochran.qtest"))

setMethod("cochran.qtest",
        signature(formula="formula", 
                  data="ANY"),
        function(formula, data=NULL,
                alpha=0.05,
                p.method="fdr",
                ...){
                    if(!requireNamespace("RVAideMemoire", 
                                        quietly = TRUE)){
                        stop("survival package needed for coxph. Please install it.",
                            call. = FALSE)
                    }
                    return(RVAideMemoire::cochran.qtest(formula=formula,
                                                      data=data,
                                                      alpha=alpha,
                                                      p.method=p.method,
                                                      ...))
                })

setMethod("cochran.qtest",
        signature(formula="formula", 
                  data="FLTable"),
        function(formula, data,
                alpha=0.05,
                p.method="fdr",
                ...){
                    data <- setAlias(data,"")
                    connection <- getOption("connectionFL")
                    if(data@isDeep)
                        stop("input table must be wide \n ")
                    vallVars <- all.vars(formula)
                    if(any(!vallVars %in% colnames(data)))
                        stop("columns specified in formula not in data \n ")
                    vBlockColname <- vallVars[3]
                    vGroupColname <- vallVars[2]
                    vValueColname <- vallVars[1]
                    vdata.name <- paste0(vValueColname," by ",vGroupColname,
                                        ", block = ",vBlockColname)
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
                                         "FLCochranQ",
                                         TableName = getTableNameSlot(data),
                                         BlockColName= vBlockColname,
                                         GroupColName = vGroupColname,
                                         ValueColname = vValueColname,
                                         WhereClause = list(...)[["whereconditions"]],
                                         GroupBy = vgrp,
                                         TableOutput = 1,
                                         outputParameter = c(ResultTable = 'a')
                                        )
                    ret <- as.character(ret[1,1])

                    ## To calculate estimate component in result
                    vestimate <- sqlQuery(connection,
                                        paste0("SELECT ",
                                                    ifelse(length(setdiff(vgrp,""))>0,
                                                        paste0("DENSE_RANK()OVER(ORDER BY ",
                                                                vgrp,")"),1)," AS groupID, \n ",
                                                    vGroupColname," AS statusVal, \n ",
                                                    "FLMean(",vValueColname,") AS estimate \n ",
                                               " FROM ",getTableNameSlot(data)," \n ",
                                               " GROUP BY groupID,",vGroupColname," \n ",
                                                " ORDER BY groupID,",vGroupColname
                                            )
                                        )

                    VarID <- c("T_Stat","P_Value")
                    vres <- sqlQuery(connection,
                                        paste0("SELECT ",
                                                    ifelse(length(setdiff(vgrp,""))>0,
                                                        paste0("DENSE_RANK()OVER(ORDER BY ",
                                                                vgrp,")"),1)," AS groupID, \n ",
                                                paste0(VarID,collapse=",")," \n ",
                                               " FROM ",ret," \n ",
                                                " ORDER BY groupID"
                                            )
                                    )
                    colnames(vres) <- c("groupID",VarID)
                    vresList <- dlply(vestimate,"groupID",
                                    function(x){
                                        vest <- x[["estimate"]]
                                        names(vest) <- paste0("proba in group ",x[["statusVal"]])
                                        vest <- as.array(vest)
                                        vstats <- vres[vres[,"groupID"]==unique(x[["groupID"]]),]
                                        vtemp <- list(method.test="Cochran's Q test",
                                                      data.name=vdata.name,
                                                      statistic=c(Q=vstats[["T_Stat"]]),
                                                      parameter=c(df=length(vest)-1),
                                                      alternative="two.sided",
                                                      null.value=c("difference in probabilities"=0),
                                                      p.value=vstats[["P_Value"]],
                                                      estimate=vest,
                                                      alpha=alpha,
                                                      method.multcomp="Wilcoxon sign test",
                                                      p.adjust.method="fdr",
                                                      p.value.multcomp=NA
                                                      )
                                        class(vtemp) <- "RVtest"
                                        return(vtemp)
                                    })
                    names(vresList) <- 1:length(vresList)
                    if(length(vresList)==1)
                        vresList <- vresList[[1]]
                    return(vresList)
})

setMethod("cochran.qtest",
        signature(formula="formula", 
                  data="FLTableMD"),
        function(formula, data,
                alpha=0.05,
                p.method="fdr",
                ...){
                    class(data) <- "FLTable"
                    return(cochran.qtest(formula=formula,
                                        data=data,
                                        alpha=alpha,
                                        p.method=p.method,
                                        ...))
                })