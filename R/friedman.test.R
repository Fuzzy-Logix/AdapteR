
NULL

#' Friedman Rank Sum Test
#'
#' Performs a Friedman rank sum test with unreplicated blocked data.
#'
#' @param y FLVector with data values or data FLMatrix
#' @param groups FLVector giving the group for the 
#' corresponding elements of y if y is a FLVector; ignored if y is FLMatrix.
#' @param blocks FLVector giving the block for the 
#' corresponding elements of y if y is a FLVector; ignored if y is FLMatrix.
#' @param formula a formula of the form a ~ b | c, where a, b and c 
#' give the data values and corresponding groups and blocks, respectively.
#' Not applicable if FLVector or FLMatrix is input.
#' @param data FLTable or FLTableMD objects.
#' @param subset Not currently used.
#' @param na.action na values omitted always.
#' @param ... The additional arguments used by FL function:
#' \code{whereconditions} WhereConditions to subset data
#' \code{GroupBy} Column names defining the different groups in data, if any.
#' @return A list with class \code{htest}.
#' A list of \code{htest} objects if the input is a FLTableMD object.
#' @examples
#' RoundingTimes <-
#' matrix(c(5.40, 5.50, 5.55,
#'        5.85, 5.70, 5.75,
#'       5.20, 5.60, 5.50,
#'       5.55, 5.50, 5.40,
#'       5.90, 5.85, 5.70,
#'       5.45, 5.55, 5.60,
#'       5.40, 5.40, 5.35,
#'       5.45, 5.50, 5.35,
#'       5.25, 5.15, 5.00,
#'       5.85, 5.80, 5.70,
#'       5.25, 5.20, 5.10,
#'       5.65, 5.55, 5.45,
#'       5.60, 5.35, 5.45,
#'       5.05, 5.00, 4.95,
#'       5.50, 5.50, 5.40,
#'       5.45, 5.55, 5.50,
#'       5.55, 5.55, 5.35,
#'       5.45, 5.50, 5.55,
#'       5.50, 5.45, 5.25,
#'       5.65, 5.60, 5.40,
#'       5.70, 5.65, 5.55,
#'       6.30, 6.30, 6.25),
#'     nrow = 22,
#'     byrow = TRUE,
#'     dimnames = list(1 : 22,
#'                     c("Round Out", "Narrow Angle", "Wide Angle")))
#' FLMatrixObj <- as.FLMatrix(RoundingTimes)
#' result1 <- friedman.test(FLMatrixObj)
#' print(result1)
#' wb <- aggregate(warpbreaks$breaks,
#'              by = list(w = warpbreaks$wool,
#'                        t = warpbreaks$tension),
#'              FUN = mean)
#' FLTableObj <- as.FLTable(wb,tableName="ARBaseTestTempTable",drop=TRUE)
#' result2 <- friedman.test(FLTableObj$x, FLTableObj$w, FLTableObj$t)
#' print(result2)
#' result3 <- friedman.test(x~w|t, data = FLTableObj)
#' print(result3)
#' fltMD <- FLTableMD("tblFriedmanTest","datasetid","obsid","groupid","num_val")
#' result4 <- friedman.test(x~w|t, data = fltMD)
#' print(result4)
#' @export
friedman.test.FLVector <- function(y,groups,blocks,...){
    if(!is.FLVector(groups) && is.numeric(groups))
        if(is.FLVector(blocks) || is.numeric(blocks))
        groups <- as.FLVector(groups)
    if(!is.FLVector(blocks) && is.numeric(blocks))
        blocks <- as.FLVector(blocks)
    if(!is.FLVector(groups) || !is.FLVector(blocks))
        stop("groups and blocks arguments should be FLVectors \n ")
    if(!length(y)==length(groups) || !length(y)==length(blocks))
        stop("y,groups and blocks should have same length \n ")

    DNAME <- paste0(deparse(substitute(y)),", ",
                    deparse(substitute(groups)),
                    " and ",deparse(substitute(blocks)))
    vView <- gen_view_name("Friedman")
    vSelect <- paste0("SELECT 1 AS DatasetID, \n ",
                            " c.vectorValueColumn AS ObsID, \n ",
                            " b.vectorValueColumn AS VarID, \n ",
                            " a.vectorValueColumn AS Num_Val \n ",
                      " FROM (",constructSelect(y),") AS a, \n ",
                            "(",constructSelect(groups),") AS b, \n ",
                            "(",constructSelect(blocks),") AS c \n ",
                      " WHERE a.vectorIndexColumn = b.vectorIndexColumn AND ",
                            "a.vectorIndexColumn = c.vectorIndexColumn ")
    vtemp <- createView(vView,pSelect=vSelect)

    vtable <- FLTableMD(vView,
                        group_id_colname="DatasetID",
                        obs_id_colname="ObsID",
                        var_id_colname="VarID",
                        cell_val_colname="Num_Val")
    return(friedman.test(Num_Val~ObsID|VarID,
                        data=vtable,
                        data.name=DNAME))
}

friedman.test.FLMatrix <- function(y,...){
    DNAME <- deparse(substitute(y))
    vView <- gen_view_name("Friedman")
    vtemp <- createView(vView,
                        pSelect=gsub("'%insertIDhere%'",1,
                                    constructSelect(y)))

    vtable <- FLTableMD(vView,
                        group_id_colname="MATRIX_ID",
                        obs_id_colname="rowIdColumn",
                        var_id_colname="colIdColumn",
                        cell_val_colname="valueColumn")
    return(friedman.test(valueColumn~colIdColumn|rowIdColumn,
                        data=vtable,
                        data.name=DNAME))
}


## S3 overload not working for default R calls:
## Error: Evaluation nested deeply.
## Becasuse stats comes after AdapteR in search path.
# friedman.test.formula <- function(formula, data,
#                                   subset,na.action,
#                                   ...){
#     if(!is.FL(data))
#         return(stats::friedman.test(formula=formula,
#                                     data=data,
#                                     subset=subset,
#                                     na.action=na.action,
#                                     ...))
#     else{
#         data <- setAlias(data,"")
#         connection <- getFLConnection()
#         if(data@isDeep){
#             vBlockColname <- getVariables(data)[["obs_id_colname"]]
#             vGroupColname <- getVariables(data)[["var_id_colname"]]
#             vValueColname <- getVariables(data)[["cell_val_colname"]]
#         }
#         else{
#             vallVars <- all.vars(formula)
#             if(any(!vallVars %in% colnames(data)))
#                 stop("columns specified in formula not in data \n ")
#             vBlockColname <- vallVars[3]
#             vGroupColname <- vallVars[2]
#             vValueColname <- vallVars[1]
#         }
#         vdata.name <- list(...)[["data.name"]]
#         if(is.null(vdata.name))
#             vdata.name <- paste0(vValueColname,", ",vGroupColname,
#                                 " and ",vBlockColname)
#         vobsIDCol <- getVariables(data)[["obs_id_colname"]]

#         # vgroupCols <- unique(c(vobsIDCol,list(...)[["GroupBy"]]))
#         vgroupCols <- unique(c(getVariables(data)[["group_id_colname"]],
#                             list(...)[["GroupBy"]]))
#         if(is.wideFLTable(data) &&
#             any(!setdiff(vgroupCols,vobsIDCol) %in% colnames(data)))
#             stop("columns specified in GroupBy not in data \n ")
#         vgrp <- paste0(vgroupCols,collapse=",")
#         if(!length(vgroupCols)>0)
#             vgrp <- NULL

#         ret <- sqlStoredProc(connection,
#                              "FLFriedmanTest",
#                              TableName = getTableNameSlot(data),
#                              ValueColname = vValueColname,
#                              ObsIDColName= vBlockColname,
#                              SampleIDColName = vGroupColname,
#                              WhereClause = list(...)[["whereconditions"]],
#                              GroupBy = vgrp,
#                              TableOutput = 1,
#                              outputParameter = c(OutTable = 'a')
#                             )
#         ret <- as.character(ret[1,1])

#         VarID <- c(statistic="TEST_STAT",
#                     p.value="Prob")
#         vdf <- sqlQuery(connection,
#                             paste0("SELECT COUNT(DISTINCT a.",
#                                         vGroupColname,")-1 AS df \n ",
#                                    " FROM ",getTableNameSlot(data)," a \n ",
#                                    constructWhere(list(...)[["whereconditions"]])," \n ",
#                                    ifelse(length(setdiff(vgrp,""))>0,
#                                             paste0("GROUP BY ",vgrp, " \n "),""),
#                                    ifelse(length(setdiff(vgrp,""))>0,
#                                             paste0("ORDER BY ",vgrp),"")
#                                 )
#                         )
#         vdf <- vdf[[1]]
#         vres <- sqlQuery(connection,
#                         paste0("SELECT ",paste0(VarID,collapse=",")," \n ",
#                                 "FROM ",ret," \n ",
#                                 ifelse(length(setdiff(vgrp,""))>0,
#                                         paste0("ORDER BY ",vgrp),"")))

#         vres <- cbind(groupID=1:nrow(vres),vres)
#         colnames(vres) <- c("groupID",names(VarID))

#         vresList <- dlply(vres,"groupID",
#                         function(x){
#                             vtemp <- list(statistic=c("Friedman chi-squared"=x[["statistic"]]),
#                                           parameter=c(df=vdf[x[["groupID"]]]),
#                                           p.value=x[["p.value"]],
#                                           method="Friedman rank sum test",
#                                           data.name=vdata.name
#                                           )
#                             class(vtemp) <- "htest"
#                             return(vtemp)
#                         })
#         names(vresList) <- 1:length(vresList)
#         if(length(vresList)==1)
#             vresList <- vresList[[1]]
#         vtemp <- dropView(getTableNameSlot(data))
#         return(vresList)
#     }
# }

## S4 implementation because S3 not working for formula input case.
#' @export
setGeneric("friedman.test",
    function(y,
            ...)
        standardGeneric("friedman.test"))

## Not working: Environments related error.
## In the default R implementation, environments
## are used.
setMethod("friedman.test",
        signature(y="ANY"),
        function(y,
                ...){
                    return(stats::friedman.test(y=y,
                                    ...))
                })

setMethod("friedman.test",
        signature(y="FLVector"),
        function(y,groups,blocks,
                ...){
                    return(friedman.test.FLVector(y=y,
                                    groups=groups,
                                    blocks=blocks,
                                    ...))
                })

setMethod("friedman.test",
        signature(y="FLMatrix"),
        function(y,
                ...){
                    return(friedman.test.FLMatrix(y=y,
                                    ...))
                })

# setMethod("friedman.test",
#         signature(formula="formula", 
#                   data="ANY"),
#         function(formula, data,
#                 subset=TRUE, 
#                 na.action=getOption("na.action"),
#                 ...){
#                     return(stats::friedman.test(formula=formula,
#                                     data=data,
#                                     subset=subset,
#                                     na.action=na.action,
#                                     ...))
#                 })

setMethod("friedman.test",
        signature(y="formula"),
        function(formula, data,
                subset=TRUE, 
                na.action=getOption("na.action"),
                y=NULL,
                ...){
                    if(!is.FL(data)){
                        return(stats::friedman.test(formula=formula,
                                                    data=data,
                                                    subset=subset,
                                                    na.action=na.action,
                                                    ...))
                    }
                    data <- setAlias(data,"")
                    connection <- getFLConnection()
                    if(data@isDeep){
                        vBlockColname <- getVariables(data)[["obs_id_colname"]]
                        vGroupColname <- getVariables(data)[["var_id_colname"]]
                        vValueColname <- getVariables(data)[["cell_val_colname"]]
                    }
                    else{
                        vallVars <- all.vars(formula)
                        if(any(!vallVars %in% colnames(data)))
                            stop("columns specified in formula not in data \n ")
                        vBlockColname <- vallVars[3]
                        vGroupColname <- vallVars[2]
                        vValueColname <- vallVars[1]
                    }
                    vdata.name <- list(...)[["data.name"]]
                    if(is.null(vdata.name))
                        vdata.name <- paste0(vValueColname,", ",vGroupColname,
                                            " and ",vBlockColname)
                    vobsIDCol <- getVariables(data)[["obs_id_colname"]]

                    # vgroupCols <- unique(c(vobsIDCol,list(...)[["GroupBy"]]))
                    vgroupCols <- unique(c(getVariables(data)[["group_id_colname"]],
                                        list(...)[["GroupBy"]]))
                    if(is.wideFLTable(data) &&
                        any(!setdiff(vgroupCols,vobsIDCol) %in% colnames(data)))
                        stop("columns specified in GroupBy not in data \n ")
                    vgrp <- paste0(vgroupCols,collapse=",")
                    if(!length(vgroupCols)>0)
                        vgrp <- NULL

                    ret <- sqlStoredProc(connection,
                                         "FLFriedmanTest",
                                         TableName = getTableNameSlot(data),
                                         ValueColname = vValueColname,
                                         ObsIDColName= vBlockColname,
                                         SampleIDColName = vGroupColname,
                                         WhereClause = list(...)[["whereconditions"]],
                                         GroupBy = vgrp,
                                         TableOutput = 1,
                                         outputParameter = c(OutTable = 'a')
                                        )
                    ret <- as.character(ret[1,1])

                    VarID <- c(statistic="TEST_STAT",
                                p.value="Prob")
                    vdf <- sqlQuery(connection,
                                        paste0("SELECT COUNT(DISTINCT a.",
                                                    vGroupColname,")-1 AS df \n ",
                                               " FROM ",getTableNameSlot(data)," a \n ",
                                               constructWhere(list(...)[["whereconditions"]])," \n ",
                                               ifelse(length(setdiff(vgrp,""))>0,
                                                        paste0("GROUP BY ",vgrp, " \n "),""),
                                               ifelse(length(setdiff(vgrp,""))>0,
                                                        paste0("ORDER BY ",vgrp),"")
                                            )
                                    )
                    vdf <- vdf[[1]]
                    vres <- sqlQuery(connection,
                                    paste0("SELECT ",paste0(VarID,collapse=",")," \n ",
                                            "FROM ",ret," \n ",
                                            ifelse(length(setdiff(vgrp,""))>0,
                                                    paste0("ORDER BY ",vgrp),"")))

                    vres <- cbind(groupID=1:nrow(vres),vres)
                    colnames(vres) <- c("groupID",names(VarID))

                    vresList <- dlply(vres,"groupID",
                                    function(x){
                                        vtemp <- list(statistic=c("Friedman chi-squared"=x[["statistic"]]),
                                                      parameter=c(df=vdf[x[["groupID"]]]),
                                                      p.value=x[["p.value"]],
                                                      method="Friedman rank sum test",
                                                      data.name=vdata.name
                                                      )
                                        class(vtemp) <- "htest"
                                        return(vtemp)
                                    })
                    names(vresList) <- 1:length(vresList)
                    if(length(vresList)==1)
                        vresList <- vresList[[1]]
                    vtemp <- dropView(getTableNameSlot(data))
                    return(vresList)
    })

# setMethod("friedman.test",
#         signature(formula="formula", 
#                   data="FLTableMD"),
#         function(formula, data,
#                 subset=TRUE, 
#                 na.action=getOption("na.action"),
#                 ...){
#                     class(data) <- "FLTable"
#                     return(friedman.test(formula=formula,
#                                         data=data,
#                                         subset=subset,
#                                         na.action=na.action,
#                                         ...))
#                     })
