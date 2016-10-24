
NULL

#' Kruskal-Wallis Rank Sum Test
#'
#' Performs a Kruskal-Wallis rank sum test.
#'
#' @param x FLVector with data values
#' @param g FLVector giving the group for the 
#' corresponding elements of y
#' @param formula a formula of the form response ~ group 
#' where response gives the data values and group a vector or factor 
#' of the corresponding groups.
#' Not applicable if FLVector is input.
#' @param data FLTable or FLTableMD objects.
#' @param subset Not currently used.
#' @param na.action na values omitted always.
#' @param ... The additional arguments used by FL function:
#' \code{whereconditions} WhereConditions to subset data
#' \code{GroupBy} Column names defining the different groups in data, if any.
#' @return A list with class \code{htest}.
#' A list of \code{htest} objects if the input is a FLTableMD object.
#' @examples
#' x <- c(2.9, 3.0, 2.5, 2.6, 3.2) # normal subjects
#' y <- c(3.8, 2.7, 4.0, 2.4)      # with obstructive airway disease
#' z <- c(2.8, 3.4, 3.7, 2.2, 2.0) # with asbestosis
#' x <- c(x, y, z)
#' g <- factor(rep(1:3, c(5, 4, 5)),
#'             labels = c("Normal subjects",
#'                        "Subjects with obstructive airway disease",
#'                        "Subjects with asbestosis"))
#' x <- as.FLVector(x)
#' g <- as.FLVector(g)
#' result1 <- kruskal.test(x, g)
#' print(result1)
#' FLTableObj <- as.FLTable(airquality,tableName="ARBaseTestTempTable",drop=TRUE)
#' result2 <- kruskal.test(Ozone ~ Month, data = FLTableObj)
#' print(result2)
#' @export
#' @method kruskal.test FLVector
kruskal.test.FLVector <- function(x,g,...){
    if(!is.FLVector(g) && is.numeric(g))
        g <- as.FLVector(g)
    if(!is.FLVector(g))
        stop("g should be FLVector \n ")
    if(length(x)!=length(g))
        stop("x,g should have same length \n ")

    DNAME <- paste0(deparse(substitute(x))," and ",
                    deparse(substitute(g)))
    vView <- gen_view_name("Kruskal")
    vSelect <- constructUnionSQL(pFrom=c(a=constructSelect(x),
                                        b=constructSelect(g)),
                                 pSelect=list(a=c(DatasetID=1,
                                                ObsID="a.vectorIndexColumn",
                                                groupID=1,
                                                Num_Val="a.vectorValueColumn"),
                                              b=c(DatasetID=1,
                                                ObsID="b.vectorIndexColumn",
                                                groupID=2,
                                                Num_Val="b.vectorValueColumn")))

    vtemp <- createView(vView,pSelect=vSelect)

    vtable <- FLTableMD(vView,
                        group_id_colname="DatasetID",
                        obs_id_colname="ObsID")
    return(kruskal.test(data=vtable,
                        formula=Num_Val~groupID,
                        data.name=DNAME,
                        ...))
}


#' @export
#' @method kruskal.test formula
kruskal.test.formula <- function(formula, data,
                                  ...){
    if(!is.FL(data)){
        return(stats:::kruskal.test.formula(formula=formula,
                                            data=data,...))
    } else
        UseMethod("kruskal.test", data)
}

#' @export
#' @method kruskal.test FLTable
kruskal.test.FLTable <- function(formula,data,
                                 subset=TRUE, 
                                 na.action=getOption("na.action"),
                                 x=NULL,
                                 ...){
                    data <- setAlias(data,"")
                    connection <- getFLConnection()
                    if(data@isDeep){
                        vSampleIDColname <- getVariables(data)[["var_id_colname"]]
                        vValueColname <- getVariables(data)[["cell_val_colname"]]
                    }
                    else{
                        vallVars <- all.vars(formula)
                        if(any(!vallVars %in% colnames(data)))
                            stop("columns specified in formula not in data \n ")
                        vSampleIDColname <- vallVars[2]
                        vValueColname <- vallVars[1]
                    }
                    vdata.name <- list(...)[["data.name"]]
                    if(is.null(vdata.name))
                        vdata.name <- paste0(vValueColname," by ",vSampleIDColname)
                    vobsIDCol <- getVariables(data)[["obs_id_colname"]]

                    vWhereCond <- NULL
                    if(is.numeric(subset)){
                        vWhereCond <- paste0(vobsIDCol," IN (",
                                            paste0(subset,collapse=","),") ")
                    }
                    if(is.FLVector(subset)){
                        vWhereCond <- paste0(vobsIDCol," IN( SELECT a.vectorValueColumn ",
                                            "FROM (",gsub("\n"," ",
                                                        gsub("'%insertIDhere%'",1,
                                                            constructSelect(subset))),") a ) ")
                    }
                    vWhereCond <- c(vWhereCond,list(...)[["whereconditions"]])
    
                    # vgroupCols <- unique(c(vobsIDCol,list(...)[["GroupBy"]]))
                    vgroupCols <- unique(c(getVariables(data)[["group_id_colname"]],
                                        list(...)[["GroupBy"]]))
                    if(is.wideFLTable(data) &&
                        any(!setdiff(vgroupCols,vobsIDCol) %in% colnames(data)))
                        stop("columns specified in GroupBy not in data \n ")
                    vgrp <- paste0(vgroupCols,collapse=",")
                    if(!length(vgroupCols)>0)
                        vgrp <- NULL

                    ##browser()
                    ret <- sqlStoredProc(connection,
                                         "FLKWTest",
                                         TableName = getTableNameSlot(data),
                                         ValueColname = vValueColname,
                                         SampleIDColName = vSampleIDColname,
                                         WhereClause = constructWhere(vWhereCond),
                                         GroupBy = vgrp,
                                         TableOutput = 1,
                                         outputParameter = c(ResultTable = 'a')
                                        )
                    ret <- as.character(ret[1,1])

                    VarID <- c(statistic="TEST_STAT",
                                p.value="P_VALUE")
                    vdf <- sqlQuery(connection,
                                        paste0("SELECT COUNT(DISTINCT a.",
                                                    vSampleIDColname,")-1 AS df \n ",
                                               " FROM ",getTableNameSlot(data)," a \n ",
                                              constructWhere(vWhereCond)," \n ",
                                               ifelse(length(setdiff(vgrp,""))>0,
                                                        paste0("GROUP BY ",vgrp, " \n "),""),
                                               ifelse(length(setdiff(vgrp,""))>0,
                                                        paste0("ORDER BY ",vgrp),"")
                                            )
                                    )
                    vdf <- vdf[[1]]
                    ##browser()
                    vres <- sqlQuery(connection,
                                    paste0("SELECT ",paste0(VarID,collapse=",")," \n ",
                                            "FROM ",ret," \n ",
                                            ifelse(length(setdiff(vgrp,""))>0,
                                                    paste0("ORDER BY ",vgrp),"")))

                    vres <- cbind(groupID=1:nrow(vres),vres)
                    colnames(vres) <- c("groupID",names(VarID))

                    vresList <- dlply(vres,"groupID",
                                    function(x){
                                        vtemp <- list(statistic=c("Kruskal-Wallis chi-squared"=x[["statistic"]]),
                                                      parameter=c(df=vdf[x[["groupID"]]]),
                                                      p.value=x[["p.value"]],
                                                      method="Kruskal-Wallis rank sum test",
                                                      data.name=vdata.name
                                                      )
                                        class(vtemp) <- "htest"
                                        return(vtemp)
                                    })
                    names(vresList) <- 1:length(vresList)
                    if(length(vresList)==1)
                        vresList <- vresList[[1]]
                    ##vtemp <- dropView(getTableNameSlot(data))
                    return(vresList)
}
