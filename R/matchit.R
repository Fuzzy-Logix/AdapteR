
##' Perform matchit algorithm in database.
##'
##' The function returns a FLSimpleVector discarded 
##'
##' 
##' @seealso \code{\link[MatchIt]{matchit}} for R reference
##'     implementation.
##' @param formula This argument takes the usual syntax of R formula,
##'     ‘treat ~ x1 + x2’, where ‘treat’ is a binary treatment
##'     indicator and ‘x1’ and ‘x2’ are the pre-treatment covariates.
##'     Both the treatment indicator and pre-treatment covariates must
##'     be contained in the same data frame, which is specified as
##'     ‘data’ (see below). No operators in formulas are currently
##'     supported in AdapteR.
##' @param data This argument specifies the data frame containing the
##'     variables called in ‘formula’. Either a FLTable or prepared
##'     data in a deep table.
##' @param method (for API compatibility with MatchIt, must be
##'     nearest)
##' @param distance (for API compatibility with MatchIt, must be
##'     logit)
##' @param distance.options (for API compatibility with MatchIt,
##'     ignored)
##' @param discard (for API compatibility with MatchIt, must be none)
##' @param reestimate (for API compatibility with MatchIt, not
##'     supported)
##' @param ...
##' @export
matchit <- function(formula, data, method = "nearest", distance = "logit",
                    distance.options = list(), discard = "none",
                    reestimate = FALSE,...){
    if(reestimate) stop("reestimate not supported.")
    if(discard!="none") stop("discard not supported.")
    if(distance!="logit") stop("only logit distance supported.")
    if(method!="nearest") stop("only nearest neighbor matching supported.")
    if(length(distance.options)>0) warning("distance options ignored")
    TIME <- list()
    ## browser()
    connection <- getFLConnection(data)
    ##excludeCols=c(excludeCols)
    ## prepare data for glm
    TIME$dataprep <- system.time({
        deepD <- prepareData(formula, data=data, family="binomial", ...)
        ##deepD <- prepareData(TREATMENT ~ ., data=data, family="binomial", performVarReduc = 1, minStdDev=.05, maxCorrel=.8)
    })
    ##
    ## logistic regression training
    TIME$logregr <- system.time({
        fit <- glm(formula,data=deepD, family="binomial",excludeCols,...)
        ## fit <- glm(TREATMENT ~ ., data=deepD, family="binomial")
    })
    ##
    ## logistic regression scoring
    TIME$scoring <- system.time({
        scores <- predict(fit)
    })
    ##
    ## create a table for Matchit
    ## browser()
    e <- gen_unique_table_name("matchit")
    Y <- setAlias(data,"a")
    sql <- paste0("
SELECT ", getIndexSQLExpression(Y,1)," obsid,
       a.TREATMENT exposure,
       b.vectorValueColumn prob
FROM (",constructSelect(Y),") a,
     (",constructSelect(scores),") b
WHERE a.obsid=b.vectorIndexColumn
")
    createTable(pTableName=e, pSelect=sql,
                pPrimaryKey="obsid",pWithData = TRUE)
    ## obsid <- getIndexSQLExpression(Y,1)
    ## sel@select@table_name <- c(sel@select@table_name,getTableNameSlot(Y))
    ## sel@select@variables <- c(sel@select@variables,
    ##                           exposure=paste0("respTab.",all.vars(update(fit@formula, .~0))))
    ## where(sel) <- c(where(sel),paste0("predTab.obsid = ",obsid))
    ## sel@select@order <- ""
    ## cat(constructSelect(sel))
    ## createTable(pTableName=e, pSelect=constructSelect(sel),
    ##             pPrimaryKey=getIndexSQLName(sel,1),pWithData = TRUE)

    TIME$matchit <- system.time({
        ret <- sqlStoredProc(connection,
                             "FLMatchIt",
                             TableName = e,
                             ObsIDColName = "obsid", ##getIndexSQLName(sel,1), 
                             TreatmentColName = "exposure",
                             PropScoreCol = "prob", ## getValueSQLName(sel),
                             MatchOrderCol = "prob", ## getValueSQLName(sel), 
                             TableOutput = 1,
                             outputParameter = c(OutTable = 'a')
                             )
    })
    discarded <- FLSimpleVector(as.character(ret$OutTable),"obsid","obsid")
    whereClause <- function(fordat=data){
        unsel <- setAlias(discarded, "adpaterSel")
        unsel@select@order <- ""
        where(unsel) <- c(where(unsel),
                          paste0(getValueSQLExpression(unsel)," = ",getIndexSQLExpression(fordat,1)))
        paste0(" NOT EXISTS (",
               constructSelect(unsel),")")
    }
    environment(whereClause)$data <- data
    environment(whereClause)$discarded <- discarded
    structure(list(
        model=fit,
        propensities=scores,
        formula=formula,
        treat=FLSimpleVector(e,"obsid","exposure"), # getIndexSQLName(sel,1),"exposure"),
        discarded=discarded, ##getIndexSQLName(sel,1),getIndexSQLName(sel,1))
        whereClause=whereClause
    ),
    timing=TIME,
    class="FLmatchit")
}


#' Select obsids in a larger table with fast EXISTS SQL sub-queries.
#'
#' @export
#' @param table an FLTable or FLTableDeep
#' @param select a FLSimpleVector containgin all obsids
#' @param exclude a flag whether to constrict to the obsids in select or to exclude the ids in select
restrictToObsids <- function(table, select,exclude=FALSE){
    unsel <- setAlias(select, "adpaterSel")
    unsel@select@order <- ""
    subdat <- table
    obsid <- getIndexSQLExpression(subdat,1)
    exclid <- getValueSQLExpression(unsel)
    where(unsel) <- c(where(unsel),
                      paste0(exclid," = ",obsid))
    where(subdat) <- c(where(subdat),
                       paste0(ifelse(exclude,"NOT",""),
                              " EXISTS (",constructSelect(unsel),")"))
    subdat
}

