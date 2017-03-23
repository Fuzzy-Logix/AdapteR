## Reading material.
## http://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html
## http://www.bodowinter.com/tutorial/bw_LME_tutorial1.pdf
##http://www.bodowinter.com/tutorial/bw_LME_tutorial2.pdf 

lmer <- function (formula,data=list(),...) {
	UseMethod("lmer", data)
}

#' @export
lmer.default <- function (formula,data=list(),...) {
   if (!requireNamespace("lme4", quietly = TRUE)){
       stop("lme4 package needed for mixed models. Please install it.",
            call. = FALSE)
   }
   else
       return(lme4::lmer(formula,data=data,...))
}

#' @export
setClass(
    "FLMix",
    slots = list(formula = "formula",
                 scoreTable = "character",
                 results = "list",
                 table = "FLTable"
                 ) )

## One Random Effect.
#' fltbl  <- FLTable("tblMixedModel", "ObsID")
#' flmod <- lmer(yVal ~ (FixVal | RanVal), data = fltbl, pIntercept = 1)
## 2 Random Effects.
#' tbl  <- FLTable("tblMixedModelInt", "ObsID")
#' flmod <- lmer(yVal ~ (FixVal |   RanVal1) + (1 | RanVal2 ), tbl)
## TO-DO: summary, plot, coefficients.

#' @export
lmer.FLTable <- function(formula, data, fetchID = TRUE,...)
{
    vcallObject <- match.call()
    vform <- as.character(vcallObject)[2]
    vreg <- gsub(pattern = "[\\|\\)]", replacement = "", x = regmatches(vform, gregexpr("\\|.*?\\)", vform))[[1]])
    Rvar <- gsub("[[:space:]]", "", vreg)
    Dvar <- gsub(pattern = " ", replacement = "", x = gsub(x = vform, pattern = "~.*", replacement = ""))
    Dvar <- gsub("[[:space:]]", "", Dvar)
    Fvar <- gsub( pattern = ".*\\(",replacement = "", x = gsub("\\| .*",x = vform, replacement = "" ))
    Fvar <- gsub("[[:space:]]", "", Fvar)

    data <- setAlias(data,"")
    functionName <- "FLMixedModelUdt"
    cnames <- c(GroupID = 1,
                Depvar= Dvar,
                FixVal = Fvar,
                RanVal = Rvar )
    if(length(Rvar) == 2)
    {
        functionName <- "FLMixedModelIntUdt"
        cnames <- c(GroupID = 1,
                Depvar= Dvar,
                FixVal = Fvar,
                RanVal1 = Rvar[1],
                RanVal2 = Rvar[2]
                )
    }
    if(is.null(list(...)$pIntercept))
    {vArg <- 1}
    else
        vArg <- list(...)$pIntercept
        
    tblname <- gen_unique_table_name("mixlin")
    t <- createTable(tblname, pSelect =  constructUDTSQL(pViewColnames = cnames,
                                                         pFuncName = functionName,
                                                         pOutColnames = c("a.*"),
                                                         pSelect = data@select@table_name[[1]],
                                                         pArgs = vArg,
                                                         pLocalOrderBy=c("GroupID"), pNest = TRUE, pFromTableFlag = TRUE))

    return(new("FLMix",
               formula=formula,
               table=data,
               results=list(call=vcallObject,
                            outtbl=tblname,
                            pArgs = list(Dvar=Dvar, Fvar = Fvar, Rvar = Rvar )),
               scoreTable=""
               ))   
}



#' @export
`$.FLMix`<-function(object,property){
                                        #parentObject <- deparse(substitute(object))
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

    if(property == "AIC"){
        quer <- paste0("SELECT  DISTINCT(AIC) FROM ",object@results$outtbl," ")
        df <- sqlQuery(connection,quer )
        return(df$AIC) }


    if(property == "logLik"){
        quer <- paste0("SELECT  DISTINCT(LogLikeliHood) FROM ",object@results$outtbl," ")
        df <- sqlQuery(connection,quer )
        return(df$LogLik) }

    if(property == "CovErr"){
        quer <- paste0("SELECT  DISTINCT(CovErr) FROM ",object@results$outtbl," ")
        df <- sqlQuery(connection,quer )
        return(df$CovErr)}

    if(property == "CovRandom"){
        quer <- paste0("SELECT  DISTINCT(CovRandom) FROM ",object@results$outtbl," ")
        df <- sqlQuery(connection,quer )
        return(df$CovRandom) }
    if(property == "u"){
        quer <- paste0("SELECT CoeffEst, CoeffID FROM ",object@results$outtbl," WHERE coeffName LIKE 'RANDOM%' ORDER BY coeffID ")
        df <- sqlQuery(connection, quer)
        return(df$CoeffEst)}
}



#' @export
predict.FLMix <- function(object,
                          newdata = object@table,
                          scoreTable = "")
{
    scoretbl <- gen_unique_table_name("mixedscore")
    vinputcols <- list(CoeffTable  = object@results$outtbl,
                         InTable = newdata@select@table_name,
                         GroupIDCol = NULL,
                         ObsIDCol =newdata@select@variables$obs_id_colname ,
                       FixVarCol = object@results$pArgs$Fvar)
    
    rnames <- c()
    functionName <- "FLMixedModelUdtScore"

    if(length(object@results$pArgs$Rvar) == 2)
    {
        functionName <- "FLMixedModelIntUdtScore"
        vinputcols <- c(vinputcols,
                        RanVar1Col = object@results$pArgs$Rvar[1],
                        RanVar2Col = object@results$pArgs$Rvar[2],
                        ScoreTable = scoretbl
                        ) }
    else
        vinputcols <- c(vinputcols,
                        RanVarCol = object@results$pArgs$Rvar,
                        ScoreTable = scoretbl)
    
    ret <- sqlStoredProc(connection,
                         functionName,
                         pInputParams <- vinputcols,
                         outputParameter = c(OutTable = 'a')
                         )


    
    val <- new("FLSimpleVector",
               select = new("FLSelectFrom",
                            table_name = scoretbl,
                            connectionName = getFLConnectionName(),
                            variables = list(ObsID = object@table@select@variables$obs_id_colname,
                                             pred = "PredictedVal"),
                            whereconditions = "",
                            order = ""),
               dimColumns = c("ObsID", "pred"),
               Dimnames = list(row = newdata@Dimnames[[1]]),
               dims = as.integer(nrow(newdata)),
               type = "integer"
               )
    return(val)
    object@results <- c(object@results,list(pred = val))
    assign(parentObject,object,envir=parent.frame())}


#' @export
AIC.FLMix <- function(object, ...){
    return(object$AIC)}


#' @export
logLik.FLMix <- function(object, ...){
    return(object$logLik)}

#' @export
residuals.FLMix <- function(object,newdata = object@table, ...){
    flpred <- predict(object)
    tbl <- object@table@select@table_name
    vob <- object@table@select@variables$obs_id_colname
    str <- paste0("SELECT (a.pred -b.",object@results$pArgs$Dvar,") AS res , a.",vob," AS ObsID FROM (",constructSelect(flpred),") AS a , ",tbl," AS b WHERE a.",vob," = b.",vob," ")

    tblfunqueryobj <- new("FLTableFunctionQuery",
                          connectionName = getFLConnectionName(),
                          variables = list(
                              obs_id_colname = "ObsID",
                              cell_val_colname = "res"),
                          whereconditions="",
                          order = "",
                          SQLquery=str)
    val <- new("FLSimpleVector",
               select = tblfunqueryobj,
               dimColumns = c("ObsID", "res"),
               Dimnames = list(row = newdata@Dimnames[[1]]),
               dims = as.integer(nrow(newdata)),
               type = "integer"
               )
    return(val)}
