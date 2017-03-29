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
#' flmod <- lmer(yVal ~ FixVal + (1 | RanVal), data = fltbl, pIntercept = 1)
#' flpred <- predict(flmod)
## 2 Random Effects.
#' tbl  <- FLTable("tblMixedModelInt", "ObsID")
#' flmod <- lmer(yVal ~ FixVal + (1 |   RanVal1) + (1 | RanVal2 ), tbl)
#' flpred <- predict(flmod)
## TODO :- implement storage in results slot.


#' @export
lmer.FLTable <- function(formula, data, fetchID = TRUE,...)
{
    vcallObject <- match.call()
    vform <- as.character(vcallObject)[2]
    vreg <- gsub(pattern = "[\\|\\)]", replacement = "", x = regmatches(vform, gregexpr("\\|.*?\\)", vform))[[1]])
    Rvar <- gsub("[[:space:]]", "", vreg)
    Dvar <- gsub(pattern = " ", replacement = "", x = gsub(x = vform, pattern = "~.*", replacement = ""))
    Dvar <- gsub("[[:space:]]", "", Dvar)
    Fvar <- gsub("[[:space:]~[:space:]+[:space:](]", replacement = "",regmatches(vform, gregexpr("\\~.*?\\(", vform)))
    
    data <- setAlias(data,"")
    vfun <- 0
    functionName <- "FLMixedModelUdt"
    cnames <- c(GroupID = 1,
                Depvar= Dvar,
                FixVal = Fvar,
                RanVal = Rvar )
    if(length(Rvar) == 2)
    {
        vfun <- 1
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

    vdf <- paste0("SELECT Count(*) as vobs, count(DISTINCT(",Rvar,")) as vrobs FROM ",data@select@table_name)
    vdf <- sqlQuery(connection, vdf)
    if(vArg == 1)
    { vin <- paste0("SELECT CoeffEst as coeff, TStat  FROM ",tblname," WHERE CoeffName LIKE '%INTERCEPT%'") 
        vin <- sqlQuery(connection, vin)}
    else
        vin <- NULL

    return(new("FLMix",
               formula=formula,
               table=data,
               results=list(call=vcallObject,
                            outtbl=tblname,
                            pArgs = list(Dvar=Dvar, Fvar = Fvar, Rvar = Rvar ),
                            vdf = vdf,
                            vin  = vin,
                            vfun = vfun),
               scoreTable=""
               ))   
}



#' @export
`$.FLMix`<-function(object,property){
                                        #parentObject <- deparse(substitute(object))
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

    if(property == "AIC"){
        quer <- paste0("SELECT  TOP 1 AIC FROM ",object@results$outtbl," ")
        df <- sqlQuery(connection,quer )
        return(df$AIC) }


    if(property == "logLik"){
        quer <- paste0("SELECT  TOP 1 LogLikeliHood FROM ",object@results$outtbl," ")
        df <- sqlQuery(connection,quer )
        return(df$LogLik) }

    if(property == "CovErr"){
        quer <- paste0("SELECT  TOP 1 CovErr FROM ",object@results$outtbl," ")
        df <- sqlQuery(connection,quer )
        return(df$CovErr)}

    if(property == "CovRandom"){
        if(!object@results$vfun[1]){
            quer <- paste0("SELECT  TOP 1 CovRandom FROM ",object@results$outtbl," ")
            df <- sqlQuery(connection,quer )
            return(df$CovRandom)}
        else
        {    quer <- paste0("SELECT  TOP 1 CovRandom1,CovRandom2 FROM ",object@results$outtbl," ")
            df <- sqlQuery(connection,quer )
            return(c(df$CovRandom1,df$CovRandom2)) } }

    if(property == "u"){
        quer <- paste0("SELECT CoeffEst, CoeffID FROM ",object@results$outtbl," WHERE coeffName LIKE 'RANDOM%' ORDER BY coeffID ")
        df <- sqlQuery(connection, quer)
        return(df$CoeffEst)}

    if(property == "fixedcoef"){
        quer <- paste0("SELECT CoeffEst, CoeffID FROM ",object@results$outtbl," WHERE coeffName LIKE 'FIXED%' ORDER BY coeffID ")
        df <- sqlQuery(connection, quer)
        return(df$CoeffEst)
    }   
}



#' @export
predict.FLMix <- function(object,
                          newdata = object@table,
                          scoreTable = "")
{
    parentObject <- unlist(strsplit(unlist(strsplit(
		as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
    scoretbl <- gen_unique_table_name("mixedscore")
    vinputcols <- list(CoeffTable  = object@results$outtbl,
                       InTable = newdata@select@table_name,
                       GroupIDCol = NULL,
                       ObsIDCol =newdata@select@variables$obs_id_colname ,
                       FixVarCol = object@results$pArgs$Fvar)
    
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
    object@results <- c(object@results,list(pred = val))
    assign(parentObject,object,envir=parent.frame())
    return(val)
    }


#' @export
AIC.FLMix <- function(object, ...){
    return(object$AIC)}


#' @export
logLik.FLMix <- function(object, ...){
    return(object$logLik)}

#' @export
residuals.FLMix <- function(object,newdata = object@table, ...){
    parentObject <- unlist(strsplit(unlist(strsplit(
        as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
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
    object@results <- c(object@results,list(res = val))
    assign(parentObject,object,envir=parent.frame())

    return(val)
}




#' @export
plot.FLMix <- function(object,limit = 1000, ...){
    vres <- residuals(object)
    vpred <- predict(object)
    p <- min(limit,vpred@dims)/(vpred@dims)
    
 str1 <- paste0("SELECT  b.pred AS pred, a.res AS res FROM (",constructSelect(vpred),") AS b, (",constructSelect(vres),") AS a WHERE a.ObSID = b.ObsID AND FLSimUniform(RANDOM(1,10000), 0.0, 1.0) < ",p," ")
   df <- sqlQuery(connection, str1)
    return(plot(df$pred, df$res))}

 


## implement for 2 variable
#' @export
print.FLMix <- function(object, ...)
{
    cat("Linear mixed model fit by Expectation Maximization method \n\n")
    cat("Formula: ")
    print(object@results$call[[2]])
    cat("Data: \n")
    val <- c("AIC" = object$AIC, "logLik" = object$logLik, "CovErr" = object$CovErr, "df.residual" = (object@results$vdf$vobs -1))
    print.default(val, digits = 3, print.gap = 2L, quote = FALSE)
    cat("Random Effects: \n")
    if(!object@results$vfun[1])    {val <- c("Groups" = object@results$pArgs$Rvar , "Name" = "(Intercept)", "Std.Dev" = object$CovRandom^.5)
        print.default(val, digits = 3, print.gap = 2L, quote = FALSE)
        print(cat("Number of obs: ",object@results$vdf$vobs,", groups: ", object@results$pArgs$Rvar, ", ",object@results$vdf$vrobs))
    }  
    else
    {
        cat("Groups", "   Name", "   Std. Dev. \n")
        cat(flmod@results$pArgs$Rvar[1]," (Intercept)", flmod$CovRandom[1],"\n")
        cat(flmod@results$pArgs$Rvar[2]," (Intercept)", flmod$CovRandom[2], "\n")
        cat("Number of obs: ",object@results$vdf[[1]]$vobs,", ",
            object@results$pArgs$Rvar[1],object@results$vdf[[1]]$vrobs,", "
            ,object@results$pArgs$Rvar[2],object@results$vdf[[2]]$vrobs, "\n\n")}
    cat("Fixed Effects: \n")
    val <- c("(Intercept)" = object@results$vin$coeff, "TStat" = object@results$vin$TStat)
    print.default(val, digits = 3, print.gap = 2L, quote = FALSE) }



#' @export
setMethod("show","FLMix",function(object){print.FLMix(object)})


#' @export
summary <- function(object, ...){
    return(print(object))
}
