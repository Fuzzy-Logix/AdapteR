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
#' flmod <- lmer(yVal ~ FixVal + (1 | RanVal), data = fltbl)
#' flpred <- predict(flmod)

#' @export
lmer.FLTable <- function(formula, data, fetchID = TRUE,...)
{
    vcallObject <- match.call()
    vform <- as.character(vcallObject)[2]
    vreg <- gsub(pattern = "[\\|\\)]", replacement = "", x = regmatches(vform, gregexpr("\\|.*?\\)", vform))[[1]])
    Rvar <- gsub("[[:space:]]", "", vreg)
    Dvar <- gsub(pattern = " ", replacement = "", x = gsub(x = vform, pattern = "~.*", replacement = ""))
    Dvar <- gsub("[[:space:]]", "", Dvar)
    Fvar <- regmatches(vform, gregexpr("\\~.*?\\(", vform))
    Fvar <- gsub("[~.*?(]", "",Fvar)
    Fvar <- gsub("[[:space:]]", "", Fvar)
    Fvar <- strsplit(Fvar, split = "+", fixed = TRUE)
    Fvar <- unlist(Fvar)
    myformula <- as.formula(paste0(Dvar,"~ ",Fvar," + ",paste0(Rvar,collapse =  " + ")))
    deeptblname <- gen_unique_table_name("deepmixlin")
    vArgs <- c(Dvar, Fvar, unlist(Rvar))
    if(!isDeep(data))
        {
    FLdeep <- prepareData(formula         = myformula ,
                          data            = data,
                          outDeepTable    = deeptblname,
                          makeDataSparse  = 1,
                          performVarReduc = 0,
                          minStdDev       = .01,
                          maxCorrel       = .8,
                          fetchIDs        = FALSE)}
    vmap <- FLdeep$vmapping
    outtblname <- gen_unique_table_name("mixedtbl")
    
    data <- setAlias(data,"")
    functionName <- "FLLinMixedModel"
    vlen <- 2 + length(Rvar)
    vchr <- c("DEPENDENT", "FIXED", rep("RANDOM", length(Rvar)))
    vterm <- c(1,1,1:length(Rvar))
    vclass <- c(0,0, rep(1, length(Rvar)))

    ## use inserintotbl function
    var <- lapply(1:vlen, function(i){
        paste0("INSERT INTO fzzlLinMixedModelSpec VALUES (",fquote(outtblname)," , ",fquote(vchr[[i]]),", ",vterm[[i]],", ",vmap[[vArgs[[i]]]]," , ",vclass[[i]],");")})

    sqlSendUpdate(connection, var)
    vinputcols <- c(TableName = deeptblname ,
                    ObsIDCol = FLdeep$deepx@select@variables$obs_id_colname,
                    VarIDCol = FLdeep$deepx@select@variables$var_id_colname,
                    ValueCol = FLdeep$deepx@select@variables$cell_val_colname,
                    SpecID = outtblname,
                    MaxIter =100 ,
                    Note = "Mixed model for AdapteR")


    ret <- sqlStoredProc(connection,
                         functionName,
                         pInputParams = vinputcols,
                         outputParameter = c(OutTable = 'a')
                         )
    vAnalysisID <- ret[[1]]

    vin <-paste0("SELECT VarType,CoeffVal AS coeff, StdErr,TStat FROM fzzlLinMixedModelCoeffs WHERE AnalysisID = '",vAnalysisID,"' AND VarType LIKE ANY('%INTERCEPT%', '%FIXED%') ORDER BY VarType ;")

    vin <- sqlQuery(connection, vin)

    str <- paste0("SELECT ParamName, ParamVal FROM fzzlLinMixedModelStats WHERE AnalysisID = '",vAnalysisID,"' ORDER BY ParamName")
    vdf <- sqlQuery(connection, str)

    
    return(new("FLMix",
               formula=formula,
               table=data,
               results=list(call=vcallObject,
                            AnalysisID=vAnalysisID,
                            pArgs = list(Dvar=Dvar, Fvar = Fvar, Rvar = Rvar ), 
                            deeptbl = FLdeep$deepx,
                            vspec = outtblname,
                            vin = vin,
                            vdf = vdf,
                            scoreTable=""
                            )))   
}




#' @export
`$.FLMix`<-function(object,property){
                                        #parentObject <- deparse(substitute(object))
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

    if(property == "AIC"){
        df <- object@results$vdf
        return(df[df$ParamName == "AIC",]$ParamVal) }


    if(property == "logLik"){
        df <- object@results$vdf
        return(df[df$ParamName == "LogLikeliHood",]$ParamVal) }

    if(property == "CovarErr"){
        df <- object@results$vdf
        return(df[df$ParamName == "CovarErr",]$ParamVal) }

    if(property == "CovarRandom"){
        df <- object@results$vdf
        return(df[df$ParamName == "CovarRan",]$ParamVal)
        }

    if(property == "u"){
        str <- paste0("SELECT CoeffVal, ClassVal FROM fzzlLinMixedModelCoeffs WHERE AnalysisID = '",object@results$AnalysisID,"' AND VarType LIKE '%RANDOM%' ORDER BY ClassVal")
        df <- sqlQuery(connection, str)
        return(df$CoeffVal)}

    if(property == "fixedcoef"){
        str <- paste0("SELECT CoeffVal, ClassVal FROM fzzlLinMixedModelCoeffs WHERE AnalysisID = '",object@results$AnalysisID,"' AND VarType LIKE '%FIXED%' ORDER BY ClassVal")
        df <- sqlQuery(connection, str)
        return(df$CoeffVal)
    }   
}

setMethod("names", signature("FLMix"), function(object) {c("AIC","logLik",
                                                          "CovarErr","CovarRandom",
                                                          "u",
                                                          "fixedcoef" )})



#' @export
predict.FLMix <- function(object,
                          newdata = object@results$deeptbl,
                          scoreTable = "")
{
    parentObject <- unlist(strsplit(unlist(strsplit(
		as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
    scoretbl <- gen_unique_table_name("mixedscore")
    vinputcols <- list(InTable = newdata@select@table_name,
                       ObsIDCol = newdata@select@variables$obs_id_colname,
                       VarIDCol = newdata@select@variables$var_id_colname,
                       ValueCol = newdata@select@variables$cell_val_colname,
                       SpecID = object@results$vspec,                       
                       pAnalysisID = as.character(object@results$AnalysisID),
                       ScoreTable = scoretbl)
    
    functionName <- "FLLinMixedModelScore"

    ret <- sqlStoredProc(connection,
                         functionName,
                         pInputParams = vinputcols,
                         outputParameter = c(OutTable = 'a')
                         )


    
    val <- new("FLSimpleVector",
               select = new("FLSelectFrom",
                            table_name = scoretbl,
                            connectionName = getFLConnectionName(),
                            variables = list(ObsID = object@table@select@variables$obs_id_colname,
                                             pred = "PredVal"),
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
residuals.FLMix <- function(object,newdata = object@results$deeptbl, ...){
    parentObject <- unlist(strsplit(unlist(strsplit(
        as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
    flpred <- predict(object)
    tbl <- newdata@select@table_name
    vob <- newdata@select@variables$obs_id_colname
    str <- paste0("SELECT (a.pred -b.",object@results$pArgs$Dvar,") AS res , a.",vob," AS ObsID FROM (",constructSelect(flpred),") AS a , ",object@table@select@table_name," AS b WHERE a.",vob," = b.",vob," ")

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


#' @export
print.FLMix <- function(object, ...)
{
    cat("Linear mixed model fit by Expectation Maximization method \n\n")
    cat("Formula: ")
    print(object@results$call[[2]])
    cat("Data: \n")
    val <- c("AIC" = object$AIC, "logLik" = object$logLik, "CovErr" = object$CovarErr, "df.residual" = (object@results$vdf$vobs -1))
    print.default(val, digits = 3, print.gap = 2L, quote = FALSE)

    cat("Random Effects: \n")
    val <- c("Groups" = object@results$pArgs$Rvar , "Name" = "(Intercept)", "Std.Dev" = object$CovarRandom^.5)
    print.default(val, digits = 3, print.gap = 2L, quote = FALSE)
    cat("         Residual           ", "  ", object$CovarErr^.5, "          \n\n")
    cat("Number of obs: ",object@results$vdf[7,2],", groups: ", object@results$pArgs$Rvar, ", ",object@results$vdf[8,2],"\n")
    
    cat("Fixed Effects: \n")
    val <- c("(Intercept)" = object@results$vin[2, ]$coeff, "TStat" = object@results$vin[2, ]$TStat)
    print.default(val, digits = 3, print.gap = 2L, quote = FALSE)
    val <- c("FIXED" = object@results$vin[1, ]$coeff, "TStat" = object@results$vin[1, ]$TStat)
    print.default(val, digits = 3, print.gap = 2L, quote = FALSE)}

#' @export
setMethod("show","FLMix",function(object){print.FLMix(object)})


#' @export
summary <- function(object, ...){
    return(print(object))
}
