#' @include utilities.R
#' @include data_prep.R
#' @include FLTable.R
#' @include FLLinRegr.R
NULL

#' An S4 class to represent output from coxph on in-database Objects
#'
#' @slot timeValCol column name representing time variable
#' @slot statusCol column name representing Status variable
#' @slot vfcalls information about system tables
#' and in-database procedures called during execution
#' @method print FLCoxPH
#' @method coefficients FLCoxPH
#' @method residuals FLCoxPH
#' @method plot FLCoxPH
#' @method summary FLCoxPH
#' @method predict FLCoxPH
#' @export
setClass(
	"FLCoxPH",
	contains="FLRegr",
	slots=list(#modelID="numeric",
				timeValCol="character",
				statusCol="character",
                vfcalls="character"))

#' Cox Proportional Hazard Model
#' 
#' \code{coxph} Fits a Cox proportional hazards regression model.
#' 
#' @param formula A symbolic description of model to fit
#' @param data FLTable object. Can be wide or deep
#' @param catToDummy Transform categorical variables to numerical values
#' either using dummy variables or by using Empirical
#' Logit. If the value is 1, transformation is done using
#' dummy variables, else if the value is 0,
#' transformation is done using Empirical Logit.
#' @param performNorm 0/1 indicating whether to perform standardization of data.
#' @param performVarReduc 0/1. If the value is 1,
#' the stored procedure eliminates variables based on standard deviation and
#' correlation.
#' @param makeDataSparse If 0,Retains zeroes and NULL values
#' from the input table. If 1, Removes zeroes and NULL. If 2,Removes zeroes 
#' but retains NULL values.
#' @param minStdDev Minimum acceptable standard deviation for
#' elimination of variables. Any variable that has a
#' standard deviation below this threshold is
#' eliminated. This parameter is only consequential if
#' the parameter PerformVarReduc = 1. Must be >0.
#' @param maxCorrel Maximum acceptable absolute correlation between
#' a pair of columns for eliminating variables. If the
#' absolute value of the correlation exceeds this
#' threshold, one of the columns is not transformed.
#' Again, this parameter is only consequential if the
#' parameter PerformVarReduc = 1. Must be >0 and <=1.
#' @param classSpec list describing the categorical dummy variables.
#' @param whereconditions takes the where_clause as a string.
#' @section Constraints:
#' The formula object should have a \code{Surv} object.
#' The arguments to \code{Surv} object must strictly be in the order
#' (\code{time},\code{time2},\code{event}) or (\code{time},\code{event}).
#' Arguments to \code{Surv} should be plain. For instance, \code{as.numeric(SurvColumn)}
#' inside \code{Surv} is not supported.
#' Only \code{coefficients},\code{linear.predictors},\code{FLSurvivalData},
#' \code{FLCoxPHStats},\code{loglik},\code{wald.test},\code{n},\code{nevent},
#' \code{rscore},\code{call},\code{formula},\code{call},\code{model},\code{x},
#' \code{means},\code{terms} can be called on fitted object using $.
#' coefficients,plot,print,summary methods are available for fitted object.
#' @return \code{coxph} returns a \code{FLCoxPH} object
#' @examples
#' widetable  <- FLTable("siemenswideARDemoCoxPH", "ObsID")
#' fitT <- coxph(Surv(startDate,endDate,event)~meanTemp+age,widetable)
#' predData <- FLTable("preddatatoday","ObsID")
#' resultList <- predict(fitT,newdata=predData)
#' resultList[[1]]
#' resultList[[2]]
#' summary(fitT)
#' plot(fitT)
#' deeptable <- FLTable("siemensdeepARDemoCoxPH","obs_id_colname",
#'                      "var_id_colname","cell_val_colname")
#' fitT <- coxph("",deeptable)
#' fitT$coefficients
#' summary(fitT)
#' plot(fitT)
## Failed due to numeric overflow in FLCoxPH
#fitT <- coxph(Surv(startDate,endDate,event)~meanTemp+age+lage,widetable)
# fitT <- coxph(Surv(startDate,endDate,event)~meanTemp,widetable)
# fitT <- coxph(Surv(age,event)~meanTemp,widetable)
#' @export
coxph <- function (formula,data=list(),...) {
	UseMethod("coxph", data)
 }

#' @export
coxph.default <- function(formula,data=list(),...){
    if (!requireNamespace("survival", quietly = TRUE)){
            stop("survival package needed for coxph. Please install it.",
            call. = FALSE)
        }
    else return(survival::coxph(formula=formula,
                                data=data,
                                ...))
}


#' @export
Surv <- survival::Surv

#' @export
coxph.FLTable <- function(formula,data, ...)
{
	if("maxiter" %in% names(list(...)))
	maxiter <- list(...)$maxiter
	else maxiter <- 15

	data <- setAlias(data,"")
    deep <- prepareData.coxph(formula,data,...)
    wideToDeepAnalysisId <- deep$wideToDeepAnalysisId
    deepx <- deep[["deeptable"]]
    
	deeptable <- deepx@select@table_name

	retobj <- sqlStoredProc(getFLConnection(),
							"FLCoxPH",
							outputParameter=c(AnalysisID="a"),
							INPUT_TABLE=deeptable,
							OBSID_COL=getVariables(deepx)[["obs_id_colname"]],
							VARID_COL=getVariables(deepx)[["var_id_colname"]],
							VALUE_COL=getVariables(deepx)[["cell_val_colname"]],
							MAX_ITER=maxiter,
							NOTE=genNote("coxph"))

 #    sqlstr <- paste0("CALL FLCoxPH(",fquote(deeptable),",\n",
	# 				 				fquote(getVariables(deepx)[["obs_id_colname"]]),",\n",
	# 				 				fquote(getVariables(deepx)[["var_id_colname"]]),",\n",
	# 				 				fquote(getVariables(deepx)[["cell_val_colname"]]),
	# 				 				",\n15,\n",fquote(genNote("coxph")),",\nAnalysisID );")
	
	# retobj <- sqlQuery(getFLConnection(),sqlstr,
 #                       AnalysisIDQuery=genAnalysisIDQuery("fzzlCoxPHInfo",genNote("coxph")))
	
	retobj <- checkSqlQueryOutput(retobj)
	AnalysisID <- as.character(retobj[1,1])
	
	vcallObject <- match.call()
    
	return(new("FLCoxPH",
				formula=deep[["formula"]],
				AnalysisID=AnalysisID,
				wideToDeepAnalysisId=wideToDeepAnalysisId,
				table=deep$vdata,
				results=list(call=vcallObject),
				deeptable=deepx,
				mapTable=deep$mapTable,
				scoreTable="",
				statusCol=deep$vStatus,
				timeValCol=deep$vTimeVal,
                RegrDataPrepSpecs=deep$RegrDataPrepSpecs))
}

prepareData.coxph <- function(formula,data,
                              catToDummy=0,
                              performNorm=0,
                              performVarReduc=0,
                              makeDataSparse=1,
                              minStdDev=0,
                              maxCorrel=1,
                              classSpec=list(),
                              whereconditions=""){
	vTimeVal <- "timeVal"
	vStatus <- "status"
	if(data@isDeep){
		vallVars <- colnames(data)
		formula <- genDeepFormula(vallVars)
	}
	if(!data@isDeep)
	{
        vtemp <- prepareSurvivalFormula(data=data,
                                        formula=formula)
        for(i in names(vtemp))
        assign(i,vtemp[[i]])
		# if(isDotFormula(formula))
		# 	formula <- genDeepFormula(pColnames=colnames(data),
		# 							pDepColumn=all.vars(formula)[1])
		# vallVars <- base::all.vars(formula)
		# checkValidFormula(formula,data)
		# vSurvival <- as.character(attr(terms(formula),"variables")[[2]])
		# if(!("Surv" %in% vSurvival))
		# stop("specify dependent variables as Surv object")
		# if(length(vSurvival)==2)
		# stop("atleast time and event components must be present in Surv object")
		# if(length(vSurvival)==3)
		# {
		# 	vTimeVal <- vSurvival[2]
		# 	vStatus <- vSurvival[3]
		# }
		# else if(length(vSurvival)==4)
		# {
		# 	vtempList <- IncludeTimeVal(data=data,
		# 								formula=formula)
		# 	vStatus <- vtempList[["vStatus"]]
		# 	vtablename <- vtempList[["vtablename"]]
		# 	vTimeVal <- vtempList[["vTimeVal"]]
		# 	data <- vtempList[["data"]]
		# 	vallVars <- vtempList[["vallVars"]]
		# 	vallVars <- c(vallVars,vTimeVal)
		# }
		# vallVars <- vallVars[vallVars!=vStatus]
	}
	
	vcolnames <- colnames(data)
	wideToDeepAnalysisId <- ""
    mapTable <- ""

    if(!data@isDeep){
    	unused_cols <- vcolnames[!vcolnames %in% vallVars]
		unused_cols <- unused_cols[unused_cols!=getVariables(data)[["obs_id_colname"]]]
		vexcludeCols <- paste0(unused_cols,collapse=",")
    }
	
	if(!data@isDeep)
	{
		deepx <- FLRegrDataPrep(data,depCol=vTimeVal,
								outDeepTableName="",
								outObsIDCol="",
								outVarIDCol="",
								outValueCol="",
								catToDummy=catToDummy,
								performNorm=performNorm,
								performVarReduc=performVarReduc,
								makeDataSparse=makeDataSparse,
								minStdDev=minStdDev,
								maxCorrel=maxCorrel,
								trainOrTest=0,
								excludeCols=vexcludeCols,
								classSpec=classSpec,
								whereconditions=whereconditions,
								inAnalysisID="")

        vRegrDataPrepSpecs <- list(outDeepTableName="",
                                outObsIDCol="",
                                outVarIDCol="",
                                outValueCol="",
                                catToDummy=catToDummy,
                                performNorm=performNorm,
                                performVarReduc=performVarReduc,
                                makeDataSparse=makeDataSparse,
                                minStdDev=minStdDev,
                                maxCorrel=maxCorrel,
                                trainOrTest=0,
                                excludeCols=vexcludeCols,
                                classSpec=classSpec)
		wideToDeepAnalysisId <- deepx[["AnalysisID"]]
		deepx <- deepx[["table"]]

		vtablename <- deepx@select@table_name
		vtablename1 <- data@select@table_name
		vobsid <- getVariables(data)[["obs_id_colname"]]
		sqlstr <- paste0("INSERT INTO ",vtablename,"\n        ",
						" SELECT ",vobsid," AS obs_id_colname,","\n               ",
						" -2 AS var_id_colname,","\n               ",
						vStatus," AS cell_val_colname","\n               ",
						" FROM ",vtablename1)
		t <- sqlSendUpdate(getFLConnection(),sqlstr)
		deepx@dimnames[[2]] <- c("-2",deepx@dimnames[[2]])
		whereconditions <- ""
		mapTable <- getRemoteTableName(tableName = "fzzlRegrDataPrepMap")
	}
	else if(class(data@select)=="FLTableFunctionQuery")
	{
		#sqlstr <- paste0("CREATE VIEW ",getOption("ResultDatabaseFL"),
		#					".",deeptablename," AS ",constructSelect(data))
		#sqlSendUpdate(connection,sqlstr)

		deeptablename <- createView(pViewName=gen_view_name(""),
                                    pSelect=constructSelect(data)
			)
		
		#sqlstr <- paste0("CREATE VIEW ",getOption("ResultDatabaseFL"),".",deeptablename1,
		#				" AS SELECT * FROM ",getOption("ResultDatabaseFL"),".",deeptablename,
		#				constructWhere(whereconditions))
		deeptablename1 <- createView(pViewName=gen_view_name("New"),
                                     pSelect=paste0("SELECT * FROM ",deeptablename,
                                                    constructWhere(whereconditions)))

		deepx <- FLTable(deeptablename1,
                   "obs_id_colname",
                   "var_id_colname",
                   "cell_val_colname"
                  )
		whereconditions <- ""
	}
	else
	{
		deepx <- data
		data@select@whereconditions <- c(data@select@whereconditions,whereconditions)
		if(length(data@select@whereconditions)>0 &&
			data@select@whereconditions!=""){
			
			#sqlstr <- paste0("CREATE VIEW ",getOption("ResultDatabaseFL"),".",
			#				deeptablename," AS ",constructSelect(data))
			#t <- sqlQuery(connection,sqlstr)
			
			deeptablename <- createView(pViewName=gen_view_name("New"),
                                        pSelect=constructSelect(data))

			deepx <- FLTable(
	                   table=deeptablename,
	                   obs_id_colname="obs_id_colname",
	                   var_id_colname="var_id_colname",
	                   cell_val_colname="cell_val_colname")
		}
		whereconditions <- ""
	}
	deepx <- setAlias(deepx,"")
    return(list(deeptable=deepx,
                wideToDeepAnalysisId=wideToDeepAnalysisId,
                formula=formula,
                mapTable=mapTable,
                vStatus=vStatus,
                vTimeVal=vTimeVal,
                vdata=data,
                RegrDataPrepSpecs=vRegrDataPrepSpecs))
}

#' @export
predict.FLCoxPH <-function(object,
							newdata=object@table,
							scoreTable="",
							survivalCurveTable="",
                            ...){
	if(!is.FLTable(newdata)) 
		stop("scoring allowed on FLTable only")
	#browser()
	newdata <- setAlias(newdata,"")
	vinputTable <- newdata@select@table_name

	if(scoreTable=="")
	# scoreTable <- getRemoteTableName(getOption("ResultDatabaseFL"),
	# 								gen_score_table_name(object@table@select@table_name))
	scoreTable <- gen_score_table_name(object@table@select@table_name)

	# if(!grepl(".",scoreTable)) scoreTable <- paste0(getOption("ResultDatabaseFL"),".",scoreTable)
	
	if(survivalCurveTable=="")
	# survivalCurveTable <- getRemoteTableName(getOption("ResultDatabaseFL"),
	# 										gen_score_table_name("survival"))
	survivalCurveTable <- gen_score_table_name("survival")
	# if(!grepl(".",survivalCurveTable)) survivalCurveTable <- paste0(getOption("ResultDatabaseFL"),".",survivalCurveTable)

	if(!newdata@isDeep)
	{
		vSurvival <- as.character(attr(terms(object@formula),"variables")[[2]])
		newdataCopy <- newdata
		vtablename <- newdataCopy@select@table_name
		vtablename2 <- object@table@select@table_name

		## SQL to Insert the dependent column ans statusColumn
		vVaridVec <- c(-2)
		vCellValVec <- c(object@statusCol)
		vfromtbl <- vtablename
		if(!object@statusCol %in% colnames(newdata))
			# stop(object@statusCol," not in newdata \n ")
			vfromtbl <- vtablename2

		if(length(vSurvival)==3 || object@timeValCol %in% colnames(newdata)){
			vVaridVec <- c(vVaridVec,-1)
			vCellValVec <- c(vCellValVec,object@timeValCol)
			vfromtbl <- c(vfromtbl,vtablename)
			if(!object@timeValCol %in% colnames(newdata))
			vfromtbl <- c(vfromtbl,vtablename2)
		}
		else if(length(vSurvival)==4)
		{
			vTimeVal1 <- vSurvival[2]
			vTimeVal2 <- vSurvival[3]
			vVaridVec <- c(vVaridVec,-1)
			vCellValVec <- c(vCellValVec,
							paste0(vTimeVal1,"-",vTimeVal2))
			if(!all(c(vTimeVal1,vTimeVal2) %in% colnames(newdata))){
				# stop("timeValue columns not found in newdata \n ")
				vfromtbl <- c(vfromtbl,vtablename2)
			}
			else{
				vfromtbl <- c(vfromtbl,vtablename)
			}
		}
		else stop("newdata is not consistent with formula object for scoring \n ")

        vRegrDataPrepSpecs <- setDefaultsRegrDataPrepSpecs(x=object@RegrDataPrepSpecs,
                                                            values=list(...))
        deepx <- FLRegrDataPrep(newdata,depCol=vRegrDataPrepSpecs$depCol,
                                outDeepTableName=vRegrDataPrepSpecs$outDeepTableName,
                                outObsIDCol=vRegrDataPrepSpecs$outObsIDCol,
                                outVarIDCol=vRegrDataPrepSpecs$outVarIDCol,
                                outValueCol=vRegrDataPrepSpecs$outValueCol,
                                catToDummy=vRegrDataPrepSpecs$catToDummy,
                                performNorm=vRegrDataPrepSpecs$performNorm,
                                performVarReduc=vRegrDataPrepSpecs$performVarReduc,
                                makeDataSparse=vRegrDataPrepSpecs$makeDataSparse,
                                minStdDev=vRegrDataPrepSpecs$minStdDev,
                                maxCorrel=vRegrDataPrepSpecs$maxCorrel,
                                trainOrTest=1,
                                excludeCols=vRegrDataPrepSpecs$excludeCols,
                                classSpec=vRegrDataPrepSpecs$classSpec,
                                whereconditions=vRegrDataPrepSpecs$whereconditions,
                                inAnalysisID=object@wideToDeepAnalysisId)

		# deepx <- FLRegrDataPrep(newdata,depCol="",
		# 						outDeepTableName="",
		# 						outObsIDCol="",
		# 						outVarIDCol="",
		# 						outValueCol="",
		# 						catToDummy=0,
		# 						performNorm=0,
		# 						performVarReduc=0,
		# 						makeDataSparse=1,
		# 						minStdDev=0,
		# 						maxCorrel=1,
		# 						trainOrTest=1,
		# 						excludeCols="",
		# 						classSpec=list(),
		# 						whereconditions="",
		# 						inAnalysisID=object@wideToDeepAnalysisId)

		newdata <- deepx[["table"]]
		newdata <- setAlias(newdata,"")

		vtablename1 <- newdata@select@table_name
		vobsid <- getVariables(object@table)[["obs_id_colname"]]
		sqlstr <- paste0("INSERT INTO ",vtablename1,"\n        ",
						paste0(" SELECT ",vobsid," AS obs_id_colname,","\n ",
										vVaridVec," AS var_id_colname, \n ",
										vCellValVec," AS cell_val_colname \n  ",
								" FROM ",vfromtbl,collapse=" UNION ALL "))
		t <- sqlSendUpdate(getFLConnection(),sqlstr)
		newdata@dimnames[[2]] <- c("-1","-2",newdata@dimnames[[2]])
	}
	vtable <- newdata@select@table_name
	vobsid <- getVariables(newdata)[["obs_id_colname"]]
	vvarid <- getVariables(newdata)[["var_id_colname"]]
	vvalue <- getVariables(newdata)[["cell_val_colname"]]

	AnalysisID <- sqlStoredProc(getFLConnection(),
								"FLCoxPHScore",
								outputParameter=c(AnalysisID="a"),
								INPUT_TABLE=newdata@select@table_name,
								OBSID_COL=vobsid,
								VARID_COL=vvarid,
								VALUE_COL=vvalue,
								ANALYSISID=object@AnalysisID,
								SCORE_TABLE=scoreTable,
								SURVIVAL_CURVE_TABLE=survivalCurveTable,
								NOTE=genNote("Scoring coxph"))
	# sqlstr <- paste0("CALL FLCoxPHScore (",fquote(newdata@select@table_name),",",
	# 										 fquote(vobsid),",",
	# 										 fquote(vvarid),",",
	# 										 fquote(vvalue),",",
	# 										 fquote(object@AnalysisID),",",
	# 										 fquote(scoreTable),",",
	# 										 fquote(survivalCurveTable),",",
	# 										 fquote(genNote("Scoring coxph")),
	# 										 ",oAnalysisID);")

	# AnalysisID <- sqlQuery(getFLConnection(),
 #                           sqlstr,
 #                           AnalysisIDQuery=genAnalysisIDQuery("fzzlCoxPHInfo",genNote("Scoring coxph")))
	AnalysisID <- checkSqlQueryOutput(AnalysisID)

	sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
						vobsid," AS vectorIndexColumn,",
						"HazardRatio AS vectorValueColumn",
					" FROM ",scoreTable)

	tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = getFLConnection(),
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

	flv <- newFLVector(
				select = tblfunqueryobj,
				dimnames = list(rownames(newdata),
								"vectorValueColumn"),
				isDeep = FALSE)

	vScore <- flv
	sqlstr <- paste0(limitRowsSQL(paste0("SELECT * from ",
								survivalCurveTable),100),
								" ORDER BY 1")
	vSurvival <- sqlQuery(getFLConnection(),sqlstr)
	return(list(score=vScore,
				survival=vSurvival))
}

#' @export
`$.FLCoxPH`<-function(object,property){
	#parentObject <- deparse(substitute(object))
	parentObject <- unlist(strsplit(unlist(strsplit(
		as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]
	if(property=="coefficients"){
		coefficientsvector <- coefficients.FLCoxPH(object)
		assign(parentObject,object,envir=parent.frame())
		return(coefficientsvector)
	}
	else if (property=="linear.predictors"){
		if(!is.null(object@results[["linear.predictors"]]))
		return(object@results[["linear.predictors"]])

		scoreTable <- getRemoteTableName(tableName = gen_score_table_name(object@table@select@table_name), temporaryTable = FALSE)
		survivalCurveTable <- getRemoteTableName(tableName = gen_score_table_name("surv"), temporaryTable = FALSE)

		vtemp <- predict(object,
						newdata=object@deeptable,
						scoreTable=scoreTable,
						survivalCurveTable=survivalCurveTable)
		hazardratiovector <- vtemp$score
		object@results <- c(object@results,
			list(linear.predictors=hazardratiovector,
				FLSurvivalData=vtemp$survival,
				FLSurvivalDataTable=survivalCurveTable))
		assign(parentObject,object,envir=parent.frame())
		return(hazardratiovector)
	}
	else if (property=="FLSurvivalData"){
		if(is.null(object@results[["FLSurvivalData"]]))
		vtemp <- object$linear.predictors
		# survivalCurveTable <- object@results[["FLSurvivalDataTable"]]

		# obs_id_colname <- getVariables(object@deeptable)[["obs_id_colname"]]
		# vsqlstr <- paste0("SELECT * FROM ",survivalCurveTable,
		# 				" \nWHERE ",obs_id_colname," < 6 ORDER BY 1")
		# if(!is.null(getOption("InteractiveFL")) && getOption("InteractiveFL"))
		# {
		# 	vinput <- readline("Fetching for top 5 observations only.--Recommended Continue? y/n ")
		# 	if(!checkYorN(vinput))
		# 	vsqlstr <- paste0("SELECT * FROM ",survivalCurveTable," ORDER BY 1")
		# }
		# vsurvivaldata <- sqlQuery(getFLConnection(),vsqlstr)
		vsurvivaldata <- object@results[["FLSurvivalData"]]
		assign(parentObject,object,envir=parent.frame())
		return(vsurvivaldata)
	}
	else if(property=="FLCoxPHStats")
	{
		if(!is.null(object@results[["FLCoxPHStats"]]))
		return(object@results[["FLCoxPHStats"]])
		else
		{
			sqlstr <- paste0("SELECT * FROM fzzlCoxPHStats\n",
							" WHERE AnalysisID=",fquote(object@AnalysisID))

			statsdataframe <- sqlQuery(getFLConnection(),sqlstr)
			object@results <- c(object@results,list(FLCoxPHStats=statsdataframe))
			assign(parentObject,object,envir=parent.frame())
			return(statsdataframe)
		}
	}
	else if(property=="loglik")
	{
		vstats <- object$FLCoxPHStats
		colnames(vstats) <- toupper(colnames(vstats))
		loglikvector <- vstats[1,c("PARTIALLL","LIKELIHOODSTATS")]
		names(loglikvector) <- c("partialLL","likelihoodStats")
		assign(parentObject,object,envir=parent.frame())
		return(loglikvector)
	}
	else if(property %in% c("wald.test",
			"rscore","nevent","n"))
	{
		vtemp <- c("wald.test","rscore","nevent","n")
		names(vtemp) <- c("WALDSTATS","LOGRANKSTATS",
			"NUMOFEVENTS","NUMOFOBS")
		vproperty <- names(vtemp)[property==vtemp]
		statsdataframe <- object$FLCoxPHStats
		colnames(statsdataframe) <- toupper(colnames(statsdataframe))
		resultvector <- as.vector(statsdataframe[[vproperty]])
        names(resultvector) <- NULL
		##names(resultvector) <- vproperty
		assign(parentObject,object,envir=parent.frame())
		return(resultvector)
	}
	else if(property %in% c("FLCoeffZScore",
		"FLCoeffPValue","FLCoeffStdErr",
		"FLCoeffexp","FLCoeffexpneg",
		"FLCoefflowerlimit","FLCoeffupperlimit"))
	{
		coeffVector <- coefficients.FLCoxPH(object)
		assign(parentObject,object,envir=parent.frame())
		return(object@results[[property]])
	}
	else if(property=="call")
	{
		return(object@results[["call"]])
	}
	else if(property=="formula")
	{
		return(object@formula)
	}
	else if(property=="model")
	{
			coeffVector <- object$coefficients
			vtemp <- c(object@timeValCol,
						names(coeffVector))
            names(vtemp) <- c(as.character(object@formula)[2],
                            names(coeffVector))
            object@results[["modelColnames"]] <- vtemp
			modelframe <- model.FLLinRegr(object)
			assign(parentObject,object,envir=parent.frame())
			return(modelframe)
	}
	else if(property=="x")
	{
		if(!is.null(object@results[["x"]]))
		return(object@results[["x"]])

		coeffVector <- object$coefficients
		object@results[["XMatrixColnames"]] <- names(coeffVector)
		modelframe <- getXMatrix(object,
								pDropCols=c(-1,-2,0))
		object@results <- c(object@results,list(x=modelframe))
		assign(parentObject,object,envir=parent.frame())
		return(modelframe)

		# modelframe <- object$model
		# modelframe[[object@statusCol]] <- NULL
		# assign(parentObject,object,envir=parent.frame())
		# return(modelframe)
	}
	else if(property=="means")
	{
		if(!is.null(object@results[["means"]]))
		return(object@results[["means"]])
		else
		{
			coeffVector <- object$coefficients
			vcolnames <- names(coeffVector)
			deeptablename <- object@deeptable@select@table_name
			obs_id_colname <- getVariables(object@deeptable)[["obs_id_colname"]]
			var_id_colname <- getVariables(object@deeptable)[["var_id_colname"]]
			cell_val_colname <- getVariables(object@deeptable)[["cell_val_colname"]]
			vsqlstr <- paste0("SELECT ",var_id_colname," AS varID,\n",
							" FLMEAN(",cell_val_colname,") AS meanval \n",
							" FROM ",deeptablename," GROUP BY ",var_id_colname,
							" \n WHERE ",var_id_colname," NOT IN('-1','0','-2')\n",
							" ORDER BY ",var_id_colname)
			meansvector <- sqlQuery(getFLConnection(),vsqlstr)[["meanval"]]
			names(meansvector) <- vcolnames
			object@results <- c(object@results,list(means=meansvector))
			assign(parentObject,object,envir=parent.frame())
			return(meansvector)
		}
	}
	else if(property=="terms")
	{
		if(!is.null(object@results[["terms"]]))
		return(object@results[["terms"]])
		else
		{
			coeffVector <- object$coefficients
			vallVars <- all.vars(object@formula)
			vcolnames <- names(coeffVector)
			vterms <- terms(formula(paste0(vallVars[1],"~",
				paste0(vcolnames,collapse="+"))))
			object@results <- c(object@results,list(terms=vterms))
			assign(parentObject,object,envir=parent.frame())
			return(vterms)
		}
	}
	else stop("That's not a valid property")
}

#' @export
coefficients.FLCoxPH <- function(object){
	if(!is.null(object@results[["coefficients"]]))
	return(object@results[["coefficients"]])
	else
	{
        object@vfcalls <- c(coefftablename="fzzlCoxPHCoeffs")
        vres <- coefficients.lmGeneric(object,
                        FLCoeffStats=c(FLCoeffStdErr="STDERR",
                                    FLCoeffZScore="ZSCORE",
                                    FLCoeffPValue="PVALUE",
                                    FLCoeffexpneg="EXPNEGCOEFF",
                                    FLCoeffexp="EXPCOEFF",
                                    FLCoefflowerlimit="LOWERLIMIT",
                                    FLCoeffupperlimit="UPPERLIMIT"),
                        pIntercept=FALSE)
		# if(object@table@isDeep)
		# coeffVector <- sqlQuery(getFLConnection(),
		# 	paste0("SELECT * FROM fzzlCoxPHCoeffs where AnalysisID=",fquote(object@AnalysisID),
		# 			" ORDER BY CoeffID"))
		# else
		# coeffVector <- sqlQuery(getFLConnection(),
		# 	paste0("SELECT CASE WHEN a.Catvalue IS NOT NULL THEN \n",
		# 			"a.COLUMN_NAME || a.Catvalue ELSE \n",
		# 			"a.Column_name END AS CoeffName,b.* \n",
		# 		   " FROM fzzlRegrDataPrepMap AS a,fzzlCoxPHCoeffs AS b \n",
		# 		   " WHERE a.Final_VarID = b.CoeffID \n",
		# 			" AND a.AnalysisID = ",fquote(object@wideToDeepAnalysisId),
		# 			"\n AND b.AnalysisID = ",fquote(object@AnalysisID),
		# 			"\n ORDER BY CoeffID"))

		# colnames(coeffVector) <- toupper(colnames(coeffVector))
		# stderrVector <- coeffVector[["STDERR"]]
		# zscoreVector <- coeffVector[["ZSCORE"]]
		# pvalVector <- coeffVector[["PVALUE"]]
		# lowerlimitVector <- coeffVector[["LOWERLIMIT"]]
		# expnegcoeffVector <- coeffVector[["EXPNEGCOEFF"]]
		# upperlimitVector <- coeffVector[["UPPERLIMIT"]]
		# coeffVector1 <- coeffVector[["COEFFVALUE"]]
		# expcoeffVector <- coeffVector[["EXPCOEFF"]]

		# if(!is.null(coeffVector[["COEFFNAME"]]))
		# names(coeffVector1) <- coeffVector[["COEFFNAME"]]
		# else{
		# 	vallVars <- all.vars(genDeepFormula(coeffVector[["COEFFID"]]))
		# 	names(coeffVector1) <- vallVars[2:length(vallVars)]
		# }
		
		# vcolnames <- colnames(object@deeptable)
		# droppedCols <- vcolnames[!vcolnames %in% c("-1","0","-2",coeffVector[["COEFFID"]])]
		# object@results <- c(object@results,list(coefficients=coeffVector1,
		# 										FLCoeffStdErr=stderrVector,
		# 										FLCoeffZScore=zscoreVector,
		# 										FLCoeffPValue=pvalVector,
		# 										FLCoeffexpneg=expnegcoeffVector,
		# 										FLCoeffexp=expcoeffVector,
		# 										FLCoefflowerlimit=lowerlimitVector,
		# 										FLCoeffupperlimit=upperlimitVector,
		# 										droppedCols=droppedCols))
		parentObject <- unlist(strsplit(unlist(strsplit
			(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(vres)
	}
}

# model.FLCoxPH <- function(object)
# {
# 	if(!is.null(object@results[["model"]]))
# 	return(object@results[["model"]])
# 	else
# 	{
# 		# if(interactive())
# 		# {
# 		# 	vinput <- readline("Fetching entire table. Continue? y/n ")
# 		# 	if(!checkYorN(vinput)) return(NULL)
# 		# }
# 		modelframe <- as.data.frame(object@deeptable)
# 		modelframe[["0"]] <- NULL ##Intercept
# 		modelframe[["-1"]] <- NULL ##timeValue
# 		coeffVector <- object$coefficients
# 		vdroppedCols <- object@results[["droppedCols"]]
# 		for(i in vdroppedCols)
# 		modelframe[[paste0(i)]] <- NULL
# 		vallVars <- all.vars(object@formula)
# 		vcolnames <- c(object@statusCol,names(coeffVector))
# 		colnames(modelframe) <- vcolnames
# 		object@results <- c(object@results,list(model=modelframe))
# 		parentObject <- unlist(strsplit(unlist(strsplit(
# 			as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
# 		assign(parentObject,object,envir=parent.frame())
# 		return(modelframe)
# 	}
# }

#' @export
summary.FLCoxPH <- function(object){
  stat <- object$FLCoxPHStats
  
  coefficients <- data.frame(as.vector(object$coefficients),
                             as.vector(object$FLCoeffexp),
                             as.vector(object$FLCoeffStdErr),
                             as.vector(object$FLCoeffZScore),
                             as.vector(object$FLCoeffPValue))
  
  rname <- all.vars(object$formula)
  colnames(coefficients) <- c("coef","exp(coef)","se(coef)","z","Pr(>|z|)")    
  rownames(coefficients) <- names(object$coefficients) 
  
  
  conf.int <- data.frame(as.vector(object$FLCoeffexp),
                         as.vector(object$FLCoeffexpneg),
                         as.vector(object$FLCoefflowerlimit),
                         as.vector(object$FLCoeffupperlimit))
  
  
  colnames(conf.int) <- c("exp(coef)","exp(-coef)","lower.95","upper.95")
  rownames(conf.int) <- names(object$coefficients)
  
  
  
  
  
  reqList <- list(call = as.call(object$call),
                  n = object$n,
                  nevent = as.numeric(object$nevent),
                  coefficients = as.matrix(coefficients),
                  conf.int = as.matrix(conf.int),
                  waldtest = c(test = stat$WaldStats,df = length(object$coefficients) , pvalue = stat$WaldPValue),
                  sctest = c(test = stat$LogRankStats, df = length(`object`$coefficients) , pvalue = stat$LogRankPValue),
                  rsq = as.numeric(NULL),
                  logtest = c(test = stat$LikelihoodStats, df = length(`object`$coefficients) , pvalue = stat$LikelihoodPValue),
                  #concordance = NULL,
                  used.robust = FALSE
                  
  )
  
  class(reqList) <- "summary.coxph"
  reqList
  
}

#' @export
print.FLCoxPH <- function(object){
	parentObject <- unlist(strsplit(unlist(strsplit(
		as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	summary.FLCoxPH(object)
	assign(parentObject,object,envir=parent.frame())
}

## Choose number of observations to fetch and plot
#' @export
plot.FLCoxPH <- function(object,nobs=5,...){
	if(is.null(object@results[["FLSurvivalDataTable"]]))
	vtemp <- object$linear.predictors
	survivalCurveTable <- object@results[["FLSurvivalDataTable"]]
	if(is.na(nobs) || is.null(nobs)) nobs<-5
	if(is.numeric(nobs))  nobs<- as.integer(nobs[1])
	else nobs <- 5
	obs_id_colname <- getVariables(object@deeptable)[["obs_id_colname"]]
	vsqlstr <- paste0("SELECT * FROM ",survivalCurveTable,
					" \nWHERE ",obs_id_colname," <= ",nobs," ORDER BY 1")

	vsurvivaldata <- sqlQuery(getFLConnection(),vsqlstr)
	colnames(vsurvivaldata) <- toupper(colnames(vsurvivaldata))

	parentObject <- unlist(strsplit(unlist(
		strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	assign(parentObject,object,envir=parent.frame())

	plot(vsurvivaldata[["TIMEVAL"]],vsurvivaldata[["SURVIVALPROB"]],
		col=vsurvivaldata[[toupper(obs_id_colname)]],
		xlab = "Time",ylab = "Survival Probability",
		title(main = "Survival curve plot FL"))
}

prepareSurvivalFormula <- function(data,
                                 formula
                                 ){

    IncludeTimeVal <- function(data,
                           formula,
                           vTimeVal=NULL){
        vSurvival <- as.character(attr(terms(formula),"variables")[[2]])
        vTimeVal1 <- vSurvival[2]
        vTimeVal2 <- vSurvival[3]
        vStatus <- vSurvival[4]
        if(is.null(vTimeVal))
        vTimeVal <- "FLTimeValCol"
        vtablename1 <- data@select@table_name

        vtablename <- createView(pViewName=gen_unique_table_name(""),
                                pSelect=paste0("SELECT b.",vTimeVal2," - b.",vTimeVal1,
                                            " AS ",vTimeVal,",b.* FROM ",vtablename1," AS b ")
                    )
        
        data@dimnames[[2]] <- c(data@dimnames[[2]],vTimeVal)
        data@select@table_name <- vtablename
        vallVars <- base::all.vars(formula)
        vallVars <- vallVars[!vallVars %in% c(vTimeVal1,vTimeVal2)]
        return(list(data=data,
                    vTimeVal=vTimeVal,
                    vStatus=vStatus,
                    vtablename=vtablename,
                    vallVars=vallVars))
    }

    if(isDotFormula(formula))
            formula <- genDeepFormula(pColnames=colnames(data),
                                    pDepColumn=all.vars(formula)[1])
        vallVars <- base::all.vars(formula)
        vtablename <- NULL
        checkValidFormula(formula,data)
        vSurvival <- as.character(attr(terms(formula),"variables")[[2]])
        if(!("Surv" %in% vSurvival))
        stop("specify dependent variables as Surv object")
        if(length(vSurvival)==2)
        stop("atleast time and event components must be present in Surv object")
        if(length(vSurvival)==3)
        {
            vTimeVal <- vSurvival[2]
            vStatus <- vSurvival[3]
        }
        else if(length(vSurvival)==4)
        {
            vtempList <- IncludeTimeVal(data=data,
                                        formula=formula)
            vStatus <- vtempList[["vStatus"]]
            vtablename <- vtempList[["vtablename"]]
            vTimeVal <- vtempList[["vTimeVal"]]
            data <- vtempList[["data"]]
            vallVars <- vtempList[["vallVars"]]
            vallVars <- c(vallVars,vTimeVal)
        }
        else stop("Error in formula:check function documentation for constraints on formula \n ")
        vallVars <- vallVars[vallVars!=vStatus]
        vIndepVars <- attr(terms(formula),"term.labels")
        return(list(vStatus=vStatus,
                    vTimeVal=vTimeVal,
                    data=data,
                    vallVars=vallVars,
                    vtablename=vtablename,
                    formula=formula,
                    vSurvival=vSurvival,
                    vIndepVars=vIndepVars))
}

