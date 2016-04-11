#' @include utilities.R
#' @include data_prep.R
#' @include FLTable.R
NULL

setClass(
	"FLCoxPH",
	slots=list(formula="formula",
				AnalysisID="character",
				wideToDeepAnalysisId="character",
				table="FLTable",
				results="list",
				deeptable="FLTable",
				mapTable="character",
				scoreTable="character",
				modelID="numeric",
				timeValCol="character",
				statusCol="character"))

#' @export
coxph <- function (formula,data=list(),...) {
	UseMethod("coxph", data)
 }

#' @export
coxph.default <- survival::coxph

#' @export
prepareData.coxph <- function(formula,data,
                              catToDummy=0,
                              performNorm=0,
                              performVarReduc=0,
                              makeDataSparse=0,
                              minStdDev=0,
                              maxCorrel=1,
                              classSpec=list(),
                              whereconditions=""){
	vallVars <- base::all.vars(formula)
	vTimeVal <- ""
	vStatus <- ""
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
		vTimeVal1 <- vSurvival[2]
		vTimeVal2 <- vSurvival[3]
		vStatus <- vSurvival[4]
		a <- genRandVarName()
		vTimeVal <- genRandVarName()
		vtablename <- paste0(getOption("ResultDatabaseFL"),".",a)
		vtablename1 <- paste0(data@select@database,".",data@select@table_name)

		sqlstr <- paste0("CREATE TABLE ",vtablename,
						" AS(SELECT b.",vTimeVal2," - b.",vTimeVal1,
							" AS ",vTimeVal,",b.* FROM ",vtablename1," AS b)WITH DATA")
		t <- sqlSendUpdate(getOption("connectionFL"),sqlstr)
		data@dimnames[[2]] <- c(data@dimnames[[2]],vTimeVal)
		data@select@database <- getOption("ResultDatabaseFL")
		data@select@table_name <- a
		vallVars <- vallVars[!vallVars %in% c(vTimeVal1,vTimeVal2)]
		vallVars <- c(vallVars,vTimeVal)
	}

	vallVars <- vallVars[vallVars!=vStatus]
	vcolnames <- colnames(data)
	wideToDeepAnalysisId <- ""
    mapTable <- ""

	unused_cols <- vcolnames[!vcolnames %in% vallVars]
	unused_cols <- unused_cols[unused_cols!=getVariables(data)[["obs_id_colname"]]]
	vexcludeCols <- paste0(unused_cols,collapse=",")
	
	if(!data@isDeep)
	{
		deepx <- FLRegrDataPrep(data,depCol=vTimeVal,
								outDeepTableName="",
								outDeepTableDatabase="",
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

		wideToDeepAnalysisId <- deepx[["AnalysisID"]]
		deepx <- deepx[["table"]]

		vtablename <- paste0(deepx@select@database,".",deepx@select@table_name)
		vtablename1 <- paste0(data@select@database,".",data@select@table_name)
		vobsid <- getVariables(data)[["obs_id_colname"]]
		sqlstr <- paste0("INSERT INTO ",vtablename,"\n        ",
						" SELECT ",vobsid," AS obs_id_colname,","\n               ",
						" -2 AS var_id_colname,","\n               ",
						vStatus," AS cell_val_colname","\n               ",
						" FROM ",vtablename1)
		t <- sqlSendUpdate(getOption("connectionFL"),sqlstr)
		deepx@dimnames[[2]] <- c("-2",deepx@dimnames[[2]])
		whereconditions <- ""
		mapTable <- getRemoteTableName(getOption("ResultDatabaseFL"),
					"fzzlRegrDataPrepMap")
	}
	else if(class(data@select)=="FLTableFunctionQuery")
	{
		deeptablename <- genRandVarName()
		sqlstr <- paste0("CREATE VIEW ",getOption("ResultDatabaseFL"),
							".",deeptablename," AS ",constructSelect(data))
		sqlSendUpdate(connection,sqlstr)

		deeptablename1 <- gen_deep_table_name("New")
		sqlstr <- paste0("CREATE VIEW ",getOption("ResultDatabaseFL"),".",deeptablename1,
						" AS SELECT * FROM ",getOption("ResultDatabaseFL"),".",deeptablename,
						constructWhere(whereconditions))
		t <- sqlQuery(connection,sqlstr)
		if(length(t)>1) stop("Input Table and whereconditions mismatch,Error:",t)

		deepx <- FLTable(
                   getOption("ResultDatabaseFL"),
                   deeptablename1,
                   "obs_id_colname",
                   "var_id_colname",
                   "cell_val_colname"
                  )
		whereconditions <- ""
	}
	else
	{
		data@select@whereconditions <- c(data@select@whereconditions,whereconditions)
		deeptablename <- gen_deep_table_name("New")
		sqlstr <- paste0("CREATE VIEW ",getOption("ResultDatabaseFL"),".",
						deeptablename," AS ",constructSelect(data))
		t <- sqlQuery(connection,sqlstr)
		if(length(t)>1) stop("Input Table and whereconditions mismatch")
		deepx <- FLTable(
                   getOption("ResultDatabaseFL"),
                   deeptablename,
                   "obs_id_colname",
                   "var_id_colname",
                   "cell_val_colname"
                  )
		whereconditions <- ""
	}
    return(list(deeptable=deepx,
                wideToDeepAnalysisId=wideToDeepAnalysisId,
                mapTable=mapTable,
                vStatus=vStatus,
                vTimeVal=vTimeVal))
}

#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' widetable  <- FLTable("FL_DEMO", "siemenswidetoday1", "ObsID")
#' fitT <- coxph(Surv(startDate,endDate,event)~meanTemp+age,widetable)
#' predData <- FLTable("FL_DEMO","preddatatoday","ObsID")
#' resultList <- predict(fitT,newdata=predData)
#' resultList[[1]]
#' resultList[[2]]
## Failed
#fitT <- coxph(Surv(startDate,endDate,event)~meanTemp+age+lage,widetable)
# fitT <- coxph(Surv(startDate,endDate,event)~meanTemp,widetable)
# fitT <- coxph(Surv(age,event)~meanTemp,widetable)
#' @export
coxph.FLTable <- function(formula,data, ...)
{
	checkValidFormula(formula,data)
    deep <- prepareData.coxph(formula,data,...)
    wideToDeepAnalysisId <- deep$wideToDeepAnalysisId
    deepx <- deep[["deeptable"]]
    
	deeptable <- paste0(deepx@select@database,".",deepx@select@table_name)

    sqlstr <- paste0("CALL FLCoxPH(",fquote(deeptable),",",
					 				fquote(getVariables(deepx)[["obs_id_colname"]]),",",
					 				fquote(getVariables(deepx)[["var_id_colname"]]),",",
					 				fquote(getVariables(deepx)[["cell_val_colname"]]),
					 				",15,'CoxPH from AdapteR',AnalysisID );")
	
	retobj <- sqlQuery(connection,sqlstr,
                       AnalysisIDQuery="SELECT top 1 ANALYSISID from fzzlCoxPHInfo where Note='CoxPH from AdapteR' order by RUNENDTIME DESC")
	retobj <- checkSqlQueryOutput(retobj)
	AnalysisID <- as.character(retobj[1,1])
	
	vcallObject <- match.call()
    
	return(new("FLCoxPH",
				formula=formula,
				AnalysisID=AnalysisID,
				wideToDeepAnalysisId=wideToDeepAnalysisId,
				table=data,
				results=list(call=vcallObject),
				deeptable=deepx,
				mapTable=deep$mapTable,
				scoreTable="",
				statusCol=deep$vStatus,
				timeValCol=deep$vTimeVal))
}

predict.FLCoxPH <-function(object,
							newdata=object@table,
							scoreTable="",
							survivalCurveTable=""){
	if(!is.FLTable(newdata)) stop("scoring allowed on FLTable only")
	vinputTable <- paste0(newdata@select@database,".",newdata@select@table_name)

	if(scoreTable=="")
	scoreTable <- paste0(getOption("ResultDatabaseFL"),".",gen_score_table_name(object@table@select@table_name))
	else if(!grep(".",scoreTable)) scoreTable <- paste0(getOption("ResultDatabaseFL"),".",scoreTable)
	if(survivalCurveTable=="")
	survivalCurveTable <- paste0(getOption("ResultDatabaseFL"),".",gen_score_table_name("survival"))
	else if(!grep(".",survivalCurveTable)) survivalCurveTable <- paste0(getOption("ResultDatabaseFL"),".",survivalCurveTable)

	if(!newdata@isDeep)
	{
		deepx <- FLRegrDataPrep(newdata,depCol="",
								outDeepTableName="",
								outDeepTableDatabase="",
								outObsIDCol="",
								outVarIDCol="",
								outValueCol="",
								catToDummy=0,
								performNorm=0,
								performVarReduc=0,
								makeDataSparse=0,
								minStdDev=0,
								maxCorrel=1,
								trainOrTest=1,
								excludeCols="",
								classSpec=list(),
								whereconditions="",
								inAnalysisID=object@wideToDeepAnalysisId)
		newdata <- deepx[["table"]]

		vtablename <- paste0(newdata@select@database,".",newdata@select@table_name)
		vtablename1 <- paste0(object@table@select@database,".",object@table@select@table_name)
		vobsid <- getVariables(object@table)[["obs_id_colname"]]
		sqlstr <- paste0("INSERT INTO ",vtablename,"\n        ",
						" SELECT ",vobsid," AS obs_id_colname,","\n               ",
						" -2 AS var_id_colname,","\n               ",
						object@statusCol," AS cell_val_colname","\n               ",
						" FROM ",vtablename1)
		t <- sqlSendUpdate(getOption("connectionFL"),sqlstr)
		newdata@dimnames[[2]] <- c("-2",newdata@dimnames[[2]])
	}
	vtable <- paste0(newdata@select@database,".",newdata@select@table_name)
	vobsid <- getVariables(newdata)[["obs_id_colname"]]
	vvarid <- getVariables(newdata)[["var_id_colname"]]
	vvalue <- getVariables(newdata)[["cell_val_colname"]]
	sqlstr <- paste0("CALL FLCoxPHScore (",fquote(newdata@select@table_name),",",
											 fquote(vobsid),",",
											 fquote(vvarid),",",
											 fquote(vvalue),",",
											 fquote(object@AnalysisID),",",
											 fquote(scoreTable),",",
											 fquote(survivalCurveTable),",",
											 "'Scoring using model ",object@AnalysisID,"',",
											 "oAnalysisID);")

	AnalysisID <- sqlQuery(getOption("connectionFL"),
                           sqlstr,
                           AnalysisIDQuery=paste0("SELECT top 1 ANALYSISID from fzzlCoxPHInfo where Note='Scoring using model ",object@AnalysisID,"' order by RUNENDTIME DESC"))
	AnalysisID <- checkSqlQueryOutput(AnalysisID)

	sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
						vobsid," AS vectorIndexColumn,",
						"HazardRatio AS vectorValueColumn",
					" FROM ",scoreTable)

	tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = getOption("connectionFL"),
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

	flv <- new("FLVector",
				select = tblfunqueryobj,
				dimnames = list(rownames(newdata),
								"vectorValueColumn"),
				isDeep = FALSE)

	vScore <- flv
	sqlstr <- paste0("SELECT TOP 100 * from ",survivalCurveTable," ORDER BY 1")
	vSurvival <- sqlQuery(getOption("connectionFL"),sqlstr)
	return(list(score=vScore,
				survival=vSurvival))
}
