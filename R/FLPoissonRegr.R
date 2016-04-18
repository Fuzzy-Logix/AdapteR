#' @include utilities.R
#' @include data_prep.R
#' @include FLTable.R
NULL

setClass(
	"FLPoissonRegr",
	slots=list(formula="formula",
				AnalysisID="character",
				wideToDeepAnalysisId="character",
				table="FLTable",
				results="list",
				deeptable="FLTable",
				mapTable="character",
				scoreTable="character",
				offset="character"))

#' @export
glm <- function (formula,family=stats::gaussian,data=list(),...) {
	UseMethod("glm", data)
 }

#' @export
glm.default <- stats::glm


#' @examples
#' library(RODBC)
#' connection <- flConnect(odbcSource = "Gandalf",database = "FL_DEV")
#' widetable  <- FLTable("FL_DEV", "siemenswidetoday1", "ObsID")
#' options(debugSQL=T)
#' pfitToffA <- glm(event ~ meanTemp, family=poisson, data=widetable,offset="age")
#' predData <- FLTable("FL_DEV","preddata1","ObsID")
#' mu <- predict(pfitToffA,newdata=predData)
#' @export
glm.FLTable <- function(formula,
					family=stats::poisson,
					data,
					offset="",
					maxiter=25,
					catToDummy=0,
					performNorm=0,
					performVarReduc=0,
					makeDataSparse=0,
					minStdDev=0,
					maxCorrel=1,
					classSpec=list(),
					whereconditions="",
					...)
{
	if(is.character(family) && base::toupper(family)!="POISSON")
	stop("only poisson family is supported currently\n")
	if(is.function(family) && !base::identical(family,stats::poisson))
	stop("only poisson family is supported currently\n")
	family <- stats::poisson()

	vallVars <- base::all.vars(formula)
	vdependent <- all.vars(formula)[1]
	vindependent <- all.vars(formula)[2:length(formula)]
	checkValidFormula(formula,data)
	
	if(!is.numeric(maxiter)||maxiter[1]<1)
	stop("invalid maxiter")
	maxiter <- as.integer(maxiter[1])
	vcolnames <- colnames(data)
	wideToDeepAnalysisId <- ""
    mapTable <- ""

    if(offset!="" && !toupper(offset) %in% toupper(vcolnames))
    stop("offset not in colnames of data")
    unused_cols <- vcolnames[!vcolnames %in% all.vars(formula)]
	unused_cols <- unused_cols[unused_cols!=getVariables(data)[["obs_id_colname"]]]
	vexcludeCols <- paste0(unused_cols,collapse=",")

	vcallObject <- match.call()
	if(!data@isDeep)
	{

		deepx <- FLRegrDataPrep(data,depCol=vdependent,
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
						ifelse(offset!="",offset,1)," AS cell_val_colname","\n        ",
						" FROM ",vtablename1)
		t <- sqlSendUpdate(getOption("connectionFL"),sqlstr)
		deepx@dimnames[[2]] <- c("-2",deepx@dimnames[[2]])
		whereconditions <- ""
		mapTable <- getRemoteTableName(getOption("ResultDatabaseFL"),
					"fzzlRegrDataPrepMap")
	}
	else if(class(data@select)=="FLTableFunctionQuery")
	{
		deeptablename <- gen_view_name("")
		sqlstr <- paste0("CREATE VIEW ",getOption("ResultDatabaseFL"),
							".",deeptablename," AS ",constructSelect(data))
		sqlSendUpdate(connection,sqlstr)

		deeptablename1 <- gen_view_name("New")
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
		deeptablename <- gen_view_name("New")
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

	deeptable <- paste0(deepx@select@database,".",deepx@select@table_name)

    note <- genNote("poisson")
    sqlstr <- paste0("CALL FLPoissonRegr(",fquote(deeptable),",",
					 				fquote(getVariables(deepx)[["obs_id_colname"]]),",",
					 				fquote(getVariables(deepx)[["var_id_colname"]]),",",
					 				fquote(getVariables(deepx)[["cell_val_colname"]]),",",
					 				maxiter,",",note,
					 				",AnalysisID );")
	
	retobj <- sqlQuery(connection,sqlstr,
                       AnalysisIDQuery=genAnalysisIDQuery("fzzlPoissonRegrInfo",note))
	retobj <- checkSqlQueryOutput(retobj)
	AnalysisID <- as.character(retobj[1,1])
	
	return(new("FLPoissonRegr",
				formula=formula,
				AnalysisID=AnalysisID,
				wideToDeepAnalysisId=wideToDeepAnalysisId,
				table=data,
				results=list(call=vcallObject),
				deeptable=deepx,
				mapTable=mapTable,
				scoreTable="",
				offset=offset))
}

#' @export
predict.FLPoissonRegr <- function(object,
								newdata=object@table,
								scoreTable=""){
	if(!is.FLTable(newdata)) stop("scoring allowed on FLTable only")
	vinputTable <- paste0(newdata@select@database,".",newdata@select@table_name)

	if(scoreTable=="")
	scoreTable <- paste0(getOption("ResultDatabaseFL"),".",gen_score_table_name(object@table@select@table_name))
	else if(!grep(".",scoreTable)) scoreTable <- paste0(getOption("ResultDatabaseFL"),".",scoreTable)
	

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
						ifelse(object@offset!="",offset,1)," AS cell_val_colname","\n        ",
						" FROM ",vtablename1)
		t <- sqlSendUpdate(getOption("connectionFL"),sqlstr)
		newdata@dimnames[[2]] <- c("-2",newdata@dimnames[[2]])
	}
	vtable <- paste0(newdata@select@database,".",newdata@select@table_name)
	vobsid <- getVariables(newdata)[["obs_id_colname"]]
	vvarid <- getVariables(newdata)[["var_id_colname"]]
	vvalue <- getVariables(newdata)[["cell_val_colname"]]
    note <- genNote("Scoring poisson")
	sqlstr <- paste0("CALL FLPoissonRegrScore (",fquote(vtable),",",
											 fquote(vobsid),",",
											 fquote(vvarid),",",
											 fquote(vvalue),",",
											 fquote(object@AnalysisID),",",
											 fquote(scoreTable),",",
											 note,
											 ",oAnalysisID);")

	AnalysisID <- sqlQuery(getOption("connectionFL"),
                           sqlstr,
                           AnalysisIDQuery=genAnalysisIDQuery("fzzlPoissonRegrInfo",note))
	AnalysisID <- checkSqlQueryOutput(AnalysisID)

	sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
						vobsid," AS vectorIndexColumn,",
						"Mu AS vectorValueColumn",
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

	return(flv)
}
