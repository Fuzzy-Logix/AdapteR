#' @include utilities.R
#' @include data_prep.R
#' @include FLTable.R
NULL
#' An S4 class to represent FLLinRegr
#'
#' @slot formula an object of class 'formula': Model Formulae
#' @slot table_name A character
#' @slot deeptablename A character vector containing name of the deeptable on conversion from a widetable
#' @slot AnalysisID An output character ID from CALL FLLinRegr
#' @slot dataprepID An output character ID from CALL FLRegrDataPrep
#' @slot datatable An object of class FLTable
#' @method print FLLinRegr
#' @param object contains: call,coefficients
#' @method coefficients FLLinRegr
#' @param object a named vector of coefficients
#' @method summary FLLinRegr
#' @param object contains: call,residuals,coefficients,significant codes note and statistical output.
setClass(
	"FLLinRegr",
	slots=list(formula="formula",
				AnalysisID="character",
				wideToDeepAnalysisId="character",
				table="FLTable",
				results="list",
				deeptable="FLTable",
				mapTable="character",
				scoreTable="character",
				modelID="numeric"))

#' @export
lm <- function (formula,data=list(),...) {
	UseMethod("lm", data)
 }

#' @export
lm.default <- stats::lm

#' Linear Regression.
#'
#' \code{lm} performs linear regression on FLTable objects.
#'
#' The wrapper overloads lm and implicitly calls FLRegrDataPrep and FLLinRegr.
#' @method lm FLTable
#' @param formula A symbolic description of model to be fitted
#' @param data An object of class FLTable
#' @section Constraints:
#' None
#' @return \code{lm} performs linear regression and replicates equivalent R output.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' widetable  <- FLTable("FL_DEMO", "tblAbaloneWide", "ObsID")
#' lmfit <- lm(Rings~Height+Diameter,widetable)
#' deeptable <- FLTable("FL_DEMO","myLinRegrSmall","ObsID","VarID","Num_Val")
#' lmfit1 <- lm(NULL,deeptable)
#' deeptable <- FLTable("FL_DEMO","myLinRegrSmallBroken","ObsID","VarID","Num_Val")
#' lmfit2 <- lm(NULL,deeptable)
#' @export
lm.FLTable <- function(formula,data,
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
	return(lmGeneric(formula=formula,
					data=data,
					catToDummy=catToDummy,
					performNorm=performNorm,
					performVarReduc=performVarReduc,
					makeDataSparse=makeDataSparse,
					minStdDev=minStdDev,
					maxCorrel=maxCorrel,
					classSpec=classSpec,
					whereconditions=whereconditions,
					...))
}

lmGeneric <- function(formula,data,
					catToDummy=0,
					performNorm=0,
					performVarReduc=0,
					makeDataSparse=0,
					minStdDev=0,
					maxCorrel=1,
					classSpec=list(),
					whereconditions="",
					specID="NULL",
					highestpAllow1=0.5,
					highestpAllow2=0.1,
					stepWiseDecrease=0.05,
					topN=1,
					direction="UFbackward",
					...)
{
	if(data@isDeep){
		vallVars <- colnames(data)
		formula <- genDeepFormula(vallVars)
	}
	else{
		vallVars <- base::all.vars(formula)
		vdependent <- all.vars(formula)[1]
		vindependent <- all.vars(formula)[2:length(formula)]
		checkValidFormula(formula,data)
	}
	
	vcolnames <- colnames(data)
	wideToDeepAnalysisId <- ""
    mapTable <- ""

    check0To1 <- function(pObject)
    {
    	if(!is.numeric(pObject) || 
    		pObject < 0 ||
    		pObject > 1)
    	stop(names(pObject)," should be >0 and <1\n")
    }
    checkSpecID <- function(pObject,pAllVars)
    {
    	if(pObject!="NULL")
    	{
    		sapply(pObject,function(x)
	        if(!(x %in% pAllVars))
	        stop(x," not in colnames of data\n"))
    	}
    }
    if(direction=="UFbackward")
    {
    	check0To1(c(highestpAllow1=highestpAllow1,
    				highestpAllow2=highestpAllow2,
    				stepWiseDecrease=stepWiseDecrease))
    	checkSpecID(specID,vallVars)
    }
    if(direction=="backward")
    {
    	check0To1(c(highestpAllow1=highestpAllow1))
    	checkSpecID(specID,vallVars)
    }
    if(direction=="forward")
    {
    	check0To1(c(highestpAllow1=highestpAllow1))
    	if(!is.numeric(topN) || as.integer(topN)<1 || as.integer(topN)>10)
    	stop("topN should be >0 and <=10")
    	topN <- as.integer(topN)
    }
    if(direction=="Fbackward")
    {
    	check0To1(c(highestpAllow1=highestpAllow1,
    				highestpAllow2=highestpAllow2))
    	checkSpecID(specID,vallVars)
    }

    if(!data@isDeep){
    	unused_cols <- vcolnames[!vcolnames %in% all.vars(formula)]
		unused_cols <- unused_cols[unused_cols!=getVariables(data)[["obs_id_colname"]]]
		vexcludeCols <- paste0(unused_cols,collapse=",")
    }
	
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

	deeptable <- paste0(deepx@select@database,".",deepx@select@table_name)

	##Get Mapping Information for specID
	sqlstr <- paste0("SELECT ")

    sqlstr <- paste0("CALL FLLinRegr(",fquote(deeptable),",",
					 				fquote(getVariables(deepx)[["obs_id_colname"]]),",",
					 				fquote(getVariables(deepx)[["var_id_colname"]]),",",
					 				fquote(getVariables(deepx)[["cell_val_colname"]]),
					 				",'LinRegr from AdapteR',AnalysisID );")
	
	retobj <- sqlQuery(connection,sqlstr)
	retobj <- checkSqlQueryOutput(retobj)
	AnalysisID <- as.character(retobj[1,1])
	
	return(new("FLLinRegr",
				formula=formula,
				AnalysisID=AnalysisID,
				wideToDeepAnalysisId=wideToDeepAnalysisId,
				table=data,
				results=list(call=vcallObject),
				deeptable=deepx,
				mapTable=mapTable,
				scoreTable=""))
}

`$.FLLinRegr`<-function(object,property){
	#parentObject <- deparse(substitute(object))
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]
	if(property=="coefficients"){
		coefficientsvector <- coefficients.FLLinRegr(object)
		assign(parentObject,object,envir=parent.frame())
		return(coefficientsvector)
	}
	else if (property=="residuals"){
		residualsvector <- residuals.FLLinRegr(object)
		assign(parentObject,object,envir=parent.frame())
		return(residualsvector)
	}
	else if(property=="fitted.values")
	{
		fitvector <- fitted.values.FLGAM(object)
		assign(parentObject,object,envir=parent.frame())
		return(fitvector)
	}
	else if(property=="FLCoeffStdErr")
	{
		coeffVector <- coefficients.FLLinRegr(object)
		assign(parentObject,object,envir=parent.frame())
		return(object@results[["FLCoeffStdErr"]])
	}
	else if(property=="FLCoeffTStat")
	{
		coeffVector <- coefficients.FLLinRegr(object)
		assign(parentObject,object,envir=parent.frame())
		return(object@results[["FLCoeffTStat"]])
	}
	else if(property=="FLCoeffPValue")
	{
		coeffVector <- coefficients.FLLinRegr(object)
		assign(parentObject,object,envir=parent.frame())
		return(object@results[["FLCoeffPValue"]])
	}
	else if(property=="FLCoeffNonZeroDensity")
	{
		coeffVector <- coefficients.FLLinRegr(object)
		assign(parentObject,object,envir=parent.frame())
		return(object@results[["FLCoeffNonZeroDensity"]])
	}
	else if(property=="FLCoeffCorrelWithRes")
	{
		coeffVector <- coefficients.FLLinRegr(object)
		assign(parentObject,object,envir=parent.frame())
		return(object@results[["FLCoeffCorrelWithRes"]])
	}
	else if(property=="call")
	{
		return(object@results[["call"]])
	}
	else if(property=="FLLinRegrStats")
	{
		if(!is.null(object@results[["FLLinRegrStats"]]))
		return(object@results[["FLLinRegrStats"]])
		else
		{
			sqlstr <- paste0("SELECT * FROM fzzlLinRegrStats",
									" WHERE AnalysisID=",fquote(object@AnalysisID))

			statsdataframe <- sqlQuery(getOption("connectionFL"),sqlstr)
			object@results <- c(object@results,list(FLLinRegrStats=statsdataframe))
			assign(parentObject,object,envir=parent.frame())
			return(statsdataframe)
		}
	}
	else if(property=="df.residual")
	{
		statsdataframe <- object$FLLinRegrStats
		colnames(statsdataframe) <- toupper(colnames(statsdataframe))
		dfResidualVector <- statsdataframe[["DFRESIDUAL"]]
		object@results <- c(object@results,list(df.residual=dfResidualVector))
		assign(parentObject,object,envir=parent.frame())
		return(dfResidualVector)
	}
	else if(property=="model")
	{
		## The Column order may not be same as
		## in formula object because add. columns
		## may be added by categorical trans.

		# if(!is.null(object@results[["model"]]))
		# return(object@results[["model"]])
		# else
		# {
			##This might stop any parent script!!
			##Need something that has wait time and
			## Default value.
			modelframe <- model.FLLinRegr(object)
			## Do not store. Better to fetch each time as
			## it saves memory and not much time loss in
			## Fetching.
			##object@results <- c(object@results,list(model=modelframe))
			assign(parentObject,object,envir=parent.frame())
			return(modelframe)
		# }
	}
	else if(property=="x")
	{
		# if(!is.null(object@results[["x"]]))
		# return(object@results[["x"]])
		# else
		# {
			modelframe <- model.FLLinRegr(object)
			modelframe[[1]] <- 1
			colnames(modelframe)[1] <- "Intercept"
			## Do not store. Better to fetch each time as
			## it saves memory and not much time loss in
			## Fetching.
			# object@results <- c(object@results,list(x=modelframe))
			assign(parentObject,object,envir=parent.frame())
			return(modelframe)
		# }
	}
	else if(property=="y")
	{
		##This is safer from simple subsetting of
		## WideTable as whereConditions may exist
		if(!is.null(object@results[["y"]]))
		return(object@results[["y"]])
		else
		{
			vtablename <- paste0(object@deeptable@select@database,".",
							object@deeptable@select@table_name)
			obs_id_colname <- getVariables(object@deeptable)[["obs_id_colname"]]
			var_id_colname <- getVariables(object@deeptable)[["var_id_colname"]]
			cell_val_colname <- getVariables(object@deeptable)[["cell_val_colname"]]

			sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,",
								obs_id_colname," AS vectorIndexColumn,",
								cell_val_colname," AS vectorValueColumn",
							" FROM ",vtablename,
							" WHERE ",var_id_colname," = -1 ")

			tblfunqueryobj <- new("FLTableFunctionQuery",
	                        connection = getOption("connectionFL"),
	                        variables = list(
				                obs_id_colname = "vectorIndexColumn",
				                cell_val_colname = "vectorValueColumn"),
	                        whereconditions="",
	                        order = "",
	                        SQLquery=sqlstr)

			yvector <- new("FLVector",
							select = tblfunqueryobj,
							dimnames = list(object@deeptable@dimnames[[1]],
											"vectorValueColumn"),
							isDeep = FALSE)
			object@results <- c(object@results,list(y=yvector))
			assign(parentObject,object,envir=parent.frame())
			return(yvector)
		}
	}
	else if(property=="qr" || property=="rank")
	{
		if(!is.null(object@results[["qr"]]))
		{
			if(property=="qr")
			return(object@results[["qr"]])
			else if(property=="rank") 
			return(object@results[["qr"]]$rank)
		}
		else
		{
			modelmatrix <- as.matrix(object$x)
			qrList <- base::qr(modelmatrix)
			vrank <- qrList$rank
			# object@results <- c(object@results,
			# 					list(rank=vrank))
			assign(parentObject,object,envir=parent.frame())
			if(property=="qr")
			return(qrList)
			else if(property=="rank") return(vrank)
		}
	}
	else if(property=="terms")
	{
		return(terms(object@formula))
	}
	else if(property=="xlevels")
	{
		cat("categorical variables are Transformed")
		return(list())
	}
	else if(property=="assign")
	{
		return(c(0,rep(1,length(all.vars(object@formula))-1)))
	}
	else stop("That's not a valid property")
}

coefficients<-function(table){
	UseMethod("coefficients",table)
}

coefficients.default <- stats::coefficients
coefficients.FLLinRegr<-function(object){
	if(!is.null(object@results[["coefficients"]]))
	return(object@results[["coefficients"]])
	else
	{
		##Since Currently only 1000 Columns are supported
		## by FLLinRegr, fetch them.
		if(object@table@isDeep)
		coeffVector <- sqlQuery(getOption("connectionFL"),
			paste0("SELECT * FROM fzzlLinRegrCoeffs where AnalysisID=",fquote(object@AnalysisID),
					" ORDER BY CoeffID"))
		else
		coeffVector <- sqlQuery(getOption("connectionFL"),
			paste0("SELECT CASE WHEN a.Catvalue IS NOT NULL THEN ",
					"a.COLUMN_NAME || a.Catvalue ELSE ",
					"a.Column_name END AS CoeffName,b.* ",
				   " FROM fzzlRegrDataPrepMap AS a,fzzlLinRegrCoeffs AS b",
				   " WHERE a.Final_VarID = b.CoeffID ",
					" AND a.AnalysisID = ",fquote(object@wideToDeepAnalysisId),
					" AND b.AnalysisID = ",fquote(object@AnalysisID),
					" ORDER BY CoeffID"))

		colnames(coeffVector) <- toupper(colnames(coeffVector))
		stderrVector <- coeffVector[["STDERR"]]
		tstatVector <- coeffVector[["TSTAT"]]
		pvalVector <- coeffVector[["PVALUE"]]
		coeffVector1 <- coeffVector[["COEFFVALUE"]]
		nonzeroDVector <- coeffVector[["NONZERODENSITY"]]
		corwithresvector <- coeffVector[["CORRELWITHRES"]]

		if(!is.null(coeffVector[["COEFFNAME"]]))
		names(coeffVector1) <- coeffVector[["COEFFNAME"]]
		else{
			vallVars <- all.vars(genDeepFormula(coeffVector[["COEFFID"]]))
			names(coeffVector1) <- c("Intercept",vallVars[2:length(vallVars)])
		}
		

		object@results <- c(object@results,list(coefficients=coeffVector1,
												FLCoeffStdErr=stderrVector,
												FLCoeffTStat=tstatVector,
												FLCoeffPValue=pvalVector,
												FLCoeffNonZeroDensity=nonzeroDVector,
												FLCoeffCorrelWithRes=corwithresvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(coeffVector1)
	}
}

residuals.FLLinRegr<-function(object)
{
	if(!is.null(object@results[["residuals"]]))
	return(object@results[["residuals"]])
	else
	{
		
		if(object@scoreTable==""){
		object@scoreTable <- paste0(getOption("ResultDatabaseFL"),".",
			gen_score_table_name(object@table@select@table_name))
		fitted.valuesVector <- predict(object,object@table,scoreTable=object@scoreTable)
		object@results <- c(object@results,list(fitted.values=fitted.valuesVector))
		}
		# vtablename <- paste0(object@table@select@database,".",object@table@select@table_name)
		# obs_id_colname <- getVariables(object@table)[["obs_id_colname"]]

		# y <- "fPred"
		# vobsid <- "ObsID"
		
    	vYVector <- object$y
		residualsvector <- vYVector - object@results[["fitted.values"]]
		# sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,",
		# 					object@scoreTable,".",vobsid," AS vectorIndexColumn,",
		# 					vtablename,".",all.vars(object@formula)[1]," - ",
		# 					object@scoreTable,".",y," AS vectorValueColumn",
		# 				" FROM ",object@scoreTable,",",vtablename,
		# 				" WHERE ",vtablename,".",obs_id_colname," = ",
		# 							object@scoreTable,".",vobsid)

		# tblfunqueryobj <- new("FLTableFunctionQuery",
  #                       connection = getOption("connectionFL"),
  #                       variables = list(
		# 	                obs_id_colname = "vectorIndexColumn",
		# 	                cell_val_colname = "vectorValueColumn"),
  #                       whereconditions="",
  #                       order = "",
  #                       SQLquery=sqlstr)

		# residualsvector <- new("FLVector",
		# 						select = tblfunqueryobj,
		# 						dimnames = list(rownames(object@table),
		# 										"vectorValueColumn"),
		# 						isDeep = FALSE)

		object@results <- c(object@results,list(residuals=residualsvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(residualsvector)
	}
}

model.FLLinRegr <- function(object)
{
	if(!is.null(object@results[["model"]]))
	return(object@results[["model"]])
	else
	{
		if(interactive())
		{
			vinput <- readline("Fetching entire table. Continue? y/n ")
			if(!checkYorN(vinput)) return(NULL)
		}
		modelframe <- as.data.frame(object@deeptable)
		modelframe[[2]] <- NULL ##Intercept
		coeffVector <- object$coefficients
		vallVars <- all.vars(object@formula)
		vcolnames <- c(vallVars[1],names(coeffVector)[2:length(coeffVector)])
		colnames(modelframe) <- vcolnames
		object@results <- c(object@results,list(model=modelframe))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(modelframe)
	}
}
summary.FLLinRegr<-function(object){
	ret <- object$FLLinRegrStats
	colnames(ret) <- toupper(colnames(ret))
	vresiduals <- object$residuals
	sqlstr <- paste0("WITH z (id,val)",
						" AS(SELECT 1,",
						 		"a.vectorValueColumn AS deviation",
						 	" FROM (",constructSelect(vresiduals),") AS a) ", 
					" SELECT FLMin(z.val),FLMax(z.val),q.*",
							" FROM (SELECT a.oPercVal as perc
							 	   FROM TABLE (FLPercUdt(z.id, z.val, 0.25) 
							 	   HASH BY z.id
							 	   LOCAL ORDER BY z.id) AS a) AS q,z
								   group by 3")
	vresult1 <- sqlQuery(getOption("connectionFL"),sqlstr)
	sqlstr <- paste0("WITH z (id,val)",
						" AS(SELECT 1,",
						 		"a.vectorValueColumn AS deviation",
						 	" FROM (",constructSelect(vresiduals),") AS a) ", 
					" SELECT q.*",
							" FROM (SELECT a.oPercVal as perc
							 	   FROM TABLE (FLPercUdt(z.id, z.val, 0.50) 
							 	   HASH BY z.id
							 	   LOCAL ORDER BY z.id) AS a) AS q")
	vresult2 <- sqlQuery(getOption("connectionFL"),sqlstr)
	sqlstr <- paste0("WITH z (id,val)",
						" AS(SELECT 1,",
						 		"a.vectorValueColumn AS deviation",
						 	" FROM (",constructSelect(vresiduals),") AS a) ", 
					" SELECT q.*",
							" FROM (SELECT a.oPercVal as perc
							 	   FROM TABLE (FLPercUdt(z.id, z.val, 0.75) 
							 	   HASH BY z.id
							 	   LOCAL ORDER BY z.id) AS a) AS q")
	vresult3 <- sqlQuery(getOption("connectionFL"),sqlstr)
	coeffframe <- data.frame(object$coefficients,
							object$FLCoeffStdErr,
							object$FLCoeffTStat,
							object$FLCoeffPValue)
	colnames(coeffframe)<-c("Estimate","Std. Error","t value","Pr(>|t|)")

	residualframe <- data.frame(vresult1[[1]],
								vresult1[[3]],
								vresult2[[1]],
								vresult3[[1]],
								vresult1[[2]])
	colnames(residualframe) <- c("Min","1Q","Median","3Q","Max")
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	assign(parentObject,object,envir=parent.frame())
	
	cat("Call:\n")
	cat(paste0(object$call),"\n")
	cat("\nResiduals:\n")
	print(residualframe)
	cat("\n\nCoefficients:\n")
	print(coeffframe)
	cat("\n---\n")
	cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 '' 1\n")
	cat("Residual standard error: ",ret[["MSRESIDUAL"]]," on ",ret[["DFRESIDUAL"]]," degrees of freedom\n\n")
	cat("Multiple R-squared: ",ret[["RSQUARED"]]," , Adjusted R-squared: ",ret[["ADJRSQUARED"]],"\n")
	FStatPVal<-pf(ret[["FSTAT"]],ret[["DFREGRESSION"]],ret[["DFRESIDUAL"]],lower.tail=FALSE)
	cat("F-statistic: ",ret[["FSTAT"]]," on ",ret[["DFREGRESSION"]]," and ",ret[["DFRESIDUAL"]]
		,"DF , p-value: ",FStatPVal,"\n")

}

predict<-function(object,newdata,...){
	UseMethod("predict",object)
}

predict.FLLinRegr<-function(object,
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
	}
	vtable <- paste0(newdata@select@database,".",newdata@select@table_name)
	vobsid <- getVariables(newdata)[["obs_id_colname"]]
	vvarid <- getVariables(newdata)[["var_id_colname"]]
	vvalue <- getVariables(newdata)[["cell_val_colname"]]
	sqlstr <- paste0("CALL FLLinRegrScore (",fquote(newdata@select@table_name),",",
											 fquote(vobsid),",",
											 fquote(vvarid),",",
											 fquote(vvalue),",",
											 "NULL,",
											 fquote(object@AnalysisID),",",
											 fquote(scoreTable),",",
											 "'Scoring using model ",object@AnalysisID,"',",
											 "oAnalysisID);")

	AnalysisID <- sqlQuery(getOption("connectionFL"),
								sqlstr)
	AnalysisID <- checkSqlQueryOutput(AnalysisID)

	sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
						vobsid," AS vectorIndexColumn,",
						"Y AS vectorValueColumn",
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

print.FLLinRegr<-function(object){
	reqList <- list(call=object$call,
					coefficients=object$coefficients)

	class(reqList) <- "lm"
	print(reqList)
}

#overloading show.
setMethod("show","FLLinRegr",print.FLLinRegr)

plot.FLLinRegr <- function(object)
{
	reqList <- list(residuals=as.vector(object$residuals),
					coefficients=object$coefficients,
					df.residual=object$df.residual,
					qr=object$qr,
					rank=object$rank,
					call=object$call,
					xlevels=object$xlevels,
					model=object$model,
					terms=object$terms)
	class(reqList) <- "lm"

	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	assign(parentObject,object,envir=parent.frame())
	plot(reqList)
}

influence.FLLinRegr <- function(model,...){
	reqList <- list(residuals=as.vector(model$residuals),
					coefficients=model$coefficients,
					df.residual=model$df.residual,
					qr=model$qr,
					rank=model$rank,
					call=model$call,
					xlevels=model$xlevels,
					model=model$model,
					terms=model$terms)
	class(reqList) <- "lm"
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	assign(parentObject,model,envir=parent.frame())
	return(stats::influence(reqList,...))
}

lm.influence <- function(model,do.coef=TRUE,...){
	UseMethod("lm.influence",model)
}

lm.influence.default <- stats::lm.influence

lm.influence.FLLinRegr <- function(model,do.coef=TRUE,...){
	reqList <- list(residuals=as.vector(model$residuals),
					coefficients=model$coefficients,
					df.residual=model$df.residual,
					qr=model$qr,
					rank=model$rank,
					call=model$call,
					xlevels=model$xlevels,
					model=model$model,
					terms=model$terms)
	class(reqList) <- "lm"
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	assign(parentObject,model,envir=parent.frame())
	return(stats::lm.influence(reqList,do.coef,...))
}

genDeepFormula <- function(pColnames)
{
	options(warn=-1)
	if(any(is.na(as.numeric(pColnames))))
	{
		options(warn=0)
		stop("varID column must be numeric\n")
	}
	options(warn=0)
	vcolnames <- as.numeric(pColnames)
	# if(!(-1 %in% vcolnames))
	# stop("-1 denoting dependent column must be present in colnames of deep table.\n")
	vcolnames <- paste0("var",vcolnames[!vcolnames %in% c(0,-1)],collapse="+")
	vformula <- paste0("varY~",vcolnames)
	return(as.formula(vformula))
}