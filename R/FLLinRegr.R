#' @include utilities.R
#' @include data_prep.R
#' @include FLTable.R
NULL
#' An S4 class to represent FLLinRegr
#'
#' @slot formula an object of class 'formula': Model Formula
#' @slot deeptable A character vector containing 
#' the deeptable on conversion from a widetable
#' @slot AnalysisID An output character ID from CALL FLLinRegr
#' @slot wideToDeepAnalysisID An output character ID from FLRegrDataPrep
#' @slot mapTable name of the mapping table
#' @slot scoreTable name of the scoring table
#' @slot modelID id of the model with best fit
#' @slot table input FLTable object
#' @slot results cache list of results computed
#' @method print FLLinRegr
#' @method coefficients FLLinRegr
#' @method residuals FLLinRegr
#' @method influence FLLinRegr
#' @method lm.influence FLLinRegr
#' @method plot FLLinRegr
#' @method summary FLLinRegr
#' @method predict FLLinRegr
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
#' The anova method is not yet available for FLLinRegr
#' @return \code{lm} performs linear regression and replicates equivalent R output.
#' @examples
#' library(RODBC)
#' connection <- flConnect("Gandalf")
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

#' Choose a model.
#'
#' \code{steps} performs linear regression on FLTable objects.
#' Choose a formula based model by p-values and R-Squared Values.
#'
#' @param object An object of class FLTable
#' @param scope A symbolic description of model to be fitted.
#' \code{scope} can be a list with upper and lower components
#' or a formula. For a widetable, upper and lower should be formulas
#' describing the range of models. If a formula is given instead of list
#' it will be treated as upper. For a deeptable, upper and lower should
#' be vectors with variable ids'.Provide empty list for deeptable if 
#' nothing is to be specified.
#' @param scale currently not used.
#' @param direction character.Must be one of backward,
#' Fbackward,UFbackward,forward.
#' @param trace if positive, information is printed out during the
#' running of the steps.
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
#' @param highestpAllow1 All the variables whose p-value exceed the value
#' specified by HighestpAllow1 are dropped in one go. 
#' Typical value for HighestProbAllow1 could be 0.50. Must be >0 and < 1.
#' Not applicable for forward.
#' @param highestpAllow2 Only one variable is dropped at a time
#' till all the p-Values are below the HighestpAllow2.
#' Typical value could be 0.10. Must be >0 and < 1.
#' Not applicable for forward and backward.
#' @param stepWiseDecrease The StepwiseDecrease is used to
#' decrease the p-Value at each stage. In first step, 
#' all variables having pValue exceeding HighestpValue1 are
#' dropped. Then the HighestpValue1 is
#' reduced by StepwiseDecreasepValue
#' and the process is repeated until all
#' the variables have p-value less than HighestpValue2.
#' Must be >0 and <1. Used only for UFbackward.
#' @section Constraints:
#' The anova method is not yet available for FLLinRegr.
#' @return \code{step} performs linear regression and replicates equivalent R output.
#' @examples
#' library(RODBC)
#' connection <- flConnect("Gandalf")
#' widetable  <- FLTable("FL_DEMO", "tblAbaloneWide", "ObsID")
#' s <- step(widetable,scope=list(lower=Rings~Height+Diameter),direction = "UFbackward")
#' plot(s)
#' s$coefficients
#' s <- step(widetable,
#' 			scope=list(lower=Rings~Height+Diameter,
#'  					upper=Rings~Height+Diameter+Sex+Num_Length),
#' 			direction = "UFbackward")
#' plot(s)
#' s$coefficients
#' s <- step(widetable,
#' 			scope=list(upper=Rings~Height+Diameter+Sex+Num_Length+DummyCat),
#'  		direction = "Fbackward")
#' plot(s)
#' s$coefficients
#' s <- step(widetable,
#' 			scope=Rings~Height+Diameter+Sex+Num_Length+DummyCat,
#'  		direction = "forward")
#' plot(s)
#' s$coefficients
#' s <- step(widetable,
#' 			scope=Rings~Height+Diameter+Sex+Num_Length+DummyCat,
#'  		direction = "Fbackward")
#' plot(s)
#' s$coefficients
#' s <- step(widetable,
#' 			scope=list(upper=Rings~Height+Diameter+Sex+Num_Length+DummyCat),
#'  		direction = "forward")
#' plot(s)
#' s$coefficients
#' deeptable <- FLTable("FL_DEMO","myLinRegrSmall","ObsID","VarID","Num_Val")
#' s <- step(deeptable,
#' 			scope=list(upper=c("-1","0","1")),
#'  		direction = "backward")
#' s <- step(deeptable,
#' 			scope=list(upper=c("1","2"),lower=c("1")),
#'  		direction = "Fbackward")
#' s <- step(deeptable,
#' 			scope=list(lower=c("2")),
#'  		direction = "UFbackward")
#' s <- step(deeptable,
#' 			scope=list(),
#'  		direction = "forward")
#' @export
step <- function (object,scope,...) {
	UseMethod("step", object)
 }

#' @export
step.default <- stats::step

#' @export
step.FLTable <- function(object, scope, scale = 0,
     				direction = "forward",
     				trace = 1,catToDummy=0,
					performNorm=0,
					performVarReduc=0,
					makeDataSparse=0,
					minStdDev=0,
					maxCorrel=1,
					classSpec=list(),
					whereconditions="",
					highestpAllow1=0.5,
					highestpAllow2=0.1,
					stepWiseDecrease=0.05,...){

	if (!direction %in% c("forward","Fbackward","backward","UFbackward"))
	stop("direction must be in c(forward,Fbackward,backward,UFbackward)")
	if(!is.list(scope) && !class(scope)=="formula")
	stop("scope argument must be a list or formula.\n",
		" empty list accepted for deeptable.\n")
	vupperformula <- ""
	if(class(scope)=="formula")
	{
		vupperformula <- scope
	}

	vinclude <- c()
	vexclude <- c()
	if(is.list(scope))
	{
		vlower <- scope[["lower"]]
		vupper <- scope[["upper"]]
		##If only lower is given. Upper includes all.
		if(is.null(vupper) && !is.null(vlower)){
			if(!object@isDeep){
				if(class(vlower)!="formula") stop("for wide table scope should have formula as components\n")
				vupperformula <- formula(paste0(all.vars(vlower)[1],"~",
									paste0(setdiff(colnames(object),
										c(all.vars(vlower)[1],
											getVariables(object)[["obs_id_colname"]])),
									collapse="+")))
				vinclude <- all.vars(vlower)[2:length(all.vars(vlower))]
			}
			else{
				if(!is.vector(vlower)) stop("for deep table scope should have vectors as components\n")
				vinclude <- vlower
			}
		}
		else if(is.null(vlower) && !is.null(vupper)){
			if(!object@isDeep){
				if(class(vupper)!="formula") stop("for wide table scope should have formula as components\n")
				vupperformula <- vupper
			}
			else{
				if(!is.vector(vupper)) stop("for deep table scope should have vectors as components\n")
				vexclude <- setdiff(colnames(object),vupper)
			}
		}
		else if(!is.null(vupper) && !is.null(vlower)){
			if(!object@isDeep){
				if(class(vupper)!="formula" || class(vlower)!="formula")
				stop("for wide table scope should have formula as components\n")
				vupperformula <- vupper
				vinclude <- all.vars(vlower)[2:length(all.vars(vlower))]
			}
			else{
				if(!is.vector(class(vupper)) || !is.vector(class(vlower)))
				stop("for deep table scope should have vectors as components\n")
				vinclude <- vlower
				vexclude <- setdiff(colnames(object),vupper)
			}	
		}
		else if(!object@isDeep) stop("scope cannot be empty list for widetable")
	}

	vinclude <- setdiff(vinclude,c("-1"))
	vexclude <- setdiff(vexclude,c("0","-1"))
	if(!length(vinclude)>0) vinclude <- NULL
	if(!length(vexclude)>0) vexclude <- NULL

	if(!is.null(vinclude) || !is.null(vexclude))
	specID <- list(include=vinclude,
					exclude=vexclude)
	else specID <- list()

	vcallObject <- match.call()
	return(lmGeneric(formula=vupperformula,
					data=object,
					catToDummy=catToDummy,
					performNorm=performNorm,
					performVarReduc=performVarReduc,
					makeDataSparse=makeDataSparse,
					minStdDev=minStdDev,
					maxCorrel=maxCorrel,
					classSpec=classSpec,
					whereconditions=whereconditions,
					direction=direction,
					specID=specID,
					trace=trace,
					highestpAllow1=highestpAllow1,
					highestpAllow2=highestpAllow2,
					stepWiseDecrease=stepWiseDecrease,
					callObject=vcallObject,
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
					specID=list(),
					highestpAllow1=0.5,
					highestpAllow2=0.1,
					stepWiseDecrease=0.05,
					topN=1,
					direction="",
					trace=1,
					callObject=NULL,
					...)
{
	if(data@isDeep){
		vallVars <- colnames(data)
		formula <- genDeepFormula(vallVars)
	}
	else{
		vallVars <- base::all.vars(formula)
		vdependent <- vallVars[1]
		vindependent <- vallVars[2:length(vallVars)]
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
    	pObject <- c(pObject[["include"]],pObject[["exclude"]])
    	if(length(pObject)>0)
    	{
    		sapply(pObject,function(x)
	        if(!(x %in% pAllVars))
	        stop(paste0(x,collapse=",")," specified in SpecID not in colnames of data\n"))
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
    	unused_cols <- vcolnames[!vcolnames %in% c(all.vars(formula),specID[["exclude"]])]
		unused_cols <- unused_cols[unused_cols!=getVariables(data)[["obs_id_colname"]]]
		vexcludeCols <- paste0(unused_cols,collapse=",")
    }
	
	vcallObject <- callObject
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

		##Get Mapping Information for specID
		vmapping <- sqlQuery(getOption("connectionFL"),
				paste0("SELECT a.Column_name AS colName,",
						" a.Final_VarID AS varID",
					   " FROM fzzlRegrDataPrepMap AS a",
					   " WHERE a.AnalysisID = ",fquote(wideToDeepAnalysisId),
						" ORDER BY a.Final_VarID"))

		vtemp <- vmapping[["varID"]]
		names(vtemp) <- vmapping[["colName"]]
		vmapping <- vtemp
		vallVars <- setdiff(vallVars,specID[["exclude"]])
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
		vmapping <- colnames(deepx)
		names(vmapping) <- colnames(deepx)
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
		vmapping <- colnames(deepx)
		names(vmapping) <- colnames(deepx)
	}

	deeptable <- paste0(deepx@select@database,".",deepx@select@table_name)
	vinclude <- NULL
	vexclude <- NULL
	##Insert SpecID
	vspecID <- "NULL"
	if(is.list(specID) && length(specID)>0 
		&& direction %in% c("UFbackward","Fbackward","backward"))
	{
		vspecID <- fquote(genRandVarName())
		sqlstr <- c()
		if(!is.null(specID[["include"]]))
		{
			vinclude <- vmapping[charmatch(specID[["include"]],names(vmapping))]
			vinclude <- vinclude[!is.na(vinclude)]
			sqlstr <- c(sqlstr,paste0("INSERT INTO ",getOption("ResultDatabaseFL"),
						".","fzzlLinRegrModelVarSpec VALUES(",vspecID,",",
							vinclude,",","'I')"))
		}
		if(!is.null(specID[["exclude"]]))
		{
			vexclude <- vmapping[charmatch(specID[["exclude"]],names(vmapping))]
			vexclude <- vexclude[!is.na(vexclude)]
			sqlstr <- c(sqlstr,paste0("INSERT INTO ",getOption("ResultDatabaseFL"),
						".","fzzlLinRegrModelVarSpec VALUES(",vspecID,",",
							vexclude,",","'X')"))
		}
		if(!is.null(sqlstr))
		t <- sqlSendUpdate(getOption("connectionFL"),paste0(sqlstr,collapse=";"))
	}
	
	vnote <- genNote("linregr")
	if(direction=="")
    sqlstr <- paste0("CALL FLLinRegr(",fquote(deeptable),",\n",
					 				fquote(getVariables(deepx)[["obs_id_colname"]]),",\n",
					 				fquote(getVariables(deepx)[["var_id_colname"]]),",\n",
					 				fquote(getVariables(deepx)[["cell_val_colname"]]),",\n",
					 				fquote(vnote),",\nAnalysisID );\n")
    else if(direction=="backward")
    sqlstr <- paste0("CALL FLLinRegrBW(",fquote(deeptable),",\n",
					 				fquote(getVariables(deepx)[["obs_id_colname"]]),",\n",
					 				fquote(getVariables(deepx)[["var_id_colname"]]),",\n",
					 				fquote(getVariables(deepx)[["cell_val_colname"]]),",\n",
					 				vspecID,",\n",
					 				highestpAllow1,",\n",
					 				fquote(vnote),",\nAnalysisID );\n")
    else if(direction=="Fbackward")
    sqlstr <- paste0("CALL FLLinRegrFB(",fquote(deeptable),",\n",
					 				fquote(getVariables(deepx)[["obs_id_colname"]]),",\n",
					 				fquote(getVariables(deepx)[["var_id_colname"]]),",\n",
					 				fquote(getVariables(deepx)[["cell_val_colname"]]),",\n",
					 				vspecID,",\n",
					 				highestpAllow1,",\n",
					 				highestpAllow2,",\n",
					 				fquote(vnote),",\nAnalysisID );\n")
    else if(direction=="UFbackward")
    sqlstr <- paste0("CALL FLLinRegrUFB(",fquote(deeptable),",\n",
					 				fquote(getVariables(deepx)[["obs_id_colname"]]),",\n",
					 				fquote(getVariables(deepx)[["var_id_colname"]]),",\n",
					 				fquote(getVariables(deepx)[["cell_val_colname"]]),",\n",
					 				vspecID,",\n",
					 				highestpAllow1,",\n",
					 				highestpAllow2,",\n",
					 				stepWiseDecrease,",\n",
					 				fquote(vnote),",\nAnalysisID );\n")
    else if(direction=="forward")
    sqlstr <- paste0("CALL FLLinRegrSW(",fquote(deeptable),",\n",
					 				fquote(getVariables(deepx)[["obs_id_colname"]]),",\n",
					 				fquote(getVariables(deepx)[["var_id_colname"]]),",\n",
					 				fquote(getVariables(deepx)[["cell_val_colname"]]),",\n",
					 				topN,",\n",
					 				highestpAllow1,",\n",
					 				fquote(vnote),",\nAnalysisID );\n")
	
	retobj <- sqlQuery(connection,sqlstr,
						genAnalysisIDQuery("fzzlLinRegrInfo",vnote))
	retobj <- checkSqlQueryOutput(retobj)
	AnalysisID <- as.character(retobj[1,1])

	##Find the max modelID to avoid joins later.
	##For forward find best fit model id.
	if(!direction %in% "forward"){
	vsqlstr <- paste0("SELECT max(ModelID) from fzzlLinRegrStats WHERE AnalysisID=",fquote(AnalysisID))
	vmaxModelID <- sqlQuery(getOption("connectionFL"),vsqlstr)[1,1]
	}
	
	if(trace>0 && !direction %in% c("","forward"))
	{
		vsqlstr <- paste0("SELECT a.coeffid,c.* \n",
						" FROM fzzllinregrcoeffs a,fzzllinregrstats c \n",
						" WHERE NOT EXISTS(SELECT 1 FROM fzzllinregrcoeffs b ",
							" WHERE b.analysisid=a.analysisid AND b.modelid=a.modelid+1 \n",
							" AND a.coeffid = b.coeffid)\n",
						" AND a.analysisid=",fquote(AnalysisID)," AND c.analysisid=a.analysisid \n",
						" AND a.modelid<>",vmaxModelID," AND c.modelid=a.modelid\n",
						" UNION ALL\n",
						" SELECT 0,a.* FROM fzzlLinRegrStats a \n",
						" WHERE a.AnalysisID=",fquote(AnalysisID),
						" AND a.ModelID=",vmaxModelID,"\n",
						" ORDER BY 3")
		d <- sqlQuery(getOption("connectionFL"),vsqlstr)
		colnames(d)<-toupper(colnames(d))
		d[["ANALYSISID"]] <- NULL
		vdroppedCols <- specID[["exclude"]]
		if(nrow(d)>1){
			for(i in unique(setdiff(d[["MODELID"]],vmaxModelID)))
			{
				cat("Step:    RSQUARED = ",d[d[,"MODELID"]==i,"RSQUARED"][1],"\n")
				vdropped <- as.numeric(d[d[,"MODELID"]==i,"COEFFID"])
				vcolnames <- names(vmapping)
				vdroppedCols1 <- sapply(vdropped,function(x) vcolnames[as.numeric(vmapping)==x])
				vdroppedCols <- c(vdroppedCols1,vdroppedCols)
				if(data@isDeep){
					vallVars <- all.vars(formula)
					vfr <- genDeepFormula(c(vdropped,vexclude))
					vdroppedCols <- all.vars(vfr)[-1]
				}
				cat(vallVars[1],"~",paste0(vallVars[!toupper(vallVars) %in% c(toupper(vdroppedCols)
					,toupper(vallVars[1]))],
					collapse=" + "),"\n")
				vdataframe <- rbind(d[d[,"MODELID"]==i,][1,],d[d[,"MODELID"]==i+1,][1,])
				rownames(vdataframe) <- c(" - None",paste0(" - ",paste0(vdroppedCols,collapse=" + ")))
				print(vdataframe[,!colnames(vdataframe) %in% c("COEFFID","BPSTAT","SIGBPSTAT")])
				cat("\n\n\n")
			}
		}
	}
	else if(direction %in% c("forward"))
	{
		vsqlstr <- paste0("SELECT TOP 1 a.*,b.maxPValue \n",
						  " FROM fzzlLinRegrStats a,( \n",
						  		" SELECT a.ModelID,",
						  		" MAX(a.PValue) AS maxPValue \n",
								" FROM fzzlLinRegrCoeffs a \n",
								" WHERE a.AnalysisID = ",fquote(AnalysisID),
								" GROUP BY a.ModelID) AS b \n",
								" WHERE b.ModelID = a.ModelID \n",
								" AND a.AnalysisID = ",fquote(AnalysisID),
								" AND b.MaxPValue < 0.10 \n",
								" ORDER BY 3 DESC, 2;\n")
		d <- sqlQuery(getOption("connectionFL"),vsqlstr)
		colnames(d) <- toupper(colnames(d))
		d[["ANALYSISID"]] <- NULL
		vmaxModelID <- d[["MODELID"]]
		if(trace>0) print(d)
	}
	return(new("FLLinRegr",
				formula=formula,
				AnalysisID=AnalysisID,
				wideToDeepAnalysisId=wideToDeepAnalysisId,
				table=data,
				results=list(call=vcallObject,
							 modelID=vmaxModelID),
				deeptable=deepx,
				mapTable=mapTable,
				scoreTable=""))
}

#' @export
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
			sqlstr <- paste0("SELECT * FROM fzzlLinRegrStats\n",
							" WHERE AnalysisID=",fquote(object@AnalysisID),
							" \nAND ModelID=",object@results[["modelID"]])

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

			sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,\n",
								obs_id_colname," AS vectorIndexColumn,\n",
								cell_val_colname," AS vectorValueColumn\n",
							" FROM ",vtablename,
							" \nWHERE ",var_id_colname," = -1 \n")

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
		if(!is.null(object@results[["terms"]]))
		return(object@results[["terms"]])
		else
		{
			coeffVector <- object$coefficients
			vallVars <- all.vars(object@formula)
			vcolnames <- names(coeffVector)[2:length(coeffVector)]
			vterms <- terms(formula(paste0(vallVars[1],"~",
				paste0(vcolnames,collapse="+"))))
			object@results <- c(object@results,list(terms=vterms))
			assign(parentObject,object,envir=parent.frame())
			return(vterms)
		}
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

#' @export
coefficients<-function(table){
	UseMethod("coefficients",table)
}
#' @export
coefficients.default <- stats::coefficients
#' @export
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
					" AND ModelID=",object@results[["modelID"]],
					" ORDER BY CoeffID"))
		else
		coeffVector <- sqlQuery(getOption("connectionFL"),
			paste0("SELECT CASE WHEN a.Catvalue IS NOT NULL THEN \n",
					"a.COLUMN_NAME || a.Catvalue ELSE \n",
					"a.Column_name END AS CoeffName,b.* \n",
				   " FROM fzzlRegrDataPrepMap AS a,fzzlLinRegrCoeffs AS b \n",
				   " WHERE a.Final_VarID = b.CoeffID \n",
					" AND a.AnalysisID = ",fquote(object@wideToDeepAnalysisId),
					"\n AND b.AnalysisID = ",fquote(object@AnalysisID),
					"\n AND b.ModelID = ",object@results[["modelID"]],
					"\n ORDER BY CoeffID"))

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
		
		vcolnames <- colnames(object@deeptable)
		droppedCols <- vcolnames[!vcolnames %in% c("-1",coeffVector[["COEFFID"]])]
		object@results <- c(object@results,list(coefficients=coeffVector1,
												FLCoeffStdErr=stderrVector,
												FLCoeffTStat=tstatVector,
												FLCoeffPValue=pvalVector,
												FLCoeffNonZeroDensity=nonzeroDVector,
												FLCoeffCorrelWithRes=corwithresvector,
												droppedCols=droppedCols))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(coeffVector1)
	}
}

#' @export
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
		vdroppedCols <- object@results[["droppedCols"]]
		for(i in vdroppedCols)
		modelframe[[paste0(i)]] <- NULL
		vallVars <- all.vars(object@formula)
		vcolnames <- c(vallVars[1],names(coeffVector)[2:length(coeffVector)])
		colnames(modelframe) <- vcolnames
		object@results <- c(object@results,list(model=modelframe))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(modelframe)
	}
}

#' @export
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

#' @export
predict<-function(object,newdata,...){
	UseMethod("predict",object)
}

#' @export
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
	sqlstr <- paste0("CALL FLLinRegrScore (",fquote(newdata@select@table_name),",\n",
											 fquote(vobsid),",\n",
											 fquote(vvarid),",\n",
											 fquote(vvalue),",\n",
											 "NULL,\n",
											 fquote(object@AnalysisID),",\n",
											 fquote(scoreTable),",\n",
											 fquote(genNote("Scoring linregr")),
											 ",\noAnalysisID);")

	AnalysisID <- sqlQuery(getOption("connectionFL"),
								sqlstr,
								genAnalysisIDQuery("fzzlLinRegrInfo",genNote("Scoring linregr")))
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

#' @export
print.FLLinRegr<-function(object){
	reqList <- list(call=object$call,
					coefficients=object$coefficients)

	class(reqList) <- "lm"
	print(reqList)
}

#overloading show.
#' @export
setMethod("show","FLLinRegr",print.FLLinRegr)

#' @export
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

#' @export
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

#' @export
lm.influence <- function(model,do.coef=TRUE,...){
	UseMethod("lm.influence",model)
}
#' @export
lm.influence.default <- stats::lm.influence
#' @export
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