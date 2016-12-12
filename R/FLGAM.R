#' @include FLTable.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include utilities.R
NULL
setOldClass("family")
#' An S4 class to represent output from gam on in-database objects
#'
#' @slot offset column name used as offset
#' @slot specid unique ID from table where specifications are stored
#' @slot family information about family of fit
#' @method print FLGAM
#' @method coefficients FLGAM
#' @method residuals FLGAM
#' @method plot FLGAM
#' @method summary FLGAM
#' @method predict FLGAM
#' @export
setClass(
    "FLGAM",
    contains="FLRegr",
    slots=list(specid = "character",
                offset="character",
                family="family"))

#' Generalized Additive Models
#'
#' \code{gam} Fits a generalized additive model
#'  to data given by FLTable object.
#'
#' @param data an object of class FLTable, must be a wide table
#' @param formula An R formula object with smooth terms \code{s},
#' \code{te}. In these smooth terms, \code{m} and \code{k} are specified which
#' represent splineDegree and number of knots respectively.In
#' addition, NumOfPredPts is specified through \code{xt} and byVarName
#' through \code{by}.One Can club Spline terms with same set of
#' specifications by passing more than one variables/colnames in \code{s}. 
#' @param family currently only stats::poisson is supported
#' @param offset column name indicating the offset if any
#' @param maxiter maximum num of iterations
#' @param ... other arguments as in mgcv::gam may be passed
#' but not currently used
#' @section Constraints:
#' Plotting is not available. The result Set may not include all
#' results and methods as in mgcv::gam.
#' @return \code{gam} returns \code{FLGAM} object
#' @examples
#' widetable <- FLTable("tblGAMSimData","ObsID")
#' myformula <- yVal~x0Val+s(x1Val,m=3,k=10)+te(x1Val,x2Val,m=3,k=5)+s(x2Val,x1Val)
#' gamobject <- gam(myformula,data=widetable,offset="x2Val")
#' predictedValues <- predict(gamobject,widetable)
#' gamobject$coefficients
#' gamobject$fitted.values
#' gamobject$residuals
#' gamobject$df.null
#' print(gamobject)
#' @export
setGeneric("gam",
	function(formula,
			family=stats::gaussian,
			data,...)
	standardGeneric("gam"))

setMethod("gam",
	signature(formula="formula",
		data="missing"),
	function(formula,family=stats::gaussian(),data=data,...){
        if (!requireNamespace("mgcv", quietly = TRUE)){
            stop("mgcv package needed for gam. Please install it.",
            call. = FALSE)
        }
        else return(mgcv::gam(formula=formula,
                              family=family,
                              data=data,
                              ...))
    })

setMethod("gam",
    signature(formula="formula",
        data="ANY"),
    function(formula,family=stats::gaussian(),data=data,...){
        if (!requireNamespace("mgcv", quietly = TRUE)){
            stop("mgcv package needed for gam. Please install it.",
            call. = FALSE)
        }
        else return(mgcv::gam(formula=formula,
                              family=family,
                              data=data,
                              ...))
    })

# setMethod("gam",
# 	signature(formula="formula",
# 		data="data.frame"),
# 	function(formula,family=stats::gaussian(),data,...)
# 	mgcv::gam(formula,family,data,...))

setMethod("gam",
	signature(formula="formula",
		data="FLTable"),
	function(formula,family=stats::poisson,
		data,offset=NULL,
		model=TRUE,maxiter=500,...){
        if (!requireNamespace("mgcv", quietly = TRUE)){
            stop("mgcv package needed for gam. Please install it.",
            call. = FALSE)
        }
        else return(gam.FLTable(formula=formula,
        family=stats::poisson,
        data=data,
        offset=offset,
        model=model,
        maxiter=maxiter,
        ...))
    })
	

gam.FLTable <- function(formula,family=stats::poisson,
						data,offset=NULL,
						maxiter=500,...)
{
	#browser()
	require("mgcv")
	data <- setAlias(data,"")
	if(is.character(family) && base::toupper(family)!="POISSON")
	stop("only poisson family is supported currently\n")
	if(is.function(family) && !base::identical(family,stats::poisson))
	stop("only poisson family is supported currently\n")
	family <- stats::poisson()

	vallVars <- base::all.vars(formula)
	vcolnames <- colnames(data)

	sapply(vallVars,function(x)
		if(!(x %in% vcolnames))
		stop(x," not in colnames of data\n"))

	if(is.null(offset)) offset <- offsetCopy <- "NULL"
	else if(!(offset%in%vcolnames)) stop(offset," not in colnames of data\n")
	else{
		offsetCopy <- offset
		offset <- offset
	}
	if(!is.numeric(maxiter) || any(maxiter<1))
	stop("maxiter should be numeric and >= 1\n")
	else maxiter <- as.integer(base::max(maxiter))

	argList  <- as.list(environment())

	typeList <- list(offset="character",
					maxiter="integer"
					)
	validate_args(argList, typeList)

	callObject <- match.call()
	pTerms <- c()

	## Needed for eval to work. Rollback to initial values if already exist
	## has to be implemented.
	vrand <- genRandVarName()
	getprev <- function(objectvector,vrand)
	{
		vlist <- base::mget(objectvector,envir=.GlobalEnv,ifnotfound=vrand)
		return(sapply(vlist,function(x)x))
	}
	vprev <- getprev(vallVars,vrand)
	sapply(vallVars,function(x) base::assign(x,rnorm(10),pos=.GlobalEnv))
	tryCatch(vmodelList <- base::eval(attr(stats::terms(formula),"variables")),
		error=function(e) stop("formula may be incorrect. Error is:",e,"\n"))

	restoreprev <- function(objectvector,vrand)
	{
		options(warn=-1)
		for(i in names(objectvector))
		{
			#browser()
			if(objectvector[i]!=vrand)
			assign(i,objectvector[i],pos=.GlobalEnv)
			else
			rm(list=i,envir=.GlobalEnv)
		}
		options(warn=0)
	}
	vtemp <- restoreprev(vprev,vrand)

	vspecid <- genRandVarName()
	vgamParams <- data.frame(specid=vspecid,termid=1,
							varname=1,splinetype=1,
							splinedegree=1,numofknots=1,
							byvarname=1,smoothparam=1,
							smoothdegree=1,numofpredpts=1)

	vtermid <- 1
	for(i in 2:length(vmodelList))
	{
		vattributes <- vmodelList[[i]]

		## te fit
		if(class(vattributes)=="tensor.smooth.spec")
		{
			vattributes <- vattributes$margin
			for(j in 1:length(vattributes))
			{
				vtempattr <- vattributes[[j]]
				vNumOfPredPts <- checkNumOfPredPts(vtempattr$xt[1])
				vsplineDegree <- checkSplineDegree(vtempattr$p.order[1])
				vNumOfKnots <- checkNumOfKnots(vtempattr$bs.dim[1],vsplineDegree)
				vByVarName <- checkByVarName(vtempattr$by[1],colnames(data))
				if(vByVarName!="NULL")
				cat("currently byVarName not supported for tensor.Setting NULL")
				
				d <- data.frame(fquote(vspecid),vtermid,
								fquote(vtempattr$term[1]),fquote("bs"),
								vsplineDegree,vNumOfKnots,
								"NULL",0,
								0,vNumOfPredPts)
				colnames(d) <- colnames(vgamParams)
				vgamParams <- base::rbind(vgamParams,d)
			}
			vtermid <- vtermid + 1
		}
		## s fit
		else if(class(vattributes)=="tp.smooth.spec")
		{
			for(j in 1:length(vattributes$term))
			{
				vtempattr <- vattributes
				vNumOfPredPts <- checkNumOfPredPts(vtempattr$xt[1])
				vsplineDegree <- checkSplineDegree(vtempattr$p.order[1])
				vNumOfKnots <- checkNumOfKnots(vtempattr$bs.dim[1],vsplineDegree)
				vByVarName <- checkByVarName(vtempattr$by[1],colnames(data))
				if(vByVarName!="NULL") vByVarName <- fquote(vByVarName)
				
				d <- data.frame(fquote(vspecid),vtermid,
								fquote(vtempattr$term[j]),fquote("bs"),
								vsplineDegree,vNumOfKnots,
								vByVarName,0,
								0,vNumOfPredPts)
				colnames(d) <- colnames(vgamParams)
				vgamParams <- base::rbind(vgamParams,d)
				vtermid <- vtermid + 1
			}
		}

		##no-fit
		else if(as.character(attr(terms(formula),"variables")[i+1]) %in% vcolnames)
		{
			
			d <- data.frame(fquote(vspecid),vtermid,
						fquote(as.character(attr(terms(formula),"variables")[i+1])),fquote("na"),
						"NULL","NULL",
						"NULL","NULL",
						"NULL",20)
			vtermid <- vtermid + 1
			colnames(d) <- colnames(vgamParams)
			vgamParams <- base::rbind(vgamParams,d)
			pTerms <- c(pTerms,as.character(attr(terms(formula),"variables")[i+1]))
		}
		else stop("Use mgcv::s and mgcv::te for spline specification in formula\n")
	}
	vgamParams <- vgamParams[-1,]

	##pTerms as in R output
	if(length(pTerms)==0) pTerms<-1
	vformula <- paste0(vallVars[1],"~",
						paste0(pTerms,collapse="+"))
	pTerms <- terms(formula(vformula))
	## Storing FLGAMParams
	#return(vgamParams)
	# vsqlstr <- base::apply(vgamParams,1,function(x){
	# 	paste0(" INSERT INTO ",getOption("ResultDatabaseFL"),".fzzlGAMParams VALUES(",
	# 		paste0(x,collapse=","),");")})
	# vsqlstr <- paste0(vsqlstr,collapse="\n")

	# sqlSendUpdate(getFLConnection(),vsqlstr)
    insertIntotbl(pTableName="fzzlGAMParams",
                pValues=vgamParams)
	
	vresult <- sqlStoredProc(getFLConnection(),
							"FLGAM",
							outputParameter=c(AnalysisID="a"),
							TableName=data@select@table_name,
							ObsID=getVariables(data)[["obs_id_colname"]],
							DepVar=vallVars[1],
							OffsetVar=offset,
							ParamsSpecID=vspecid,
							DistType="POISSON",
							MaxIterations=maxiter,
							Note=genNote("gam")
							)
	# vsqlstr <- paste0(" CALL FLGAM(",fquote(data@select@table_name),",",
	# 							fquote(getVariables(data)[["obs_id_colname"]]),",",
	# 							fquote(vallVars[1]),",",
	# 							offset,",",
	# 							fquote(vspecid),",'POISSON',",
	# 							maxiter,",",
	# 							fquote(genNote("gam")),",AnalysisID);")

	# vresult <- sqlQuery(getFLConnection(),vsqlstr,
	# 				AnalysisIDQuery=genAnalysisIDQuery("fzzlGAMInfo",genNote("gam")))
	vresult <- checkSqlQueryOutput(vresult)
	vanalysisId <- as.character(vresult[1,1])
	return(new("FLGAM",
				formula=formula,
				AnalysisID=vanalysisId,
				table=data,
				results=list(call=callObject,
							pTerms=pTerms),
				specid=vspecid,
				scoreTable="",
				family=family,
				offset=offsetCopy
				))
}

#' @export
`$.FLGAM`<-function(object,property)
{
	#parentObject <- deparse(substitute(object))
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

	if(property=="coefficients")
	{
		coefficientsvector <- coefficients.FLGAM(object)
		assign(parentObject,object,envir=parent.frame())
		return(coefficientsvector)
	}
	else if(property=="fitted.values" || property=="linear.predictors")
	{
		fitvector <- fitted.values.FLGAM(object)
		assign(parentObject,object,envir=parent.frame())
		return(fitvector)
	}
	else if(property=="residuals")
	{
		residualsvector <- residuals.FLGAM(object)
		assign(parentObject,object,envir=parent.frame())
		return(residualsvector)
	}
	else if(property=="FLCoeffStdErr")
	{
		coeffVector <- coefficients.FLGAM(object)
		assign(parentObject,object,envir=parent.frame())
		return(object@results[["FLCoeffStdErr"]])
	}
	else if(property=="FLCoeffChiSq")
	{
		coeffVector <- coefficients.FLGAM(object)
		assign(parentObject,object,envir=parent.frame())
		return(object@results[["FLCoeffChiSq"]])
	}
	else if(property=="FLCoeffPValue")
	{
		coeffVector <- coefficients.FLGAM(object)
		assign(parentObject,object,envir=parent.frame())
		return(object@results[["FLCoeffPValue"]])
	}
	else if(property=="call")
	{
		return(object@results[["call"]])
	}
	else if(property=="converged")
	{
		return(TRUE)
	}
	else if(property=="deviance")
	{
		deviancevector <- deviance.FLGAM(object)
		assign(parentObject,object,envir=parent.frame())
		return(deviancevector)
	}
	else if(property=="df.residual")
	{
		##Reference from FLLinRegr. Not Correct Anyways.
		## Actual df.residual is less than this.
		df.residualsvector <- nrow(object@table)-length(object$coefficients)
		assign(parentObject,object,envir=parent.frame())
		return(df.residualsvector)
	}
	else if(property=="sig2")
	{
		sig2vector <- sig2.FLGAM(object)
		assign(parentObject,object,envir=parent.frame())
		return(sig2vector)
	}
	else if(property=="pterms")
	{
		return(object@results[["pTerms"]])
	}
	else if(property=="assign")
	{
		pTerms <- object@results[["pTerms"]]
		return(c(0,rep(1,length(attr(pTerms,"variables"))-2)))
	}
	else if(property=="nsdf")
	{
		return(length(object$assign))
	}
	else if(property=="data")
	{
		if(!is.null(object@results[["data"]]))
		return(object@results[["data"]])
		else
		{
			# if(interactive())
			# {
			# 	vinput <- readline("Fetching entire table. Continue? y/n ")
			# 	if(!checkYorN(vinput)) return(NULL)
			# }
			object@results <- c(object@results,list(data=as.data.frame(object@table)))
			assign(parentObject,object,envir=parent.frame())
			return(object@results[["data"]])
		}
	}
	else if(property=="df.null")
	{
		return(nrow(object@table)-1)
	}
	else if(property=="edf")
	{
		edfvector <- length(object$coefficients)-1
		assign(parentObject,object,envir=parent.frame())
		return(edfvector)
	}
	else if(property=="scale.estimated")
		return(TRUE)
	else if(property=="family")
		return(object@family)
	else if(property=="formula")
		return(object@formula)
	else if(property=="min.edf")
		return(length(all.vars(object@formula)))
	else if(property=="model")
	{
		modelframe <- model.FLGAM(object)
		assign(parentObject,object,envir=parent.frame())
		return(modelframe)
	}
	else if(property=="terms")
	{
		vterms <- terms.FLGAM(object)
		assign(parentObject,object,envir=parent.frame())
		return(vterms)
	}
	else if(property=="offset")
	{
		offsetvector <- offset.FLGAM(object)
		assign(parentObject,object,envir=parent.frame())
		return(offsetvector)
	}
	else if(property=="pred.formula")
	{
		vallVars <- all.vars(object@formula)
		vformula <- paste0("~",paste0(vallVars[2:length(vallVars)],collapse="+"))
		return(formula(vformula))
	}
	else if(property=="prior.weights" || property=="weights")
	{
		return(rep(1,nrow(object@table)))
	}
	else if(property=="offset")
	{
		offsetvector <- offset.FLGAM(object)
		assign(parentObject,object,envir=parent.frame())
		return(offsetvector)
	}
	else if(property=="y")
	{
		yvector <- y.FLGAM(object)
		assign(parentObject,object,envir=parent.frame())
		return(yvector)
	}
	else if(property=="var.summary")
	{
		var.summaryList <- var.summary.FLGAM(object)
		assign(parentObject,object,envir=parent.frame())
		return(var.summaryList)
	}
	else if(property=="knots")
	{
		if(!is.null(object@results[["knots"]]))
		return(object@results[["knots"]])
		else
		{
			sqlstr <- paste0("SELECT * FROM fzzlGAMKnots WHERE ",
							" analysisID='",object@AnalysisID,"'",
							" ORDER BY 2,3,4")
			knotsdataframe <- sqlQuery(getFLConnection(),sqlstr)
			knotsdataframe <- checkSqlQueryOutput(knotsdataframe)
			object@results <- c(object@results,list(knots=knotsdataframe))
			assign(parentObject,object,envir=parent.frame())
			return(object@results[["knots"]])
		}
	}
	else stop(property," is not a valid property\n")
}

#' @export
coefficients.FLGAM <- function(object)
{
	if(!is.null(object@results[["coefficients"]]))
	return(object@results[["coefficients"]])
	else
	{
		##Since Currently only 1000 coeffs are supported
		## by FLGAM, fetch them.
		coeffVector <- sqlQuery(getFLConnection(),
			paste0("SELECT * FROM fzzlGAMCoeffs where AnalysisID=",fquote(object@AnalysisID),
					" ORDER BY CoeffID"))

		colnames(coeffVector) <- toupper(colnames(coeffVector))
		stderrVector <- coeffVector[["STDERR"]]
		chisqVector <- coeffVector[["CHISQ"]]
		pvalVector <- coeffVector[["PVALUE"]]
		coeffVector1 <- coeffVector[["COEFFVALUE"]]

		names(coeffVector1) <- renameDuplicates(as.character(coeffVector[["COEFFTERM"]]))
		# names(coeffVector1) <- paste0(coeffVector[["COEFFTERM"]],".",coeffVector[["COEFFID"]]+1)

		object@results <- c(object@results,list(coefficients=coeffVector1,
												FLCoeffStdErr=stderrVector,
												FLCoeffChiSq=chisqVector,
												FLCoeffPValue=pvalVector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(coeffVector1)
	}
}

#' @export
fitted.values.FLGAM <- function(object)
{
    if(!is.null(object@results[["fitted.values"]]))
	return(object@results[["fitted.values"]])
    else
    {
        if(object@scoreTable=="")
            # object@scoreTable <- paste0(getOption("ResultDatabaseFL"),".",gen_score_table_name(object@table@select@table_name))
            object@scoreTable <- gen_score_table_name(object@table@select@table_name)
        if(length(object@deeptable@select@variables)>0)
            vtbl <- object@deeptable
        else vtbl <- object@table
        
        fitted.valuesVector <- predict(object,vtbl,scoreTable=object@scoreTable)
        object@results <- c(object@results,list(fitted.values=fitted.valuesVector))
        parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
        assign(parentObject,object,envir=parent.frame())
        return(fitted.valuesVector)
    }
}

#' @export
residuals.FLGAM <- function(object)
{
	if(!is.null(object@results[["residuals"]]))
	return(object@results[["residuals"]])
	else
	{
		
		if(object@scoreTable==""){
		# object@scoreTable <- paste0(getOption("ResultDatabaseFL"),".",
		# 	gen_score_table_name(object@table@select@table_name))
		object@scoreTable <- gen_score_table_name(object@table@select@table_name)
		fitted.valuesVector <- predict(object,object@table,scoreTable=object@scoreTable)
		object@results <- c(object@results,list(fitted.values=fitted.valuesVector))
		}
		vtablename <- object@table@select@table_name
		obs_id_colname <- getVariables(object@table)[["obs_id_colname"]]

		y <- "fPred"
		vobsid <- "ObsID"
		
  #   	if(!object@table@isDeep)
		# vYVector <- object@table[,all.vars(object@formula)[1]]
		# else
		# vYVector <- object@table[,"-1"]
		# residualsvector <- vYVector - object@results[["fitted.values"]]
		sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,",
							object@scoreTable,".",vobsid," AS vectorIndexColumn,",
							vtablename,".",all.vars(object@formula)[1]," - ",
							object@scoreTable,".",y," AS vectorValueColumn",
						" FROM ",object@scoreTable,",",vtablename,
						" WHERE ",vtablename,".",obs_id_colname," = ",
									object@scoreTable,".",vobsid)

		tblfunqueryobj <- new("FLTableFunctionQuery",
                        connectionName = getFLConnectionName(),
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

		residualsvector <- newFLVector(
								select = tblfunqueryobj,
								Dimnames = list(rownames(object@table),
												"vectorValueColumn"),
								isDeep = FALSE)

		object@results <- c(object@results,list(residuals=residualsvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(residualsvector)
	}
}

deviance.FLGAM <- function(object)
{
	if(!is.null(object@results[["deviance"]]))
	return(object@results[["deviance"]])
	else
	{
		if(object@scoreTable==""){
		# object@scoreTable <- paste0(getOption("ResultDatabaseFL"),".",gen_score_table_name(object@table@select@table_name))
		object@scoreTable <- gen_score_table_name(object@table@select@table_name)
		fitted.valuesVector <- predict.FLGAM(object,object@table,scoreTable=object@scoreTable)
		object@results <- c(object@results,list(fitted.values=fitted.valuesVector))
		}
		vtablename <- object@table@select@table_name
		obs_id_colname <- getVariables(object@table)[["obs_id_colname"]]

		y <- paste0(vtablename,".",all.vars(object@formula)[1])
		pred <- paste0(object@scoreTable,".fPred")

		sqlstr <- paste0("WITH z(yval,pred,vlog) AS ",
						"(SELECT ",y," AS yval,",pred," AS pred,",
						y,"/",pred," AS vlog ",
						" FROM ",object@scoreTable,",",vtablename,
							" WHERE ",vtablename,".",obs_id_colname," = ",
										object@scoreTable,".ObsID)",
						" SELECT SUM(a.vLL*a.vLL) FROM ",
						"(SELECT CASE WHEN z.vlog> 0 ",
							" THEN (z.yval * LOG(vlog))-(z.yval-z.pred) ELSE z.pred END AS vLL from z) as a")

		deviancevector <- sqlQuery(getFLConnection(),sqlstr)
		deviancevector <- checkSqlQueryOutput(deviancevector)[[1]]
		object@results <- c(object@results,list(deviance=deviancevector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(deviancevector)
	}
}

sig2.FLGAM <- function(object)
{
	if(!is.null(object@results[["sig2"]]))
	return(object@results[["sig2"]])
	else
	{
		if(object@scoreTable==""){
		# object@scoreTable <- paste0(getOption("ResultDatabaseFL"),".",gen_score_table_name(object@table@select@table_name))
		object@scoreTable <- gen_score_table_name(object@table@select@table_name)
		fitted.valuesVector <- predict.FLGAM(object,object@table,scoreTable=object@scoreTable)
		object@results <- c(object@results,list(fitted.values=fitted.valuesVector))
		}
		vtablename <- object@table@select@table_name
		obs_id_colname <- getVariables(object@table)[["obs_id_colname"]]

		df.residualsvector <- object$df.residual
		sqlstr <- paste0("SELECT SUM(a.vresiduals*a.vresiduals)/",df.residualsvector,
						" FROM(SELECT ",
							vtablename,".",all.vars(object@formula)[1]," - ",
							object@scoreTable,".fPred AS vresiduals",
						" FROM ",object@scoreTable,",",vtablename,
						" WHERE ",vtablename,".",obs_id_colname," = ",
									object@scoreTable,".ObsID) AS a")
		sig2vector <- sqlQuery(getFLConnection(),sqlstr)[[1]]

		object@results <- c(object@results,list(sig2=sig2vector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(sig2vector)
	}
}

#' @export
model.FLGAM <- function(object)
{
	if(!is.null(object@results[["model"]]))
	return(object@results[["model"]])
	else
	{
		vallVars <- all.vars(object@formula)
		vcolnames <- toupper(vallVars)
		if(!is.null(object@results[["data"]]))
		{
			modelframe <- object@results[["data"]]
			colnames(modelframe) <- toupper(colnames(modelframe))
			modelframe <- modelframe[,vcolnames]
		}
		else
		{
			# vinput <- ""
			# if(interactive())
			# {
			# 	vinput <- readline("Fetch top 10 rows only(preferred) y/n ")
			# 	vtablename <- paste0(object@table@select@database,".",object@table@select@table_name)
			# 	if(checkYorN(vinput)) vinput <- paste0(" TOP 10 ")
			# }
			
			obs_id_colname <- getVariables(object@table)[["obs_id_colname"]]

			vsqlstr <- paste0("SELECT ",paste0(vcolnames,collapse=","),
							 " FROM ",vtablename,
							 " ORDER BY ",obs_id_colname)
			modelframe <- sqlQuery(getFLConnection(),vsqlstr)
			modelframe <- checkSqlQueryOutput(modelframe)
		}
		colnames(modelframe) <- vallVars
		object@results <- c(object@results,list(model=modelframe))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(modelframe)
	}
}

#' @export
terms.FLGAM <- function(object)
{
	if(!is.null(object@results[["terms"]]))
	return(object@results[["terms"]])
	else
	{
		vallVars <- all.vars(object@formula)
		vformula <- paste0(vallVars[1],"~",1,"+",
						paste0(vallVars[2:length(vallVars)],collapse="+"))
		pTerms <- terms(formula(vformula))
		object@results <- c(object@results,list(terms=pTerms))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(pTerms)
	}
}

offset.FLGAM <- function(object)
{
	if(!is.null(object@results[["offset"]]))
	return(object@results[["offset"]])
	else
	{
		if(object@offset=="NULL")
		offsetvector <- rep(0,nrow(object@table))
		else
		{
			vtablename <- object@table@select@table_name
			obs_id_colname <- getVariables(object@table)[["obs_id_colname"]]

			sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,",
								obs_id_colname," AS vectorIndexColumn,",
								object@offset," AS vectorValueColumn",
							" FROM ",vtablename)

			tblfunqueryobj <- new("FLTableFunctionQuery",
	                        connectionName = getFLConnectionName(),
	                        variables = list(
				                obs_id_colname = "vectorIndexColumn",
				                cell_val_colname = "vectorValueColumn"),
	                        whereconditions="",
	                        order = "",
	                        SQLquery=sqlstr)

			offsetvector <- newFLVector(
									select = tblfunqueryobj,
									Dimnames = list(1:nrow(object@table),
													"vectorValueColumn"),
									isDeep = FALSE)
		}
		
		object@results <- c(object@results,list(offset=offsetvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(offsetvector)
	}
}

var.summary.FLGAM <- function(object)
{
	if(!is.null(object@results[["var.summary"]]))
	return(object@results[["var.summary"]])
	else
	{
		vallVars <- all.vars(object@formula)
		vcolnames <- toupper(vallVars[2:length(vallVars)])
		if(!is.null(object@results[["data"]]))
		{
			modelframe <- object@results[["data"]]
			colnames(modelframe) <- toupper(colnames(modelframe))
			modelframe <- modelframe[,vcolnames]
			var.summaryList <- as.list(as.data.frame(apply(modelframe,2,summary))[c(4,3,6),])
		}
		else
		{
			vtablename <- object@table@select@table_name
			obs_id_colname <- getVariables(object@table)[["obs_id_colname"]]

			vsqlstr <- paste0("SELECT FLMean(",vcolnames,") AS mean1,",
								"Median(",vcolnames,") AS median1,",
								"FLMax(",vcolnames,") AS max1 ",
							 " FROM ",vtablename,collapse=" UNION ALL ")
			modelframe <- sqlQuery(getFLConnection(),vsqlstr)
			modelframe <- checkSqlQueryOutput(modelframe)
			modelframe <- as.data.frame(t(as.matrix(modelframe)))
			colnames(modelframe) <- vcolnames
			var.summaryList <- as.list(modelframe)
		}

		object@results <- c(object@results,list(var.summary=var.summaryList))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(var.summaryList)
	}
}

y.FLGAM <- function(object)
{
	if(!is.null(object@results[["y"]]))
	return(object@results[["y"]])
	else
	{
		vallVars <- all.vars(object@formula)
		vcolnames <- toupper(vallVars[1])
		if(!is.null(object@results[["data"]]))
		{
			modelframe <- object@results[["data"]]
			colnames(modelframe) <- toupper(colnames(modelframe))
			yvector <- modelframe[[vcolnames]]
		}
		else
		{
			vtablename <- object@table@select@table_name
			obs_id_colname <- getVariables(object@table)[["obs_id_colname"]]

			sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,",
								obs_id_colname," AS vectorIndexColumn,",
								vcolnames," AS vectorValueColumn",
							" FROM ",vtablename)

			tblfunqueryobj <- new("FLTableFunctionQuery",
	                        connectionName = getFLConnectionName(),
	                        variables = list(
				                obs_id_colname = "vectorIndexColumn",
				                cell_val_colname = "vectorValueColumn"),
	                        whereconditions="",
	                        order = "",
	                        SQLquery=sqlstr)

			yvector <- newFLVector(
							select = tblfunqueryobj,
							Dimnames = list(1:nrow(object@table),
											"vectorValueColumn"),
							isDeep = FALSE)
		}
		
		object@results <- c(object@results,list(y=yvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(yvector)
	}
}

#' @export
predict.FLGAM <- function(object,
						newdata=object@table,
						scoreTable="",
						...)
{
	newdata <- setAlias(newdata,"")
	args <- list(...)
	names(args) <- tolower(names(args))
	vinputCols <- list(ModelSpecID=object@specid,
						InAnalysisID=object@AnalysisID)
	if(all(c("termidscore","byvarvalscore",
			"termidparam","varname",
			"byvarvalparam","varval") %in% names(args))){
		vspecid <- genRandVarName()
		# vsqlstr <- paste0("INSERT INTO fzzlGAMScoreParams \n ",
		# 					"VALUES(",fquote(vspecid),",",
		# 							args[["termidparam"]],",",
		# 							fquote(args[["varname"]]),",",
		# 							fquote(args[["varname"]]),",",
		# 							args[["termidparam"]],",",
		# 							fquote(args[["varname"]]),
		# 						")",collapse=";")
		# sqlSendUpdate(getOption(getFLConnection()),vsqlstr)
        vdf <- data.frame(vspecid,args[["termidparam"]],
                        args[["varname"]],args[["varname"]],
                        args[["termidparam"]],args[["varname"]])
        insertIntotbl(pTableName="fzzlGAMScoreParams",
                    pValues=vdf)
		vinputCols <- c(vinputCols,
						InTableName="NULL",
						ObsIDCol="NULL",
						ScoreSpecID=vspecid,
						TermID=args[["termidscore"]],
						ByVarVal=args[["byvarvalscore"]]
						)
	}
	else if(is.FLTable(newdata)){
		if(newdata@isDeep) stop("input wide table for scoring\n")
		vinputCols <- c(vinputCols,
						InTableName=newdata@select@table_name,
						ObsIDCol=getVariables(newdata)[["obs_id_colname"]],
						ScoreSpecID="NULL",
						TermID="NULL",
						ByVarVal="NULL"
						)
	}
	else stop("provide FLTable as newdata or ",
			paste0(c("termidscore","byvarvalscore",
						"termidparam","varname",
						"byvarvalparam","varval"),collapse=","),
			" using ... argument \n")

	if(scoreTable=="")
	scoreTable <- paste0(getOption("ResultDatabaseFL"),".",gen_score_table_name(object@table@select@table_name))
	else if(!grep(".",scoreTable)) 
	scoreTable <- paste0(getOption("ResultDatabaseFL"),".",scoreTable)
	
	vinputCols <- c(vinputCols,
					scoreTable=scoreTable,
					Note=genNote("Scoring gam"))
	AnalysisID <- sqlStoredProc(getFLConnection(),
								"FLGAMScore",
								outputParameter=c(AnalysisID="a"),
								pInputParams=vinputCols
								)
	# sqlstr <- paste0("CALL FLGAMScore(",fquote(object@specid),",",
	# 									fquote(object@AnalysisID),",",
	# 									fquote(newdata@select@table_name),",",
	# 									fquote(getVariables(newdata)[["obs_id_colname"]]),",",
	# 									"NULL,",
	# 									"NULL,",
	# 									"NULL,",
	# 									fquote(scoreTable),",",
	# 									fquote(genNote("Scoring gam")),
	# 									",AnalysisID);")

	# AnalysisID <- sqlQuery(getFLConnection(),
	# 				sqlstr,
	# 				AnalysisIDQuery=genAnalysisIDQuery("fzzlGAMInfo",genNote("Scoring gam")))
	AnalysisID <- checkSqlQueryOutput(AnalysisID)

	sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
						"ObsID AS vectorIndexColumn,",
						"fpred AS vectorValueColumn",
					" FROM ",scoreTable)

	tblfunqueryobj <- new("FLTableFunctionQuery",
                        connectionName = getFLConnectionName(),
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

	flv <- newFLVector(
				select = tblfunqueryobj,
				Dimnames = list(1:nrow(newdata),
								"vectorValueColumn"),
				isDeep = FALSE)

	return(flv)
}
#' @export
print.FLGAM <- function(object)
{
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	vDummyGam <- list(family=object$family,
					formula=object$formula,
					edf=object$edf,
					df.null=object$df.null,
					df.residual=object$df.residual)
	class(vDummyGam) <- c("gam","glm","lm")
	assign(parentObject,object,envir=parent.frame())
	mgcv::print.gam(vDummyGam)
}
#' @export
setMethod("show","FLGAM",
			function(object)
			{
				parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
				print(object)
				assign(parentObject,object,envir=parent.frame())
			}
		 )

checkNumOfPredPts <- function(vNumOfPredPts)
{
	if(is.null(vNumOfPredPts) || is.na(vNumOfPredPts))
			vNumOfPredPts <- 20
	else if(is.numeric(vNumOfPredPts) && vNumOfPredPts <= 0)
	{
		cat("NumOfPredPts given by xt must be >0. Setting to 20")
		vNumOfPredPts <- 20
	}
	else if(!is.numeric(vNumOfPredPts))
	{
		cat("NumOfPredPts given by xt must be numeric. Setting to 20")
		vNumOfPredPts <- 20
	}
	vNumOfPredPts
}

checkSplineDegree <- function(vsplineDegree)
{
	if(is.na(vsplineDegree) || is.null(vsplineDegree)) vsplineDegree <- 3
	else if(!is.numeric(vsplineDegree))
	{
		cat("spline degree given by m should be numeric.Setting to 3")
		vsplineDegree <- 3
	}
	else if(vsplineDegree <= 0 || vsplineDegree > 5)
	{
		cat("spline degree given by m must satisfy 0<m<=5.Setting to 3")
		vsplineDegree <- 3
	}
	vsplineDegree
}

checkNumOfKnots <- function(vNumOfKnots,vsplineDegree)
{
	if(is.na(vNumOfKnots) || is.null(vNumOfKnots)) vNumOfKnots <- vsplineDegree+1
	else if(!is.numeric(vNumOfKnots))
	{
		cat("No.Of.Knots given by k should be numeric.Setting to splinedegree(m) + 1")
		vNumOfKnots <- vsplineDegree + 1
	}
	else if(vNumOfKnots==-1) 
	vNumOfKnots <- vsplineDegree + 1
	else if(vNumOfKnots <= 0 || vNumOfKnots <= vsplineDegree)
	{
		cat("No.of.knots given by k must be > splineDegree(m).Setting to m+1")
		vNumOfKnots <- vsplineDegree + 1
	}
	vNumOfKnots
}

checkByVarName <- function(vByVarName,vcolnames)
{
	if(!is.character(vByVarName))
	{
		cat("byVarName given by 'by' must be a character.Setting to NULL")
		vByVarName <- "NULL"
	}
	else if(vByVarName=="NA")
	vByVarName <- "NULL"
	else
	{
		vByVarName <- base::strsplit(vByVarName,"\"")[[1]][2]
		if(!(vByVarName %in% vcolnames))
		{
			cat("byVarName given by 'by' must be in colnames of input wide table.Setting to NULL")
			vByVarName <- "NULL"
		}
	}
	vByVarName
}
