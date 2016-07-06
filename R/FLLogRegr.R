#' @include utilities.R
#' @include data_prep.R
#' @include FLTable.R
NULL
#' An S4 class to represent FLLinRegr
#'
#' @slot formula an object of class 'formula': Model Formula
#' @slot deeptable A character vector containing 
#' the deeptable on conversion from a widetable
#' @slot AnalysisID An output character ID from CALL FLLogRegr
#' @slot wideToDeepAnalysisID An output character ID from FLRegrDataPrep
#' @slot mapTable name of the mapping table
#' @slot scoreTable name of the scoring table
#' @slot modelID id of the model with best fit
#' @slot table input FLTable object
#' @slot results cache list of results computed
#' @slot vfcalls contains names of tables
#' @method print FLLogRegr
#' @method coefficients FLLogRegr
#' @method residuals FLLogRegr
#' @method influence FLLogRegr
#' @method lm.influence FLLogRegr
#' @method plot FLLogRegr
#' @method summary FLLogRegr
#' @method predict FLLogRegr
setClass(
	"FLLogRegr",
	slots=list(formula="formula",
				AnalysisID="character",
				wideToDeepAnalysisId="character",
				table="FLTable",
				results="list",
				deeptable="FLTable",
				mapTable="character",
				scoreTable="character",
				offset="character",
				vfcalls="character"))

#' @export
glm <- function (formula,data=list(),...) {
	UseMethod("glm", data)
 }

#' @export
glm.default <- stats::glm

#' Logistic and Poisson Regression.
#'
#' \code{glm} performs logistic and poisson regression on FLTable objects.
#'
#' @param formula A symbolic description of model to be fitted
#' @param family Can be one of poisson,binomial,linear or multinomial.
#' Can be family functions like stats::poisson wherever possible.
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
#' @param pThreshold The threshold for False positive value 
#' that a user can specify to calculate the false positives 
#' and false negatives. Must be between 0 and 1.
#' @param pRefLevel Reference value for dependent variable
#' in case of multinomial family.
#' @param maxiter maximum number of iterations.
#' @section Constraints:
#' The anova method is not yet available for FLLogRegr.
#' In case of multinomial family, residuals,fitted.values
#' properties are not available.plot,influence methods are
#' also not available.
#' @return \code{glm} performs logistic 
#' or poisson regression and replicates equivalent R output.
#' @examples
#' library(RODBC)
#' connection <- flConnect("Gandalf")
#' deeptable <- FLTable("FL_DEMO","tblLogRegr","ObsID","VarID","Num_Val",
#'                whereconditions="ObsID<7001")
#' glmfit <- glm(NULL,data=deeptable)
#' summary(glmfit)
#' plot(glmfit)
#' glmfit <- glm(NULL,data=deeptable,family="logisticwt",eventweight=0.8,noneventweight=1)
#' summary(glmfit)
#' plot(glmfit)
#' connection <- flConnect(odbcSource = "Gandalf",database = "FL_DEV")
#' widetable  <- FLTable("FL_DEV", "siemenswidetoday1", "ObsID")
#' poissonfit <- glm(event ~ meanTemp, family=poisson, data=widetable,offset="age")
#' summary(poissonfit)
#' plot(poissonfit)
#' predData <- FLTable("FL_DEV","preddata1","ObsID")
#' mu <- predict(poissonfit,newdata=predData)
#' deeptable <- FLTable("FL_DEMO","tblLogRegrMN10000","ObsID","VarID","Num_Val",
#'              whereconditions="ObsID<7001")
#' glmfit <- glm(NULL,data=deeptable,family="multinomial")
#' glmfit$coefficients
#' glmfit$FLLogRegrStats
#' glmfit$FLCoeffStdErr
#' summary(glmfit)
#' print(glmfit)
#' @export
glm.FLTable <- function(formula,
						family="binomial",
						data,
						...)
{
	vcallObject <- match.call()
	data <- setAlias(data,"")
	if(is.character(family)){
		if(!family%in%c("poisson","binomial","multinomial","logisticwt"))
		stop("only poisson,binomial and multinomial are currently supported in glm\n")
		if(family %in% "binomial") family <- "logistic"
	}
	if(is.function(family)){
		if(base::identical(family,stats::poisson))
		family <- "poisson"
		else if(base::identical(family,stats::binomial))
		family <- "logistic"
		else stop("only poisson,binomial,multinomial and logisticwt are currently supported in glm\n")
	}

	return(lmGeneric(formula=formula,
					data=data,
					callObject=vcallObject,
					familytype=family,
					...))
}

#' @export
`$.FLLogRegr`<-function(object,property){
	#parentObject <- deparse(substitute(object))
	parentObject <- unlist(strsplit(unlist(strsplit(
		as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]
	if(property %in% c("coefficients","residuals",
		"fitted.values","FLCoeffStdErr",
		"FLCoeffPValue","call","model","x",
		"y","qr","rank","xlevels","terms","assign"))
	{
		propertyValue <- `$.FLLinRegr`(object,property)
		assign(parentObject,object,envir=parent.frame())
		return(propertyValue)
	}
	else if(property=="FLCoeffChiSq")
	{
		coeffVector <- coefficients.FLLogRegr(object)
		assign(parentObject,object,envir=parent.frame())
		return(object@results[["FLCoeffChiSq"]])
	}
	else if(property=="FLLogRegrStats")
	{
		if(!is.null(object@results[["FLLogRegrStats"]]))
		return(object@results[["FLLogRegrStats"]])
		else
		{
			sqlstr <- paste0("SELECT * FROM ",object@vfcalls["statstablename"],"\n",
							" WHERE AnalysisID=",fquote(object@AnalysisID),
							ifelse(!is.null(object@results[["modelID"]]),
							paste0(" \nAND ModelID=",object@results[["modelID"]]),""))

			statsdataframe <- sqlQuery(getOption("connectionFL"),sqlstr)
			object@results <- c(object@results,list(FLLogRegrStats=statsdataframe))
			assign(parentObject,object,envir=parent.frame())
			return(statsdataframe)
		}
	}
	else if(property=="df.residual")
	{
		df.residualsvector <- nrow(object@table)-length(object$coefficients)
		assign(parentObject,object,envir=parent.frame())
		return(df.residualsvector)
	}
	else stop("That's not a valid property")
}

#' @export
coefficients.FLLogRegr<-function(object){
	parentObject <- unlist(strsplit(unlist(strsplit(
		as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	coeffVector <- coefficients.lmGeneric(object,
						FLCoeffStats=c(FLCoeffStdErr="STDERR",
							FLCoeffPValue="PVALUE",
							FLCoeffChiSq="CHISQ"))
	assign(parentObject,object,envir=parent.frame())
	return(coeffVector)
	}

#' @export
residuals.FLLogRegr<-function(object)
{
	parentObject <- unlist(strsplit(unlist(strsplit(
		as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	residualsvector <- residuals.FLLinRegr(object)
	assign(parentObject,object,envir=parent.frame())
	return(residualsvector)
}

#' @export
predict.FLLogRegr <- function(object,
							newdata=object@table,
							scoreTable=""){
	return(predict.lmGeneric(object,newdata=newdata,
							scoreTable=scoreTable))
}

#' @export
summary.FLLogRegr<-function(object){
	ret <- object$FLLogRegrStats
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
							object$FLCoeffChiSq,
							object$FLCoeffPValue)
	colnames(coeffframe)<-c("Estimate","Std. Error","ChiSquare","Pr(>|t|)")

	residualframe <- data.frame(vresult1[[1]],
								vresult1[[3]],
								vresult2[[1]],
								vresult3[[1]],
								vresult1[[2]])
	colnames(residualframe) <- c("Min","1Q","Median","3Q","Max")
	parentObject <- unlist(strsplit(unlist(strsplit
		(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	assign(parentObject,object,envir=parent.frame())
	
	cat("Call:\n")
	cat(paste0(object$call),"\n")
	cat("\nResiduals:\n")
	print(residualframe)
	cat("\n\nCoefficients:\n")
	print(coeffframe)
	cat("\n---\n")
	cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 '' 1\n")
	print(ret)
	cat("\n")
}

#' @export
print.FLLogRegr<-function(object){
	parentObject <- unlist(strsplit(unlist(strsplit(
		as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	print.FLLinRegr(object)
	assign(parentObject,object,envir=parent.frame())
}

#' @export
setMethod("show","FLLogRegr",print.FLLinRegr)

#' @export
plot.FLLogRegr <- function(object)
{
	plot.FLLinRegr(object)
	parentObject <- unlist(strsplit(unlist(strsplit(
		as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	assign(parentObject,object,envir=parent.frame())
}

#' @export
influence.FLLogRegr <- function(model,...){
	parentObject <- unlist(strsplit(unlist(strsplit(as.character
		(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]

	vresult <- influence.FLLinRegr(model,...)
	assign(parentObject,model,envir=parent.frame())
	return(vresult)
}

#' @export
lm.influence.FLLogRegr <- function(model,do.coef=TRUE,...){
	parentObject <- unlist(strsplit(unlist(strsplit(as.character
		(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	vresult <- lm.influence.FLLinRegr(model,do.coef=do.coef,...)
	assign(parentObject,model,envir=parent.frame())
	return(vresult)
}
