#' @include utilities.R
#' @include data_prep.R
#' @include FLTable.R
NULL

#' An S4 class to represent output from glm when family is multinomial
#'
#' @slot offset column name used as offset
#' @slot vfcalls contains names of tables
#' @method print FLLogRegr
#' @method coefficients FLLogRegr
#' @method residuals FLLogRegr
#' @method influence FLLogRegr
#' @method lm.influence FLLogRegr
#' @method plot FLLogRegr
#' @method summary FLLogRegr
#' @method predict FLLogRegr
#' @export
setClass(
    "FLLogRegrMN",
    contains="FLRegr",
    slots=list(offset="character",
                vfcalls="character"))

#' @export
`$.FLLogRegrMN`<-function(object,property){
	#parentObject <- deparse(substitute(object))
	parentObject <- unlist(strsplit(unlist(strsplit(
		as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]
    if(property %in% c("FLCoeffStdErr",
        "FLCoeffPValue","FLCoeffChiSq"))
    {
        vcoeff <- coefficients.FLLogRegrMN(object)
        propertyValue <- object@results[[property]]
        assign(parentObject,object,envir=parent.frame())
        return(propertyValue)
    }
	if(property %in% c("call","model","x",
		"y","qr","rank","xlevels","terms","assign",
		"FLLogRegrStats","df.residual"))
	{
		propertyValue <- `$.FLLogRegr`(object,property)
		assign(parentObject,object,envir=parent.frame())
		return(propertyValue)
	}
	else if(property %in% "coefficients"){
        vcoeff <- coefficients.FLLogRegrMN(object)
        assign(parentObject,object,envir=parent.frame())
        return(vcoeff)
    }
    else
        stop("That's not a valid property")
}

#' @export
coefficients.FLLogRegrMN <- function(object){
	if(!is.null(object@results[["coefficients"]]))
	return(object@results[["coefficients"]])
	else
	{
		vfcalls <- object@vfcalls
		if(object@table@isDeep)
		coeffVector <- sqlQuery(getFLConnection(),
			paste0("SELECT * FROM ",vfcalls["coefftablename"],
				" where AnalysisID=",fquote(object@AnalysisID),
				ifelse(!is.null(object@results[["modelID"]]),
					paste0(" AND ModelID=",object@results[["modelID"]]),""),
					" ORDER BY LevelID,CoeffID"))
		else
		coeffVector <- sqlQuery(getFLConnection(),
			paste0("SELECT CASE WHEN a.Catvalue IS NOT NULL THEN \n",
					"a.COLUMN_NAME || a.Catvalue ELSE \n",
					"a.Column_name END AS CoeffName,b.* \n",
				   " FROM fzzlRegrDataPrepMap AS a,",vfcalls["coefftablename"]," AS b \n",
				   " WHERE a.Final_VarID = b.CoeffID \n",
					" AND a.AnalysisID = ",fquote(object@wideToDeepAnalysisId),
					"\n AND b.AnalysisID = ",fquote(object@AnalysisID),
					ifelse(!is.null(object@results[["modelID"]]),
					paste0("\n AND b.ModelID = ",object@results[["modelID"]]),""),
					"\n ORDER BY LevelID,CoeffID"))

		colnames(coeffVector) <- toupper(colnames(coeffVector))
		coeffVector1 <- coeffVector[["COEFFVALUE"]]
		FLCoeffStats  <- lapply(c(FLCoeffStdErr="STDERR",
							FLCoeffPValue="PVALUE",
							FLCoeffChiSq="CHISQ"),
								function(x)coeffVector[[x]])

		if(!is.null(coeffVector[["COEFFNAME"]]))
		vcolnames1 <- coeffVector[["COEFFNAME"]]
		else{
			vallVars <- all.vars(genDeepFormula(coeffVector[["COEFFID"]]))
			vcolnames1 <- c("Intercept",vallVars[2:length(vallVars)])
		}
		
		vrownames1 <- sort(unique(coeffVector[["LEVELID"]]))
		coeffVector1 <- matrix(coeffVector1,nrow=length(vrownames1),
								ncol=length(vcolnames1),
								Dimnames=list(vrownames1,
												vcolnames1))
		FLCoeffStats <- lapply(FLCoeffStats,function(x)
							matrix(x,nrow=length(vrownames1),
								ncol=length(vcolnames1),
								Dimnames=list(vrownames1,
												vcolnames1)))
		vcolnames <- colnames(object@deeptable)
		droppedCols <- vcolnames[!vcolnames %in% c("-1",coeffVector[["COEFFID"]])]
		object@results <- c(object@results,list(coefficients=coeffVector1,
												droppedCols=droppedCols),
												FLCoeffStats)
		parentObject <- unlist(strsplit(unlist(strsplit(
			as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(coeffVector1)
	}
}

#' @export
summary.FLLogRegrMN <- function(object){
	ret <- object$FLLogRegrStats
	colnames(ret) <- toupper(colnames(ret))
	cat("Call:\n")
	cat(paste0(object$call),"\n")

	sapply(c("coefficients","FLCoeffStdErr",
		"FLCoeffPValue","FLCoeffChiSq"),
		function(x){
			cat("\n\n",x,"\n")
			print(`$.FLLogRegrMN`(object,x))
			cat("\n---\n")
			cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 '' 1\n\n")
			})
	
	cat("FLLogRegrMN Statistics:\n")
	print(ret)
	cat("\n")
	parentObject <- unlist(strsplit(unlist(strsplit
		(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	assign(parentObject,object,envir=parent.frame())
}

#' @export
print.FLLogRegrMN <- function(object){
	parentObject <- unlist(strsplit(unlist(strsplit(
		as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	print.FLLinRegr(object)
	assign(parentObject,object,envir=parent.frame())
}

#' @export
setMethod("show","FLLogRegrMN",print.FLLogRegrMN)

#' @export
residuals.FLLogRegrMN<-function(object)
{
    parentObject <- unlist(strsplit(unlist(strsplit(
        as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
    residualsvector <- clacResiduals(object,"response")
    object@results <- c(object@results,list(residuals=residualsvector))
    assign(parentObject,object,envir=parent.frame())
    return(residualsvector)
}
