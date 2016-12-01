#' @export

setClass(
	"FLLinRegrSF",
	contains="FLRegr",
	slots=list(offset="character",
				vfcalls="character"))



setClass(
	"FLLogRegrSF",
	contains="FLRegr",
	slots=list(offset="character",
				vfcalls="character"))


stepSF<-function(formula,data,method,...){
	vcallObject <- match.call()
	if(method=="lin") familytype<-"FLLinRegrSF"
	else if(method=="log") familytype<-"logisticSF"
	else stop("Please specify method as either 'lin' or 'log' ")
	return(lmGeneric(formula=formula,
                     data=data,
                     callObject=vcallObject,
                     familytype=familytype,
                     ...))
}

summary.FLLinRegrSF<-function(object,modelid=1){
	AnalysisID<-object@AnalysisID
	statstablename<-object@vfcalls["statstablename"]
	query<-paste0("Select * from ",statstablename, " Where AnalysisID = ",
					fquote(AnalysisID)," And modelid =",modelid)
	x<-sqlQuery(getFLConnection(),query)
	coeff<-sqlQuery(getFLConnection(),paste0("Select * from ",object@vfcalls["coefftablename"],
											 " Where AnalysisID=",fquote(AnalysisID)," And modelid=coeffid"))
	coeffframe <- data.frame(coeff=coeff$COEFFVALUE,
                             stderr=coeff$STDERR,
                             t_stat=coeff$TSTAT,
                             p_value=coeff$PVALUE)
	reqList <- list(call = as.call(object@formula),
					residuals  = NULL,
	                coefficients = as.matrix(coeffframe),
	                sigma = x$STDERR,
	                df = as.vector(c((x$DFREGRESSION + 1),x$DFRESIDUAL, (x$DFREGRESSION + 1))),
	                r.squared = x$RSQUARED,
	                adj.r.squared = x$ADJRSQUARED,
	                fstatistic = c(x$FSTAT, x$DFREGRESSION, x$DFRESIDUAL ),
	                aliased = FALSE
	                        )
    class(reqList) <- "summary.lm"
    reqList
}

`$.FLLinRegrSF`<-function(object,property){
	if(property=="coefficients")
	return(coefficients(object))
}

coefficients.FLLinRegrSF<-function(object){
	AnalysisID<-object@AnalysisID
	coefftablename<-object@vfcalls["coefftablename"]
	query1<-paste0("Select a.modelid, a.coeffvalue From ",coefftablename,
					" a Where AnalysisID= ",fquote(AnalysisID)," And a.modelid=a.coeffid ORDER BY 1")
	query2<-paste0("Select a.modelid, a.coeffvalue From ",coefftablename,
					" a Where AnalysisID= ",fquote(AnalysisID)," And a.modelid!=a.coeffid ORDER BY 1")
	a<-sqlQuery(getFLConnection(),query1)
	b<-sqlQuery(getFLConnection(),query2)
	ret<-data.frame(ModelID=a$MODELID,
					Intercept=b$COEFFVALUE,
					Coeff=a$COEFFVALUE)
	return(data.matrix(ret))
}

summary.FLLogRegrSF <- function(object,modelid=1){ #browser()
    AnalysisID<-object@AnalysisID
    statstablename<-object@vfcalls["statstablename"]
    query<-paste0("Select * from ",statstablename," Where AnalysisID=",fquote(AnalysisID),
    			  " And VarID =",modelid)
    stat<-sqlQuery(getFLConnection(),query)
    coeffVector<-sqlQuery(getFLConnection(),paste0("Select * from ",object@vfcalls["coefftablename"],
    										" Where AnalysisID =",fquote(AnalysisID)," Order By 1"))
    coeffframe <- data.frame(coeff=coeffVector$ESTALPHA,
							stderr=coeffVector$STDERRALPHA,
							chisq=coeffVector$CHISQALPHA,
							p_value=coeffVector$PVALALPHA)
    reqList <- list(call = as.call(object@formula),
                    coefficients = as.matrix(coeffframe),
                    df = as.vector(c((stat$NUMOFOBS + 1),(stat$NUMOFOBS-1-nrow(coeffVector)), (stat$NUMOFOBS + 1))),
                    aliased = FALSE,
                    dispersion = 1,
                    df.residual = (stat$NUMOFOBS-1-nrow(coeffVector)),
                    iter = stat$ITERATIONS,
                    df.null = (stat$NUMOFOBS - 1),
                    null.deviance = NA
                )
    class(reqList) <- "summary.glm"
    reqList
}

coefficients.FLLogRegrSF<-function(object){
	AnalysisID<-object@AnalysisID
	coefftablename<-object@vfcalls["coefftablename"]
	query<-paste0("Select VarID, EstAlpha as Coeff From ",coefftablename,
				  " Where AnalysisID=",fquote(AnalysisID)," Order By 1")
	coeffVector<-sqlQuery(getFLConnection(),query)
	return(coeffVector)
}

`$.FLLogRegrSF`<-function(object,property){
	if(property=="coefficients")
	return(coefficients(object))
}