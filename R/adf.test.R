adf.test<-function(object,...){
	UseMethod("adf.test",object)
}

#' @export
adf.test.default<-function (object,...) {
    if (!requireNamespace("tseries", quietly = TRUE)){
        stop("tseries package needed for Augmented Dickey Fuller test. Please install it.",
             call. = FALSE)
    }
    else return(tseries::adf.test(object,...))
}

#' @export
adf.test.FLVector<-function(object,
						 	k=6,
						 	trend=1,...){
	if(!is.FLVector(object)) stop("The class of the object should be FLVector.")
	vinputcols<-list(INPUT_TABLE=getTableNameSlot(object),
					 OBSID="vectorIndexColumn",
					 NUMVAL="vectorValueColumn",
					 TREND=trend,
					 LAG=k)
	vfuncName<-"FLADF"
	AnalysisID<-sqlStoredProc(getFLConnection(),
						  vfuncName,
						  outputParameter=c(AnalysisID="a"),
						  pInputParameters=vinputcols)
	ret<-sqlQuery(getFLConnection(),paste0("select * from fzzlADFStats where AnalysisID = ", fquote(AnalysisID)))
	statistic<-ret[1,4]
	parameter<-k	
	names(statistic)<-"Dickey-Fuller"
	names(parameter)<-"Lag order"
	ret <- list(statistic=statistic,
				parameter=parameter,
				alternative="stationary",
				p.value=ret[1,5],
				method="Augmented Dickey-Fuller Test",
				data.name="object")
	class(ret)<-"htest"
	return(ret)
}