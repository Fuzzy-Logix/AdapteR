#' @export
NULL

#' Augmented Dickey Fuller Test
#' 
#' Computes the Augmented Dickey-Fuller test for the null that x has a unit root.
#'
#' The general regression equation which incorporates a constant and a linear trend
#' is used and the t-statistic for a first order autoregressive coefficient equals one is computed.
#' The number of lags used in the regression is k. The default value of trunc((length(x)-1)^(1/3))
#' corresponds to the suggested upper bound on the rate at which the number of lags, k, should be made to grow with the sample size for the general ARMA(p,q) setup.
#'
#' @param object FLVector
#' @param k the lag order to calculate the test statistic.
#' @param trend 1 for including trend.
#'
#' @return A list with class "htest" containing the following components:
#' \describe{
#' \item{statistic}{the value of the test statistic.}
#' \item{parameter}{the lag order.}
#' \item{p.value}{the p-value of the test.}
#' \item{method}{a character string indicating what type of test was performed.}
#' \item{data.name}{a character string giving the name of the data.}
#' \item{alternative}{a character string describing the alternative hypothesis.}
#' }
#'
#' @examples
#' x<-rnorm(1000)
#' flv<-as.FLVector(x)
#' flobj<-adf.test(object = flv, k=9)

#' @export
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