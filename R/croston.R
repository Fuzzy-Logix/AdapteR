#' @export
NULL

#' Forecasts for intermittent demand using Croston's method
#' 
#' Returns forecasts and other information for Croston's forecasts applied to object.
#'
#' Based on Croston's (1972) method for intermittent demand forecasting,
#' also described in Shenstone and Hyndman (2005). Croston's method 
#' involves using simple exponential smoothing (SES) on the non-zero elements of the time series
#' and a separate application of SES to the times between non-zero elements of the time series.
#' The smoothing parameters are denoted by alpha.
#'
#' @param object FLVector
#' @param alpha Smoothing parameter. Default value = 0.5 .
#' @param h Number of periods for forecasting.
#'
#' @return An object containing details of the fitted model and forecasted values.
#' @examples
#' x<-rnorm(1000)
#' flv<-as.FLVector(x)
#' flobj<-croston(object = flv, h=9)

#' @export
croston<-function(object,...){
	UseMethod("croston",object)
}

#' @export
croston.default  <- function (object,...){
    if (!requireNamespace("forecast", quietly = TRUE)){
        stop("forecast package needed for croston. Please install it.",
             call. = FALSE)
    }
    else return(forecast::croston(object,...))
}

#' @export
croston.FLVector<-function(object,
						   alpha=0.5,
						   h=7,...){ #browser()
	if(!is.FLVector(object)) stop("The class of the input object should be FLVector")
	if(alpha<0 || alpha>1) stop("The alpha value should be between 0 and 1")

	t <- constructUnionSQL(pFrom = c(a = constructSelect(object)),
                           pSelect = list(a = c(GroupID = 1,
                           						PeriodID= "a.vectorIndexColumn",
                                                Num_Val = "a.vectorValueColumn")))
	temp1 <- createTable(pTableName=gen_unique_table_name("croston"),pSelect=t)
	pSelect<-paste0("Select * from ",temp1)
	query<-constructUDTSQL(pViewColnames=c(pGroupID="GroupID",
										   pPeriodID="PeriodID",
						   				   pNum_Val="Num_Val"),
						   pSelect=pSelect,
						   pArgs=c(alpha=alpha,
						   		   forecastPeriod=h),
						   pOutColnames=c("a.*"),
						   pFuncName="FLCrostonsUdt",
						   pLocalOrderBy=c("pGroupID","pPeriodID"),
						   UDTInputSubset=c(1,3))
	temp2 <- createTable(pTableName=gen_unique_table_name("temp"),pSelect=query)
	ret <- sqlQuery(getFLConnection(),paste0("Select * from ",temp2," order by 2"))
	retv<-ret$oForecastValue
	mean<-ts(data=tail(retv,n=h),start=length(object)+1)
	fitted<-head(retv,n=length(object))
	residuals<-object-fitted
	ret<-list(mean=mean,
			  fitted=fitted,
			  x=object,
			  residuals=residuals,
			  method="Croston's method",
			  series="object")
	class(ret)<-"FLCroston"
	return(ret)
}

print.FLCroston<-function(object){
	df<-data.frame(object$mean)
	colnames(df)<-"Point Forecast"
	rownames(df)<-(length(object$x)+1):(length(object$x)+length(object$mean))
	print(df)
}