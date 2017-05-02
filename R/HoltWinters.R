#' @export
HoltWinters<-function(object,...){
	UseMethod("HoltWinters",object)
}

#' @export
HoltWinters.default  <- function (object,...){
    return(stats::HoltWinters(object,...))
}

#' @export
HoltWinters.FLVector<-function(object,
						 	   alpha=0.5,
						 	   beta=0.5,
						  	   gamma=0.5,
						 	   periodicity=7,
						 	   forecastperiod=7,
						 	   normalization=0,...){ #browser()
	if(!is.FLVector(object)) stop("The class of the input object should be FLVector")
	if(!all(beta)) Beta<-0 else Beta<-beta
	if(!all(gamma)) Gamma<-0 else Gamma<-gamma
 	t <- constructUnionSQL(pFrom = c(a = constructSelect(object)),
                           pSelect = list(a = c(GroupID = 1,
                           						PeriodID="a.vectorIndexColumn",
                                                Num_Val = "a.vectorValueColumn",
                                                periodicity=periodicity,
                                                alpha=alpha,
                                                beta=Beta,
                                                gamma=Gamma,
                                                forecastperiod=forecastperiod)))
	temp1 <- createTable(pTableName=gen_unique_table_name("HoltWinters"),pSelect=t)

	if(!all(gamma) && all(beta)){
		pSelect<-paste0("Select GroupID, PeriodID, Num_Val, periodicity,
						alpha, beta, forecastperiod from ",temp1)
		pViewColnames<-c(pGroupID="GroupID",
						pNum_Val="Num_Val",
						pPeriodicity="periodicity",
						pAlpha="alpha",
						pBeta="beta",
						pNormal=normalization,
						pForecastPeriod=forecastperiod)
		pFuncName<-"FLExpSmooth2FactorUdt"
	}
	else if(!all(gamma) && !all(beta)){ 
		pSelect<-paste0("Select GroupID, PeriodID, Num_Val, periodicity,
						alpha, forecastperiod from ",temp1)
		pViewColnames<-c(pGroupID="GroupID",
						 pNum_Val="Num_Val",
						 pPeriodicity="periodicity",
						 pAlpha="alpha",
						 pNormal=normalization,
						 pForecastPeriod=forecastperiod)
		pFuncName<-"FLExpSmooth1FactorUdt"
	}
	else {
		pSelect<-paste0("Select GroupID, PeriodID, Num_Val, periodicity,
						alpha, beta, gamma, forecastperiod from ",temp1)
		pViewColnames<-c(pGroupID="GroupID",
						 pNum_Val="Num_Val",
			 			 pPeriodicity="periodicity",
						 pAlpha="alpha",
						 pBeta="beta",
						 pGamma="gamma",
						 pNormal=normalization,
						 pForecastPeriod=forecastperiod)
		pFuncName<-"FLHoltWintersUdt"
	}

	query<-constructUDTSQL(pViewColnames=pViewColnames,
						   pSelect=pSelect,
						   pOutColnames=c("a.*"),
						   pFuncName=pFuncName,
						   pLocalOrderBy=c("pGroupID"),
						   pNest=TRUE)
	temp2 <- createTable(pTableName=gen_unique_table_name("temp"),pSelect=query)
	ret <- sqlQuery(getFLConnection(),paste0("Select * from ",temp2," order by 2"))
	retobj<-list(fitted=ret,
				 x=object,
				 alpha=alpha,
				 beta=beta,
				 gamma=gamma,
				 coefficients=NULL,
				 seasonal="additive",
				 SSE=NULL,
				 call=match.call())
	class(retobj)<-"HoltWinters"
	return(retobj)
}
