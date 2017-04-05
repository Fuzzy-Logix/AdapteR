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

	t <- constructUnionSQL(pFrom = c(a = constructSelect(object)),
                           pSelect = list(a = c(GroupID = 1,
                           						PeriodID="a.vectorIndexColumn",
                                                Num_Val = "a.vectorValueColumn",
                                                periodicity=periodicity,
                                                alpha=alpha,
                                                beta=beta,
                                                gamma=gamma,
                                                forecastperiod=forecastperiod)))
	temp1 <- createTable(pTableName=gen_unique_table_name("HoltWinters"),pSelect=t)
	pSelect<-paste0("Select GroupID, PeriodID, Num_Val, periodicity,
					alpha, beta, gamma, forecastperiod from ",temp1)
	query<-constructUDTSQL(pViewColnames=c(pGroupID="GroupID",
										   pNum_Val="Num_Val",
										   pPeriodicity="periodicity",
						   				   pAlpha="alpha",
						   				   pBeta="beta",
						   				   pGamma="gamma",
						   				   pNormal=normalization,
						   				   pForecastPeriod=forecastperiod),
						   pSelect=pSelect,
						   pOutColnames=c("a.*"),
						   pFuncName="FLHoltWintersUdt",
						   pLocalOrderBy=c("pGroupID"),
						   pNest=TRUE)
	temp2 <- createTable(pTableName=gen_unique_table_name("temp"),pSelect=query)
	ret <- sqlQuery(getFLConnection(),paste0("Select * from ",temp2," order by 2"))
	return(ret)
}
