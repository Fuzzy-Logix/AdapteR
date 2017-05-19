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
	if(!all(beta)) beta<-0 else beta<-beta
	if(!all(gamma)) gamma<-0 else gamma<-gamma
 	t <- constructUnionSQL(pFrom = c(a = constructSelect(object)),
                           pSelect = list(a = c(groupid = 1,
                           						periodid ="a.vectorIndexColumn",
                                                num_val = "a.vectorValueColumn",
                                                periodicity=periodicity,
                                                alpha=alpha,
                                                beta=beta,
                                                gamma=gamma,
                                                normalization=normalization,
                                                forecastperiod=forecastperiod)))
	temp1 <- createTable(pTableName=gen_unique_table_name("HoltWinters"),
                        pSelect=t,
                        pPrimaryKey="groupid")

    pSelect<-paste0("Select * FROM ",temp1)

	if(!all(gamma) && all(beta)){
		
		pViewColnames<-c(pgroupid="groupid",
                        pperiodid="periodid",
						pnum_val="num_val",
						pperiodicity="periodicity",
						palpha="alpha",
						pbeta="beta",
						pnormal="normalization",
						pforecastperiod="forecastperiod")
		pFuncName<-"FLExpSmooth2FactorUdt"
        vUDTinputSubset <- c("pgroupid","pnum_val","pperiodicity",
                            "palpha","pbeta",
                            "pnormal","pforecastperiod")
	}
	else if(!all(gamma) && !all(beta)){ 
		# pSelect<-paste0("Select GroupID, PeriodID, Num_Val, periodicity, \n ",
		# 				" alpha, forecastperiod from ",temp1)
		pViewColnames<-c(pgroupid="groupid",
                        pperiodid="periodid",
						 pnum_val="num_val",
						 pperiodicity="periodicity",
						 palpha="alpha",
						 pnormal="normalization",
						 pforecastperiod="forecastperiod")
		pFuncName<-"FLExpSmooth1FactorUdt"
        vUDTinputSubset <- c("pgroupid","pnum_val","pperiodicity",
                            "palpha","pnormal","pforecastperiod")
	}
	else {
		# pSelect<-paste0("Select GroupID, PeriodID, Num_Val, periodicity, \n ",
		# 				" alpha, beta, gamma, forecastperiod from ",temp1)
		pViewColnames<-c(pgroupid="groupid",
                        pperiodid="periodid",
						 pnum_val="num_val",
			 			 pperiodicity="periodicity",
						 palpha="alpha",
						 pbeta="beta",
						 pgamma="gamma",
						 pnormal="normalization",
						 pforecastperiod="forecastperiod")
		pFuncName<-"FLHoltWintersUdt"
        vUDTinputSubset <- c("pgroupid","pnum_val","pperiodicity",
                            "palpha","pbeta","pgamma",
                            "pnormal","pforecastperiod")
	}

    vPrimaryKey <- "ogroupid"
    if(inherits(object,"FLVector.TDAster")){
        vUDTinputSubset <- setdiff(vUDTinputSubset,"pgroupid")
        vPrimaryKey <- "partition1"
    }

	query<-constructUDTSQL(pViewColnames=pViewColnames,
						   pSelect=pSelect,
						   pOutColnames=c("a.*"),
						   pFuncName=pFuncName,
						   pLocalOrderBy=c("pgroupid","pperiodid"),
						   pNest=TRUE,
                           UDTInputSubset=vUDTinputSubset)

	temp2 <- createTable(pTableName=gen_unique_table_name("temp"),
                        pSelect=query,
                        pPrimaryKey=vPrimaryKey)
    
	ret <- sqlQuery(getFLConnection(),
                    paste0("Select * from ",
                            temp2," order by 2"))[[4]]

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
