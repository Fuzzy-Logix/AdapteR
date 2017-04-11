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
						   h=7,...){ browser()
	if(!is.FLVector(object)) stop("The class of the input object should be FLVector")
	if(alpha<0 || alpha>1) stop("The alpha value should be between 0 and 1")

	t <- constructUnionSQL(pFrom = c(a = constructSelect(object)),
                           pSelect = list(a = c(GroupID = 1,
                           						PeriodID= "a.vectorIndexColumn",
                                                Num_Val = "a.vectorValueColumn")))
	temp1 <- createTable(pTableName=gen_unique_table_name("croston"),pSelect=t)
	pSelect<-paste0("Select GroupID, PeriodID from ",temp1)
	query<-constructUDTSQL(pViewColnames=c(pGroupID="GroupID",
						   				   pNum_Val="Num_Val"),
						   pSelect=pSelect,
						   pArgs=c(alpha=alpha,
						   		   forecastPeriod=h),
						   pOutColnames=c("a.*"),
						   pFuncName="FLCrostonsUdt",
						   pLocalOrderBy=c("pGroupID"))
	temp2 <- createTable(pTableName=gen_unique_table_name("temp"),pSelect=query)
	ret <- sqlQuery(getFLConnection(),paste0("Select * from ",temp2," order by 2"))
	# coefFrame<- ret[ret$oParamType=="1",4:5]
	# coefnames<-coefFrame[,1]
	# coef<-coefFrame[,2]
	# names(coef)<-coefnames
	# rtobj<-list(coef=coef,
	# 			ret=ret,
	# 			sigma2=ret[ret$oParamName=="SigmaSq",5],
	# 			call=match.call(),
	# 			loglik=ret[ret$oParamName=="Likelihood",5],
	# 			nobs=length(object)-order[2],
	# 			series="object",
	# 			n.cond=order[1]+order[2])
	# class(rtobj)<-"FLcroston"
	return(ret)
}

print.FLcroston<-function(object){ #browser()
	if(!class(object)=="FLcroston") stop("The object class should be FLcroston")
	cat("\nCall:", deparse(object$call, width.cutoff = 75L), "", sep = "\n")
	s.e.<-object$ret[object$ret$oParamType=="1",6]
	m<-rbind(object$coef,s.e.)
	cat("Coefficients:\n")
	print(m)
	cat(paste0("\nsigma^2 estimated as ",round(object$sigma2,digits=3),":  log likelihood = ",
				round(object$loglik, digits=3)))
}	
