#' @export
arima<-function(object,...){
	UseMethod("arima",object)
}

#' @export
arima.default  <- function (object,...){
    return(stats::arima(object,...))
}

#' @export
arima.FLVector<-function(object,
						 order=c(1,0,0),...){ #browser()
	if(!is.FLVector(object)) stop("The class of the input object should be FLVector")
	if(any(order)<0) stop("The p, d and q values should be positive integers")

	t <- constructUnionSQL(pFrom = c(a = constructSelect(object)),
                           pSelect = list(a = c(GroupID = 1,
                                                Num_Val = "a.vectorValueColumn",
                                                p=order[1],
                                                d=order[2],
                                                q=order[3])))
	temp1 <- createTable(pTableName=gen_unique_table_name("arima"),pSelect=t)
	pSelect<-paste0("Select * from ",temp1)
	query<-constructUDTSQL(pViewColnames=c(pGroupID="GroupID",
						   				   pNum_Val="Num_Val",
						   				   p="p",
						   				   d="d",
						   				   q="q"),
						   pSelect=pSelect,
						   pOutColnames=c("a.*"),
						   pFuncName="FLARIMAUdt",
						   pLocalOrderBy=c("pGroupID"))
	temp2 <- createTable(pTableName=gen_unique_table_name("temp"),pSelect=query)
	ret <- sqlQuery(getFLConnection(),paste0("Select * from ",temp2," order by 2,3"))
	coefFrame<- ret[ret$oParamType=="1",4:5]
	coefnames<-coefFrame[,1]
	coef<-coefFrame[,2]
	names(coef)<-coefnames
	rtobj<-list(coef=coef,
				ret=ret,
				sigma2=ret[ret$oParamName=="SigmaSq",5],
				call=match.call(),
				loglik=ret[ret$oParamName=="Likelihood",5],
				nobs=length(object),
				series="object",
				n.cond=order[1]+order[2])
	class(rtobj)<-"FLArima"
	return(rtobj)
}

print.FLArima<-function(object){ #browser()
	if(!class(object)=="FLArima") stop("The object class should be FLArima")
	cat("\nCall:", deparse(object$call, width.cutoff = 75L), "", sep = "\n")
	s.e.<-object$ret[object$ret$oParamType=="1",6]
	m<-rbind(object$coef,s.e.)
	cat("Coefficients:\n")
	print(m)
	cat(paste0("\nsigma^2 estimated as ",round(object$sigma2,digits=3),":  log likelihood = ",
				round(object$loglik, digits=3)))
}	
