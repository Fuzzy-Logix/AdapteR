#' @export
NULL

#' ARIMA Modelling of Time Series
#' 
#' Fit an ARIMA model to a univariate time series.
#'
#' Different definitions of ARMA models have different signs for the AR and/or MA coefficients. The definition used here has
#' X[t] = a[1]X[t-1] +…+ a[p]X[t-p] + e[t] + b[1]e[t-1] +…+ b[q]e[t-q]
#'
#' @param object FLVector
#' @param order A specification of the non-seasonal part of the ARIMA model: 
#' the three integer components (p, d, q) are the AR order, the degree of differencing,
#' and the MA order.
#'
#' @return An object of class "FLArima" containing the coefficient and intercept details.
#' @examples
#' x<-rnorm(1000)
#' flv<-as.FLVector(x)
#' flobj<-arima(object = flv, order=c(3,0,0))

#' @export
arima<-function(object,...){
	UseMethod("arima",object)
}

#' @export
arima.default  <- function (object,...){
    return(stats::arima(object,...))
}

#' @export
arima.FLVector.TDAster<-function(object,
						 order=c(1,0,0),...){ browser()
	if(!is.FLVector(object)) stop("The class of the input object should be FLVector")
	if(any(order<0)) stop("The p, d and q values should be positive integers")

	t <- constructUnionSQL(pFrom = c(a = constructSelect(object)),
                           pSelect = list(a = c(groupid = 1,
                                                obsid = "a.vectorIndexColumn",
                                                num_val = "a.vectorValueColumn",
                                                p=order[1],
                                                d=order[2],
                                                q=order[3])))
	temp1 <- createTable(pTableName=gen_unique_table_name("arima"),
                         pSelect=t,
                         pPrimaryKey="GroupID")
	pSelect<-paste0("Select * from ",temp1)

	query<-constructUDTSQL(pViewColnames=c(pgroupid="groupid",
                                           pobsid="obsid",
						   				   pnum_val="num_val",
						   				   p="p",
						   				   d="d",
						   				   q="q"),
						   pSelect=pSelect,
						   pOutColnames=c("a.*"),
						   pFuncName="FLArimaUdt",
						   pLocalOrderBy=c("pgroupid","pobsid"),
                           pNest=TRUE,
                           ...)

    ## Platform Mapping
    vMap <- getMatrixUDTMapping("FLArimaUdt")
    pOutColnames <- vMap$argsPlatform


    temp2 <- createTable(pTableName=gen_unique_table_name("temp"),
                        pSelect=query,
                        pPrimaryKey=pOutColnames["oGroupID"])
    
    ret <- sqlQuery(getFLConnection(),
                    paste0("Select * from ",temp2," order by 2,3"))

    coefFrame<- ret[ret[[pOutColnames["oParamType"]]]=="1",4:5]
    coefnames<-coefFrame[,1]
    coef<-coefFrame[,2]
    names(coef)<-coefnames
    rtobj<-list(coef=coef,
                ret=ret,
                sigma2=ret[ret[[pOutColnames["oParamName"]]]=="SigmaSq",5],
                call=match.call(),
                loglik=ret[ret[[pOutColnames["oParamName"]]]=="Likelihood",5],
                nobs=length(object)-order[2],
                series="object",
                n.cond=order[1]+order[2]
                )
    class(rtobj)<-"FLArima"
    return(rtobj)
}

#' @export
arima.FLVector.TD<-function(object,
                            order=c(1,0,0),...){

    return(arima.FLVector.TDAster(object=object,
                                order=order,
                                UDTInputSubset=c("pgroupid","pnum_val",
                                                "p","d","q"),
                                ...))
    # if(!is.FLVector(object)) stop("The class of the input object should be FLVector")
    # if(any(order<0)) stop("The p, d and q values should be positive integers")

    # t <- constructUnionSQL(pFrom = c(a = constructSelect(object)),
    #                        pSelect = list(a = c(groupid = 1,
    #                                             obsid = "a.vectorIndexColumn",
    #                                             num_val = "a.vectorValueColumn",
    #                                             p=order[1],
    #                                             d=order[2],
    #                                             q=order[3])))
    # temp1 <- createTable(pTableName=gen_unique_table_name("arima"),
    #                     pSelect=t,
    #                     pPrimaryKey="GroupID")
    # pSelect<-paste0("Select * from ",temp1)
    # query<-constructUDTSQL(pViewColnames=c(pgroupid="groupid",
    #                                        pobsid="obsid",
    #                                        pnum_val="num_val",
    #                                        p="p",
    #                                        d="d",
    #                                        q="q"),
    #                        pSelect=pSelect,
    #                        pOutColnames=c("a.*"),
    #                        pFuncName="FLARIMAUdt",
    #                        pLocalOrderBy=c("pgroupid","pobsid"),
    #                        pNest=TRUE,
    #                        UDTInputSubset=c("pgroupid","pnum_val",
    #                                         "p","d","q")
    #                        )

    # ## Platform Mapping
    # vMap <- getMatrixUDTMapping("FLArimaUdt")
    # pOutColnames <- vMap$argsPlatform


    # temp2 <- createTable(pTableName=gen_unique_table_name("temp"),
    #                     pSelect=query,
    #                     pPrimaryKey=pOutColnames["oGroupID"])
    
    # ret <- sqlQuery(getFLConnection(),
    #                 paste0("Select * from ",temp2," order by 2,3"))

    # coefFrame<- ret[ret[[pOutColnames["oParamType"]]]=="1",4:5]
    # coefnames<-coefFrame[,1]
    # coef<-coefFrame[,2]
    # names(coef)<-coefnames
    # rtobj<-list(coef=coef,
    #             ret=ret,
    #             sigma2=ret[ret[[pOutColnames["oParamName"]]]=="SigmaSq",5],
    #             call=match.call(),
    #             loglik=ret[ret[[pOutColnames["oParamName"]]]=="Likelihood",5],
    #             nobs=length(object)-order[2],
    #             series="object",
    #             n.cond=order[1]+order[2]
    #             )
    # class(rtobj)<-"FLArima"
    # return(rtobj)
}

#' @export
print.FLArima<-function(object){
	if(!class(object)=="FLArima") stop("The object class should be FLArima")
	cat("\nCall:", deparse(object$call, width.cutoff = 75L), "", sep = "\n")
	s.e.<-object$ret[object$ret$oParamType=="1",6]
	m<-rbind(object$coef,s.e.)
	cat("Coefficients:\n")
	print(m)
	cat(paste0("\nsigma^2 estimated as ",round(object$sigma2,digits=3),":  log likelihood = ",
				round(object$loglik, digits=3)))
}	

