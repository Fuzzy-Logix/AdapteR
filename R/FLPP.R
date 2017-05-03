#' @export
PP.test<-function(x,...){
	UseMethod("PP.test",x)
}

#' @export
PP.test.default<-function (x,...) {
     return(stats::PP.test(x,...))
}



#' test case in test_FLdifference.
#' 
#' \code{PP.test} performs Phillips Perron for Stationarity on FLVector objects.
#'
#' The DB Lytix function called is FLPP. Stored procedure to perform Phillips Perron
#' for Stationarity.Stationarity : A weak stationarity condition requires the
#' first moment and the auto-covariance function of the time series to be
#' finite and time invariant stores the results in data frame.
#'
#' @seealso \code{\link[stats]{PP.test}} for R reference implementation.

#' @param data An object of class FLVector.
#' @param trend Whether to include trend order or not.
#' @param Lag Whether to use long or short order for lag of auto-correlation functions.
#' @return \code{PP.test} returns a list of class htest
#' @examples
#' vdf <- sqlQuery(connection, "SELECT Num_Val FROM tblsensex")
#' flv <- as.FL(vdf$Num_Val)
#' rv <- vdf$Num_Val
#' flmod <- PP.test(flv)
#' @export
PP.test.FLVector<-function(x,
                           data,
                           trend=1,
                           lag = 1,
                           ...){
    ##browser()
    ##if(!is.FLVector(object)) stop("The class of the object should be FLVector.")
    vviewname <- gen_view_name("pp.test")
    vtbl <- createView(pViewName = vviewname,
                       pSelect = constructSelect(x))
    vinputcols<-list(TableName=vviewname,
                     ObsIDCol="VectorindexColumn",
                     ValueCol="VectorValueColumn",
                     Trend=trend,
                     Lag=lag)
    vfuncName<-"FLPP"
    dname <- as.character(match.call())[[2]]
    AnalysisID<-sqlStoredProc(getFLConnection(),
                              vfuncName,
                              outputParameter=c(AnalysisID="a"),
                              pInputParameters=vinputcols)
    ret<-sqlQuery(getFLConnection(),paste0("select coeff, stderr, PVal_ZTAlpha as Pval,Z_T_Alpha as dft from fzzlPPStats where AnalysisID = ", fquote(AnalysisID[1,1])))
    ##statistic<-ret[1,4]
    ##parameter<-k
    statistic = ret$dft
    parameter=2
    names(statistic)<-"Dickey-Fuller"
    names(parameter)<-"Truncation lag parameter"
    vreturn <- structure(list(statistic = statistic,
                              parameter=parameter,
                              ##alternative="stationary",
                              p.value=ret$Pval,
                              method="Phillips-Perron Unit Root Test",
                              data.name=dname),
                         class  = c("htest")
                         )    
    return(vreturn)   
}
