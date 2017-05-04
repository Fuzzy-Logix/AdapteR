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
#' vdf <- sqlQuery(connection, "SELECT Num_Val as num_val FROM tblsensex")
#' flv <- as.FL(vdf$num_val)
#' rv <- vdf$num_val
#' flmod <- PP.test(flv)
#' @export
PP.test.FLVector<-function(x,
                           data,
                           trend=1,
                           lag = 1,
                           ...){
    ##if(!is.FLVector(object)) stop("The class of the object should be FLVector.")
    vviewname <- gen_view_name("pp.test")
    vtbl <- createView(pViewName = vviewname,
                       pSelect = constructSelect(x))
    vinputcols<-list(TableName=vviewname,
                     ObsIDCol="VectorindexColumn",
                     ValueCol="VectorValueColumn",
                     Trend=trend,
                     Lag=lag,
                     Note="FLPP FROM AdapteR")
    vfuncName<-"FLPP"
    dname <- as.character(match.call())[[2]]
    AnalysisID<-sqlStoredProc(getFLConnection(),
                              vfuncName,
                              outputParameter=c(AnalysisID="a"),
                              pInputParameters=vinputcols)
    ret<-sqlQuery(getFLConnection(),
                paste0("select * from fzzlPPStats where AnalysisID = ", 
                    fquote(AnalysisID[1,1])))
    colnames(ret) <- tolower(colnames(ret))
    
    ##statistic<-ret[1,4]
    ##parameter<-k
    statistic = ret$z_t_alpha
    parameter=2
    pval <- ret$pval_ztalpha
    ret <- ret[,!colnames(ret)%in%c("z_t_alpha","pval_ztalpha","analysisid")]
    ret <- as.list(ret)
    names(statistic)<-"Dickey-Fuller"
    names(parameter)<-"Truncation lag parameter"
    vreturn <- structure(c(list(statistic = statistic,
                              parameter=parameter,
                              ##alternative="stationary",
                              p.value=pval,
                              method="Phillips-Perron Unit Root Test",
                              data.name=dname),
                            ret),
                         class  = c("htest")
                        )
    dropView(vviewname) 
    return(vreturn)   
}

# `$.FLPP`<-function(object,property){
#     parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
#                         "(",fixed=T))[2],",",fixed=T))[1]
#     if(property == "statistic"){
#         return(object$dft)}
#     if(property == "method"){
#         return("Phillips-Perron Unit Root Test")}
#     if(property == "parameter"){
#         return(0)}
#     if(property == "p.value"){
#         return(object$Pval)      
# } }
