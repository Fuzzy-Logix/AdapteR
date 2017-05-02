#' vdf <- sqlQuery(connection, "SELECT Num_Val FROM tblsensex")
#' flv <- as.FL(vdf$Num_Val)
#' rv <- vdf$Num_Val
#' flmod <- PP.test(flv)
#' test case in test_FLdifference.


#' @export
setClass(
    "FLPP",
    slots=list(results = "list" ))



#' @export
PP.test<-function(object,...){
	UseMethod("PP.test",object)
}

#' @export
PP.test.default<-function (object,...) {
     return(PP.test(object,...))
}

#' @bexport
PP.test.FLTable<-function(object,
                           trend=1,
                           lag = 1,
                           ...){
    browser()
    ##if(!is.FLVector(object)) stop("The class of the object should be FLVector.")
    vinputcols<-list(TableName=getTableNameSlot(object),
                     ObsIDCol=object@select@variables$obs_id_colname,
                     ValueCol="NUM_VAL",
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
                     class  = c("FLPP")
                     )    
    return(vreturn)   
}

`$.FLPP`<-function(object,property){
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

    if(property == "statistic"){
        return(object$dft)}
    if(property == "method"){
        return("Phillips-Perron Unit Root Test")}
    if(property == "parameter"){
        return(0)}
    if(property == "p.value"){
        return(object$Pval)      
} }
 

