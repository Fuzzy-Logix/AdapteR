naiveBayes <- function (formula,data=list(),...) {
	UseMethod("naiveBayes", data)
}

#' @export
naiveBayes.default <- function (formula,data=list(),...) {
    if (!requireNamespace("rpart", quietly = TRUE)){
        stop("e1071 package needed for naiveBayes. Please install it.",
             call. = FALSE)
    }
    else return(e1071::naiveBayes(formula,data,...))
}

naiveBayes.FLTable <- function(object,formula,laplace=0,...){
	#browser()
	deepx<-tableformat(object,formula)
	vobsid <- getVariables(deepx)[["obs_id_colname"]]
	vvarid <- getVariables(deepx)[["var_id_colname"]]
	vvalue <- getVariables(deepx)[["cell_val_colname"]]
	vinputcols<-list(INPUT_TABLE=deepx@select@table_name,
			  		OBSID=vobsid,
			  		VARID=vvarid,
			  		VALUE=vvalue,
			  		laplace=laplace,
			  		NOTE="Naive Bayes model")
	vfuncName<-"FLNaiveBayesModel"
	AnalysisID <- sqlStoredProc(getFLConnection(),
								vfuncName,
								outputParameter=c(AnalysisID="a"),
								pInputParams=vinputcols)
	frame <- sqlQuery(getFLConnection(),paste0("Select * from fzzlNaiveBayesModel
									 where AnalysisID = ",fquote(AnalysisID)," order by 2,3,4"))
	vars <- unique(frame$VarID)
	levels <- unique(frame$ClassValue)
	tables<-list()
	for(i in vars){
	  subframe<-frame[frame$VarID==i,]
	  obj<-table(subframe$VarValue,subframe$ClassValue)
	  for(j in 1:nrow(obj)){
	  	obj[j,2]<-subframe[2*j,"ClassVarCount"]/(subframe[2*j,"ClassVarCount"]+subframe[2*j - 1,"ClassVarCount"])
	  	obj[j,1]<-subframe[2*j -1,"ClassVarCount"]/(subframe[2*j,"ClassVarCount"]+subframe[2*j - 1,"ClassVarCount"])
	  }
	  vname<-paste0("V",i)
	  names(dimnames(obj))<-c("Y",vname)
	  eval(parse(text=paste0("tables$V",i,"<-obj")))
	}
	apriori<-sqlQuery(connection,paste0("select ",vvalue,", count(*) from ",
					deepx@select@table_name," where ",vvarid," = -1 group by ",vvalue))

	retobj<-list(apriori=apriori,
				 tables=tables,
				 levels=levels,
				 call=match.call(),
				 formula=formula,
				 AnalysisID=AnalysisID)
	class(retobj)<-"naiveBayes"
	return(retobj)
}

predict.naiveBayes<-function(object,newdata,scoreTable="",...){
	if(!is.FLTable(newdata)) stop("scoring allowed on FLTable only")
	if(scoreTable=="")
	scoreTable <- gen_score_table_name("NaiveBayes")
	newdatatable<-tableformat(newdata,formula=object$formula)
	newdatatable <- setAlias(newdatatable,"")
	tablename<- getTableNameSlot(newdatatable)
	vobsid <- getVariables(newdatatable)[["obs_id_colname"]]
	vvarid <- getVariables(newdatatable)[["var_id_colname"]]
	vvalue <- getVariables(newdatatable)[["cell_val_colname"]]
	vinputcols<-list()
	vinputcols <- c(vinputcols,
					TableName=tablename,
					ObsIDCol=vobsid,
					VarIDCol=vvarid,
					ValueCol=vvalue,
					InAnalysisID=object$AnalysisID,
					ScoreTable=scoreTable,
					NOTE="NaiveBayes predict")
	vfuncName<-"FLNaiveBayesPredict"
	AnalysisID <- sqlStoredProc(getFLConnection(),
								vfuncName,
								outputParameter=c(AnalysisID="a"),
								pInputParams=vinputcols)
	ret<-sqlQuery(getFLConnection(),paste0("Select * from ",scoreTable))
	return(ret)
}