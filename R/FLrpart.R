# setClass(
# 	"FLrpart",
# 	slots=list(frame="list",
# 			   method="character",
# 			   control="list",
# 			   where="integer",
# 			   call="call",
# 			   AnalysisID="character",
# 			   deeptable="FLTable",
# 			   prepspecs="list"))

FLrpart<-function(data,
				  formula,
				  control=rpart.control(minsplit=10,
				  maxdepth=5,
				  cp=0.95),
				  method="class",
				  ...){#browser()
	call<-match.call()
	if(!class(formula)=="formula") stop("Please enter a valid formula")
	if(control["cp"]>1 || control["cp"]<0) stop("cp should be between 0 and 1")
	if(!class(formula)=="formula") stop("Please enter a valid formula")
	if(data@isDeep){
		deepx<-data
		deepx<-setAlias(deepx,"")	
		deeptablename<-data@select@table_name
		vprepspecs<-list()	
	}
	else{
		if(!isDotFormula(formula)){
			data <- setAlias(data,"")
			vcolnames<-colnames(data)		
			vexclude<-setdiff(vcolnames,all.vars(formula))
			obs<-getVariables(data)[["obs_id_colname"]]
			vexclude<-setdiff(vexclude,obs)
			vexclude<-paste0(vexclude,collapse=",")
		}
		else{
			vexclude<-NULL
		}
			depCol<-all.vars(formula)[1]
			vprepspecs<-list(depCol,vexclude)
			deep<-FLRegrDataPrep(data,depCol=depCol,excludeCols=vexclude)
			deepx<-deep[["table"]]
			deepx<- setAlias(deepx,"")
			deeptablename<-deepx@select@table_name
	}
	vobsid <- getVariables(deepx)[["obs_id_colname"]]
	vvarid <- getVariables(deepx)[["var_id_colname"]]
	vvalue <- getVariables(deepx)[["cell_val_colname"]]
	vnote<-genNote("abc")
	vcolnames<- deepx@select@variables
	vinputcols<-c(INPUT_TABLE=deeptablename,
				  OBSID=vobsid,
				  VARID=vvarid,
				  VALUE=vvalue,
				  MINOBS=control["minsplit"],
				  MAXLEVEL=control["maxdepth"],
				  PURITY=control["cp"],
				  NOTE=vnote)
	vfuncName<-"FLDecisionTreeMN"
	retobj<-sqlStoredProc(getFLConnection(),
						  vfuncName,
						  outputParameter=c(AnalysisID="a"),
						  pInputParameters=vinputcols)

	AnalysisID<-as.character(retobj[1,1])
	sql<-paste0("Select * from fzzlDecisionTreeMN where AnalysisID = ",fquote(AnalysisID)," Order by 1,2,3")
	ret<-sqlQuery(connection,sql)
	frame<-data.frame(NodeID=ret$NodeID,
					  n=ret$NodeSize,
					  prob=ret$PredictClassProb,
					  yval=ret$PredictClass,
					  var=ret$SplitVarID,
					  SplitVal=ret$SplitVal,
					  leftson=ret$ChildNodeLeft,
					  rightson=ret$ChildNodeRight)
	 
	retobj<- list(frame=frame,
			 	  method=method,
			 	  control=control,
			 	  where=ret$NodeID,
				  call=call,
				  deeptable=deepx,
				  AnalysisID=AnalysisID,
				  prepspecs=vprepspecs)
	class(retobj)<-"FLrpart"
	return(retobj)
}

summary.FLrpart<-function(x,...){
	cat("Call:\n")
	dput(x$call)
	cat(" n=",x$frame$n[1],"\n\n")
	## should have cp table here which we do not get from DBLytix
	for(i in 1:length(x$frame$NodeID)){
		cat("\n\nNode number ", x$frame$NodeID[i], ": ", x$frame$n[i], " observations", sep = "")
		cat("\n predicted class=",x$frame$yval[i])
		cat("\n probability:",x$frame$prob[i])
		if(!is.null(x$frame$SplitVal[i])&&!is.na(x$frame$SplitVal[i])){
			cat("\n left son=",x$frame$leftson[i],"  right son=",x$frame$rightson[i])
			cat("\n Split:")
			cat("\n ",x$frame$var[i]," < ",x$frame$SplitVal[i])
		}
	}

}

predict.FLrpart<-function(object,
						  newdata=object$deeptable,
						  scoreTable="",
						  ...){#browser()
	if(!is.FLTable(newdata)) stop("Only allowed for FLTable")
	newdata <- setAlias(newdata,"")
	if(scoreTable=="")
	scoreTable<-gen_score_table_name(object$deeptable@select@table_name)

	if(!newdata@isDeep){
		deepx<-FLRegrDataPrep(newdata,
							  depCol=object$prepspecs$depCol,
							  excludeCols=object$prepspecs$vexclude)
		newdata<-deepx[["table"]]
		newdata<-setAlias(newdata,"")
	}
	vtable <- newdata@select@table_name
	vobsid <- getVariables(newdata)[["obs_id_colname"]]
	vvarid <- getVariables(newdata)[["var_id_colname"]]
	vvalue <- getVariables(newdata)[["cell_val_colname"]]

	vinputcols <- c(INPUT_TABLE=newdata@select@table_name,
					OBSID_COL=vobsid,
					VARID_COL=vvarid,
					VALUE_COL=vvalue,
					ANALYSISID=object$AnalysisID,
					OUTPUT_TABLE=scoreTable,
					NOTE=genNote("Score"))
	vfuncName<-"FLDecisionTreeMNScore"
	AnalysisID<-sqlStoredProc(getFLConnection(),
							  vfuncName,
							  outputParameter=c(AnalysisID="a"),
						 	  pInputParameters=vinputcols)
	AnalysisID <- checkSqlQueryOutput(AnalysisID)
	query<-paste0("Select * from ",scoreTable," Order by 1")
	result<-sqlQuery(getFLConnection(),query)
	return(result)
}

print.FLrpart<-function(object){
	retobj<-list(frame=object$frame,
			     method=object$method,
			   	 control=object$control,
			  	 where=object$where,
			     call=object$call)
	class(retobj)<-"rpart"
	print(retobj)
}

# `$.FLrpart` <- function(object,property){
# 	return(slot(object,property))
# }
setMethod("show","FLrpart",print.FLrpart)
