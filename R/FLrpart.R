setClass(
	"FLrpart",
	slots=list(frame="list",
			   method="character",
			   control="list",
			   where="integer",
			   call="call"))

FLrpart<-function(data,
				  formula,
				  control=rpart.control(minsplit=10,
				  maxdepth=5,
				  cp=0.95),
				  method="class",
				  ...){
	call<-match.call()
	if(!class(formula)=="formula") stop("Please enter a valid formula")
	if(control["cp"]>1 || control["cp"]<0) stop("cp should be between 0 and 1")
	if(!class(formula)=="formula") stop("Please enter a valid formula")
	if(data@isDeep){	
		tablename<-data@select@table_name
		deepx<-data
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
			deep<-FLRegrDataPrep(data,depCol=depCol,excludeCols=vexclude)
			deepx<-deep[["table"]]
			deepx<- setAlias(deepx,"")
			deeptablename<-deepx@select@table_name
	}
	deeptablename<-createView(pViewName=gen_view_name("New"),
							  	  pSelect=constructSelect(deepx))
	vnote<-genNote("abc")
	vcolnames<- deepx@select@variables
	vinputcols<-c(INPUT_TABLE=deeptablename,
				  OBSID="obs_id_colname",
				  VARID="var_id_colname",
				  VALUE="cell_val_colname",
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
	dropView(deeptablename)
	frame<-list()
	frame$NodeID<-ret$NodeID
	frame$n<-ret$NodeSize
	frame$prob<-ret$PredictClassProb
	frame$yval<-ret$PredictClass
	frame$var<-ret$SplitVarID
	frame$SplitVal<-ret$SplitVal
	frame$leftson<-ret$ChildNodeLeft
	frame$rightson<-ret$ChildNodeRight
	
	## class(outobj)<-"rpart": Amal: Some issues with printing the 'rpart' object. 
	return(new(Class="FLrpart",
			   frame=frame,
			   method=method,
			   control=control,
			   where=ret$NodeID,
			   call=call))
}

summary.FLrpart<-function(x,...){
	cat("Call:\n")
	dput(x@call)
	cat(" n=",x@frame$n[1],"\n\n")
	## should have cp table here which we do not get from DBLytix
	for(i in 1:length(x@frame$NodeID)){
		cat("\n\nNode number ", x@frame$NodeID[i], ": ", x@frame$n[i], " observations", sep = "")
		cat("\n predicted class=",x@frame$yval[i])
		cat("\n probability:",x@frame$prob[i])
		if(!is.null(x@frame$SplitVal[i])&&!is.na(x@frame$SplitVal[i])){
			cat("\n left son=",x@frame$leftson[i],"  right son=",x@frame$rightson[i])
			cat("\n Split:")
			cat("\n ",x@frame$var[i]," < ",x@frame$SplitVal[i])
		}
	}

}