FLrpart<-function(data,
				  formula,
				  control=rpart.control(minsplit=10,
				  maxdepth=5,
				  cp=0.95),
				  method="class",
				  ...){
	if(!class(formula)=="formula") stop("Please enter a valid formula")
	if(data@isDeep){
		if(control["cp"]>1 || control["cp"]<0) stop("cp should be between 0 and 1")
		if(!class(formula)=="formula") stop("Please enter a valid formula")
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
	return(ret)	
}