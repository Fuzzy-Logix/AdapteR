FLrpart<-function(data=flt,
				  formula,
				  control=list(minsplit=10,
				  maxdepth=5,
				  cp=0.95),
				  method="class",
				  ...){
	if(flt@isDeep){
		if(control["cp"]>1 || control["cp"]<0) stop("cp should be between 0 and 1")
		if(!class(formula)=="formula") stop("Please enter a valid formula")
		else{
			if(!isDotFormula(formula)){
				vcolnames<-colnames(flt)
				vexclude<-setdiff(vcolnames,all.vars(formula))}
			else{
				stop("Still developing")
			}
		}
		tablename<-flt@select@table_name
		deeptablename<-createView(pViewName=gen_view_name("New"),
							  	  pSelect=constructSelect(flt))
		vnote<-genNote("abc")
		vcolnames<- flt@select@variables
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
		else stop("not available for wide tables yet")
}