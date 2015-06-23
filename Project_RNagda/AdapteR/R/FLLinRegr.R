#beginning code for linear model

#return objects are of class "FLLinRegr"
setClass(
	"FLLinRegr",
	slots=list(
		formula="formula",
		table_name="character",
		deeptablename="character",
		AnalysisID="character",
		dataprepID="character",
		datatable="FLTable"
	)
)

#overloading lm
lm <- function (formula,data,...) {
	UseMethod("lm", data)
 }

#lm performs normally for data frames
lm.data.frame<-stats::lm

lm.FLTable<-function(formula,data,...){
	dependent <- all.vars(formula)[1]
	independents <- all.vars(formula)[2:length(formula)]
	cols<-names(data)

	#unused_cols represents the columns that are not to be included in the regression analysis
	unused_cols <- cols[!cols %in% all.vars(formula)]
	unused_cols <- unused_cols[unused_cols!=data@primary_key]

	unused_cols_str <- ""
	for(i in 1:length(unused_cols)){
		unused_cols_str <- paste0(unused_cols_str,unused_cols[i],", ")
		i<-i+1
	}
	deeptablename <- gen_deep_table_name(data@table_name)
	unused_cols_str <- substr(unused_cols_str,1,nchar(unused_cols_str)-2)
	sqlQuery(data@odbc_connection,paste0("DATABASE ",data@db_name))
	sqlQuery(data@odbc_connection,"SET ROLE ALL")
	sqlstr<-paste0("CALL FLRegrDataPrep('",data@table_name,"','",data@primary_key,"','",dependent,"','",deeptablename,"','ObsID','VarID','Num_Val',0,0,0,0,0,0,0,'",unused_cols_str,"',NULL,NULL,NULL,AnalysisID);")
	dataprepID <- as.vector(retobj<-sqlQuery(data@odbc_connection,sqlstr)[1,1])

	AnalysisID<-as.vector(sqlQuery(data@odbc_connection,paste0("CALL FLLinRegr('",deeptablename,"', 'ObsID', 'VarID', 'Num_Val', 'Test', AnalysisID);"))[1,1])
	
	new("FLLinRegr",
		formula=formula,
		table_name=data@table_name,
		deeptablename=deeptablename,
		AnalysisID=AnalysisID,
		dataprepID=dataprepID,
		datatable=data
	)
}

`$.FLLinRegr`<-function(object,property){
	if(property=="coefficients"){
		coefficients(object)
	}
	else if (property=="residuals"){
		residuals(object)
	}
	else "That's not a valid property"
}

lmdata<-function(table){
	UseMethod("lmdata",table)
}

lmdata.FLLinRegr<-function(object){
	sqlstr<-paste0("SELECT a.Column_Name, a.Final_VarID FROM fzzlRegrDataPrepMap a WHERE a.AnalysisID = '",object@dataprepID,"' ORDER BY a.Final_VarID;")
	mapframe<-sqlQuery((object@datatable)@odbc_connection,sqlstr)
	
	# Converting the data frame into a namedvector
	mapList<-as.numeric(mapframe$Final_VarID)
	names(mapList)<-as.character(mapframe$COLUMN_NAME)

	sqlstr2<-paste0("SELECT a.* FROM fzzlLinRegrCoeffs a WHERE a.AnalysisID = '",object@AnalysisID,"' order by COEFFID;")
	coeffframe<-sqlQuery(object@datatable@odbc_connection,sqlstr2)
	retlist<-list(mapList = mapList, coeffframe = coeffframe)
	retlist
}

print.FLLinRegr<-function(object){
	cat("CALL\n")
	cat("lm.FLLinRegr(formula = ")
	cat(deparse(object@formula))	
	cat(", data = TODO)")
	cat("\n\nCoefficients:\n")
	coefficients(object)	
}

#overloading show.
setMethod("show","FLLinRegr",print.FLLinRegr)

coefficients<-function(table){
	UseMethod("coefficients",table)
}

coefficients.FLLinRegr<-function(object){
	data<-lmdata(object)
	coeffvector<-data$coeffframe$COEFFVALUE
	for(i in 1:length(data$coeffframe$COEFFVALUE)){
		j<-data$coeffframe$COEFFID[i]
		names(coeffvector)[i] = names(data$mapList[data$mapList==j])
	}
	coeffvector
}

summary.FLLinRegr<-function(object){
	data<-lmdata(object)
	coeffframe<-data$coeffframe
	for(i in 1:length(coeffframe$COEFFVALUE)){
		j<-coeffframe$COEFFID[i]
		rownames(coeffframe)[i] = names(data$mapList[data$mapList==j])
	}
	coeffframe<-coeffframe[4:7]
	colnames(coeffframe)<-c("Estimate","Std. Error","t value","Pr(>|t|)")
	cat("CALL\n")
	cat("lm.FLLinRegr(formula = ")
	cat(deparse(object@formula))
	cat("\nResiduals:TODO")
	cat("\nCoefficients:\n")
	print(coeffframe)
	cat("\n---\n")
	print("Signif. codes 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 '' 1")
	sqlstr<-paste0("SELECT a.* FROM fzzlLinRegrStats a WHERE a.AnalysisID = '",object@AnalysisID,"';")
	return<-sqlQuery(object@datatable@odbc_connection,sqlstr)
	cat("Residual standard error: ",return[15]$MSRESIDUAL," on ",return[9]$DFRESIDUAL," degrees of freedom\n")
	cat("Multiple R-squared: ",return[4]$RSQUARED," , Adjusted R-squared: ",return[5]$ADJRSQUARED,"\n")
	FStatPVal<-pf(return[16]$FSTAT,return[8]$DFREGRESSION,return[9]$DFRESIDUAL,lower.tail=FALSE)
	cat("F-statistic: ",return[16]$FSTAT," on ",return[8]$DFREGRESSION," and ",return[9]$DFRESIDUAL," , p-value: ",FStatPVal,"\n")
}