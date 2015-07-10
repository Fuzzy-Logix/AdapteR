#' @include utilities.R
#' @include data_prep.R
#' @include FLTable.R
NULL
#' An S4 class to represent FLLinRegr
#'
#' @slot formula an object of class 'formula': Model Formulae
#' @slot table_name A character
#' @slot deeptablename A character vector containing name of the deeptable on conversion from a widetable
#' @slot AnalysisID An output character ID from CALL FLLinRegr
#' @slot dataprepID An output character ID from CALL FLRegrDataPrep
#' @slot datatable An object of class FLTable
#' @method print FLLinRegr
#' @param object contains: call,coefficients
#' @method coefficients FLLinRegr
#' @param object a named vector of coefficients
#' @method summary FLLinRegr
#' @param object contains: call,residuals,coefficients,significant codes note and statistical output.
setClass(
	"FLLinRegr",
	slots=list(
		formula="formula",
		table_name="character",
		deeptablename="character",
		AnalysisID="character",
		dataprepID="character",
		datatable="FLTable",
		dataname="character"
	)
)

lm <- function (formula,data,...) {
	UseMethod("lm", data)
 }

lm.data.frame<-stats::lm

#' Linear Regression.
#'
#' \code{lm} performs linear regression on FLTable objects.
#'
#' The wrapper overloads lm and implicitly calls FLRegrDataPrep and FLLinRegr.
#' @method lm FLTable
#' @param formula A symbolic description of model to be fitted
#' @param data An object of class FLTable
#' @section Constraints:
#' None
#' @return \code{lm} performs linear regression and replicates equivalent R output.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' widetable  <- FLTable(connection, "FL_REV4546", "tblAbaloneWide", "ObsID")
#' lmfit <- lm(Rings~Height+Diameter,widetable)
#' @export
lm.FLTable<-function(formula,data,...){
		dependent <- all.vars(formula)[1]
		independents <- all.vars(formula)[2:length(formula)]
		cols<-names(data)

		unused_cols <- cols[!cols %in% all.vars(formula)]
		unused_cols <- unused_cols[unused_cols!=data@primary_key]

		unused_cols_str <- ""
		for(i in 1:length(unused_cols)){
			unused_cols_str <- paste0(unused_cols_str,unused_cols[i],", ")
			i<-i+1
		}
	if(!data@isDeep){
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
			datatable=data,
			dataname = deparse(substitute(data))
		)
	}
	else{
		deeptablename <- data@table_name
		unused_cols_str <- substr(unused_cols_str,1,nchar(unused_cols_str)-2)
		sqlQuery(data@odbc_connection,paste0("DATABASE ",data@db_name))
		sqlQuery(data@odbc_connection,"SET ROLE ALL")

		AnalysisID<-as.vector(sqlQuery(data@odbc_connection,paste0("CALL FLLinRegr('",deeptablename,"', '",data@primary_key,"', '",data@var_id_name,"', '",data@num_val_name,"', 'Test', AnalysisID);"))[1,1])

		new("FLLinRegr",
			formula=formula,
			table_name=data@table_name,
			deeptablename=data@table_name,
			AnalysisID=AnalysisID,
			dataprepID=character(0),
			datatable=data,
			dataname = deparse(substitute(data))
		)

	}
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

lmdata<-function(object){
	UseMethod("lmdata",object)
}

lmdata.FLLinRegr<-function(object){
	if(!object@datatable@isDeep){
		sqlQuery(object@datatable@odbc_connection,paste("DATABASE",object@datatable@db_name))
		sqlstr<-paste0("SELECT a.Column_Name, a.Final_VarID FROM fzzlRegrDataPrepMap a WHERE a.AnalysisID = '",object@dataprepID,"' ORDER BY a.Final_VarID;")
		mapframe<-sqlQuery((object@datatable)@odbc_connection,sqlstr)


		# Converting the data frame into a namedvector
		mapList<-as.numeric(mapframe$Final_VarID)
		names(mapList)<-as.character(mapframe$COLUMN_NAME)

		sqlstr2<-paste0("SELECT a.* FROM fzzlLinRegrCoeffs a WHERE a.AnalysisID = '",object@AnalysisID,"' order by COEFFID;")
		coeffframe<-sqlQuery(object@datatable@odbc_connection,sqlstr2)

		dependent <- all.vars(object@formula)[1]
		scoretable<-gen_score_table_name(object@table_name)
		queryscore<-paste0("CALL FLLinRegrScore ('",object@deeptablename,"','ObsID','VarID','Num_Val',NULL,'",object@AnalysisID,"','",scoretable,"','Scoring using model ",object@AnalysisID,"',oAnalysisID);")
		err<-sqlQuery(object@datatable@odbc_connection,queryscore)
		
		queryminmax<-paste0("SELECT FLmin(c.deviation), FLMax(c.deviation) FROM (SELECT (a.",dependent," - b.Y) as deviation FROM ",object@table_name," as a, (SELECT * FROM ",scoretable,") as b WHERE a.ObsID = b.ObsID) as c")
		minmax<-sqlQuery(object@datatable@odbc_connection,queryminmax)
		query1q<-paste0("WITH z (groupID,deviation) AS (SELECT 1,c.deviation FROM ((SELECT (a.",dependent," - b.Y) as deviation FROM ",object@table_name," as a, (SELECT * FROM ",scoretable,") as b WHERE a.ObsID = b.ObsID) AS c)) SELECT q.* FROM(SELECT a.oPercVal FROM TABLE (FLPercUdt(z.groupID, z.deviation, 0.25) Hash by z.groupID LOCAL ORDER BY z.groupID) as a) as q")
		oneq<-sqlQuery(object@datatable@odbc_connection,query1q)
		querymedian<-paste0("WITH z (groupID,deviation) AS (SELECT 1,c.deviation FROM ((SELECT (a.",dependent," - b.Y) as deviation FROM ",object@table_name," as a, (SELECT * FROM ",scoretable,") as b WHERE a.ObsID = b.ObsID) AS c)) SELECT q.* FROM(SELECT a.oPercVal FROM TABLE (FLPercUdt(z.groupID, z.deviation, 0.5) Hash by z.groupID LOCAL ORDER BY z.groupID) as a) as q")
		median<-sqlQuery(object@datatable@odbc_connection,querymedian)
		query3q<-paste0("WITH z (groupID,deviation) AS (SELECT 1,c.deviation FROM ((SELECT (a.",dependent," - b.Y) as deviation FROM ",object@table_name," as a, (SELECT * FROM ",scoretable,") as b WHERE a.ObsID = b.ObsID) AS c)) SELECT q.* FROM(SELECT a.oPercVal FROM TABLE (FLPercUdt(z.groupID, z.deviation, 0.75) Hash by z.groupID LOCAL ORDER BY z.groupID) as a) as q")
		threeq<-sqlQuery(object@datatable@odbc_connection,query3q)
		minmax<- as.numeric(minmax)
		oneq<- as.numeric(oneq)
		three1<- as.numeric(threeq)
		median<-as.numeric(median)
		residuals<-as.numeric(c(minmax[1],oneq,median,threeq,minmax[2]))
		names(residuals)<-c("min","1Q","median","3Q","max")
		retlist<-list(mapList = mapList, coeffframe = coeffframe,residuals = residuals)
		retlist
	}
	else{
		sqlQuery(object@datatable@odbc_connection,paste("DATABASE",object@datatable@db_name))

		sqlstr2<-paste0("SELECT a.* FROM fzzlLinRegrCoeffs a WHERE a.AnalysisID = '",object@AnalysisID,"' order by COEFFID;")
		coeffframe<-sqlQuery(object@datatable@odbc_connection,sqlstr2)

		dependent <- "DEPENDENT"
		scoretable<-gen_score_table_name(object@table_name)
		widetable<-gen_wide_table_name(object@table_name)
		queryscore<-paste0("CALL FLLinRegrScore ('",object@deeptablename,"','ObsID','VarID','Num_Val',NULL,'",object@AnalysisID,"','",scoretable,"','Scoring using model ",object@AnalysisID,"',oAnalysisID);")
		err<-sqlQuery(object@datatable@odbc_connection,queryscore)
		querydeeptowide<-paste0("CALL FLDeepToWide('",object@table_name,"','",object@datatable@primary_key,"','",object@datatable@var_id_name,"','",object@datatable@num_val_name,"',NULL,NULL,'",widetable,"',Message);")
		err2<-sqlQuery(object@datatable@odbc_connection,querydeeptowide)
		queryminmax<-paste0("SELECT FLmin(c.deviation), FLMax(c.deviation) FROM (SELECT (a.",dependent," - b.Y) as deviation FROM ",widetable," as a, (SELECT * FROM ",scoretable,") as b WHERE a.ObsID = b.ObsID) as c")
		minmax<-sqlQuery(object@datatable@odbc_connection,queryminmax)
		query1q<-paste0("WITH z (groupID,deviation) AS (SELECT 1,c.deviation FROM ((SELECT (a.",dependent," - b.Y) as deviation FROM ",widetable," as a, (SELECT * FROM ",scoretable,") as b WHERE a.ObsID = b.ObsID) AS c)) SELECT q.* FROM(SELECT a.oPercVal FROM TABLE (FLPercUdt(z.groupID, z.deviation, 0.25) Hash by z.groupID LOCAL ORDER BY z.groupID) as a) as q")
		oneq<-sqlQuery(object@datatable@odbc_connection,query1q)
		querymedian<-paste0("WITH z (groupID,deviation) AS (SELECT 1,c.deviation FROM ((SELECT (a.",dependent," - b.Y) as deviation FROM ",widetable," as a, (SELECT * FROM ",scoretable,") as b WHERE a.ObsID = b.ObsID) AS c)) SELECT q.* FROM(SELECT a.oPercVal FROM TABLE (FLPercUdt(z.groupID, z.deviation, 0.5) Hash by z.groupID LOCAL ORDER BY z.groupID) as a) as q")
		median<-sqlQuery(object@datatable@odbc_connection,querymedian)
		query3q<-paste0("WITH z (groupID,deviation) AS (SELECT 1,c.deviation FROM ((SELECT (a.",dependent," - b.Y) as deviation FROM ",widetable," as a, (SELECT * FROM ",scoretable,") as b WHERE a.ObsID = b.ObsID) AS c)) SELECT q.* FROM(SELECT a.oPercVal FROM TABLE (FLPercUdt(z.groupID, z.deviation, 0.75) Hash by z.groupID LOCAL ORDER BY z.groupID) as a) as q")
		threeq<-sqlQuery(object@datatable@odbc_connection,query3q)
		minmax<- as.numeric(minmax)
		oneq<- as.numeric(oneq)
		three1<- as.numeric(threeq)
		median<-as.numeric(median)
		residuals<-as.numeric(c(minmax[1],oneq,median,threeq,minmax[2]))
		names(residuals)<-c("min","1Q","median","3Q","max")
		retlist<-list(mapList = list(), coeffframe = coeffframe,residuals = residuals)
		retlist
	}
}

print.FLLinRegr<-function(object){
	cat("Call:\n")
	cat("lm.FLLinRegr(formula = ")
	cat(deparse(object@formula))
	cat(", data = ")
	cat(object@dataname)
	cat(")")
	cat("\n\nCoefficients:\n")
	print(coefficients(object))
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
	if(!object@datatable@isDeep){
		for(i in 1:length(coeffframe$COEFFVALUE)){
			j<-coeffframe$COEFFID[i]
			rownames(coeffframe)[i] = names(data$mapList[data$mapList==j])
		}
	}

	coeffframe<-coeffframe[4:7]
	colnames(coeffframe)<-c("Estimate","Std. Error","t value","Pr(>|t|)")
	cat("Call:\n")
	cat("lm.FLLinRegr(formula = ")
	cat(paste0(deparse(object@formula),", data = ",object@dataname,")\n"))
	cat("\nResiduals:\n")
	print(data$residuals)
	cat("\n\nCoefficients:\n")
	print(coeffframe)
	cat("\n---\n")
	cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 '' 1\n")
	sqlstr<-paste0("SELECT a.* FROM fzzlLinRegrStats a WHERE a.AnalysisID = '",object@AnalysisID,"';")
	ret<-sqlQuery(object@datatable@odbc_connection,sqlstr)
	cat("Residual standard error: ",ret[15]$MSRESIDUAL," on ",ret[9]$DFRESIDUAL," degrees of freedom\n\n")
	cat("Multiple R-squared: ",ret[4]$RSQUARED," , Adjusted R-squared: ",ret[5]$ADJRSQUARED,"\n")
	FStatPVal<-pf(ret[16]$FSTAT,ret[8]$DFREGRESSION,ret[9]$DFRESIDUAL,lower.tail=FALSE)
	cat("F-statistic: ",ret[16]$FSTAT," on ",ret[8]$DFREGRESSION," and ",ret[9]$DFRESIDUAL," , p-value: ",FStatPVal,"\n")

}




predict<-function(object,new,...){
	UseMethod("predict",object)
}

predict.FLLinRegr<-function(object,new){
	independents <- all.vars(object@formula)[2:length(object@formula)]
	retobj<-c()
	retobj[1:nrow(new)] <- 0
	coeffs<-coefficients(object)
	for(i in 1:nrow(new)){
		retobj[i]<-coeffs["INTERCEPT"]
		for(j in 1:length(independents)){
			retobj[i]<-retobj[i] + coeffs[independents[j]]*new[independents[j]][i,1]
			j<-j+1
		}
		i<-i+1
	}
	retobj
}

residuals.FLLinRegr<-function(object){
	dependent <- all.vars(object@formula)[1]
	scoretable<-gen_score_table_name(object@table_name)
	queryscore<-paste0("CALL FLLinRegrScore ('",object@deeptablename,"','ObsID','VarID','Num_Val',NULL,'",object@AnalysisID,"','",scoretable,"','Scoring using model ",object@AnalysisID,"',oAnalysisID);")
	err<-sqlQuery(object@datatable@odbc_connection,queryscore)
	queryresiduals<-paste0("SELECT (a.",dependent," - b.Y) as deviation FROM ",object@table_name," as a, (SELECT * FROM ",scoretable,") as b WHERE a.ObsID = b.ObsID")
	reslist<-sqlQuery(object@datatable@odbc_connection,queryresiduals)
	options(scipen=999)
	reslist<-as.numeric(reslist[,1])
	reslist
}
