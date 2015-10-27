#' @include utilities.R
#' @include data_prep.R
#' @include FLTable.R
NULL
#' An S4 class to represent FLLogRegr
#'
#' @slot formula an object of class 'formula': Model Formulae
#' @slot table_name A character
#' @slot deeptablename A character vector containing name of the deeptable on conversion from a widetable
#' @slot AnalysisID An output character ID from CALL FLLogRegr
#' @slot dataprepID An output character ID from CALL FLRegrDataPrep
#' @slot datatable An object of class FLTable
#' @method print FLLogRegr
#' @param object contains: call,coefficients
#' @method coefficients FLLogRegr
#' @param object a named vector of coefficients
#' @method summary FLLogRegr
#' @param object contains: call,residuals,coefficients,significant codes note and statistical output.
setClass(
	"FLLogRegr",
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

glm <- function (formula,data,rushil,iter,threshold,...) {
	UseMethod("glm", data)
 }

glm.data.frame<-stats::glm

#' Linear Regression.
#'
#' \code{glm} performs linear regression on FLTable objects.
#'
#' The wrapper overloads glm and implicitly calls FLRegrDataPrep and FLLogRegr.
#' @method glm FLTable
#' @param formula A symbolic description of model to be fitted
#' @param data An object of class FLTable
#' @section Constraints:
#' None
#' @return \code{glm} performs linear regression and replicates equivalent R output.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' widetable  <- FLTable(connection, "FL_REV4546", "tblAbaloneWide", "ObsID")
#' glmfit <- glm(Rings~Height+Diameter,widetable)
#' @export
glm.FLTable<-function(formula,data,rushil="binomial",iter=25,threshold=0.1,...){
	if(rushil!="binomial")
	{
		stop("Currently only binomial is supported")
	}
	dependent <- all.vars(formula)[1]
	independents <- all.vars(formula)[2:length(formula)]
	cols<-names(data)

	unused_cols <- cols[!cols %in% all.vars(formula)]
	unused_cols <- unused_cols[unused_cols!=data@primary_key]
	unused_cols_str <- ""
	i<-1
	while(i<=length(unused_cols)){
		unused_cols_str <- paste0(unused_cols_str,unused_cols[i],", ")
		i<-i+1
	}
	deeptablename <- gen_deep_table_name(data@table_name)
	unused_cols_str <- substr(unused_cols_str,1,nchar(unused_cols_str)-2)
	sqlQuery(data@odbc_connection,
			 paste0("DATABASE ",data@db_name,";
			 		 SET ROLE ALL;"))
	sqlstr<-paste0("CALL FLRegrDataPrep('",data@table_name,"',
										'",data@primary_key,"',
										'",dependent,"',
										'",deeptablename,"',
										'ObsID',
										'VarID',
										'Num_Val',
										0,
										0,
										0,
										0,
										0,
										0,
										0,
										'",unused_cols_str,"',
										NULL,
										NULL,
										NULL,
										AnalysisID);")
	dataprepID <- sqlQuery(data@odbc_connection,sqlstr)
	dataprepID <- as.vector(dataprepID[1,1])

	AnalysisID<-as.vector(sqlQuery(data@odbc_connection,
								   paste0("CALL FLLogRegr('",deeptablename,"', 
								   						  'ObsID', 
								   						  'VarID', 
								   						  'Num_Val',
								   						  ",iter,",
								   						  ",threshold,", 
								   						  'Test',
								   						  AnalysisID);"))[1,1])
	AnalysisID<-as.character(AnalysisID)
	new("FLLogRegr",
		formula=formula,
		table_name=data@table_name,
		deeptablename=deeptablename,
		AnalysisID=AnalysisID,
		dataprepID=dataprepID,
		datatable=data,
		dataname = deparse(substitute(data))
	)
}

`$.FLLogRegr`<-function(object,property){
	if(property=="coefficients")
	{
		coefficients(object)
	}
	else if (property=="residuals")
	{
		residuals(object)
	}
	else "That's not a valid property"
}

lrdata<-function(object){
	UseMethod("lrdata",object)
}

residuals.FLLogRegr<-function(object){
	dependent <- all.vars(object@formula)[1]
	scoretable<-gen_score_table_name(object@table_name)
	queryscore<-paste0("CALL FLLogRegrScore ('",object@deeptablename,"',
											 'ObsID',
											 'VarID',
											 'Num_Val',
											 NULL,
											 '",object@AnalysisID,"',
											 '",scoretable,"',
											 'Scoring using model ",object@AnalysisID,"',
											 oAnalysisID);")
	err<-sqlQuery(object@datatable@odbc_connection,queryscore)
	queryresiduals<-paste0("SELECT (a.",dependent," - b.Y) as deviation 
							FROM ",object@table_name," AS a, 
								  (SELECT * 
								   FROM ",scoretable,") AS b 
							WHERE a.ObsID = b.ObsID")
	reslist<-sqlQuery(object@datatable@odbc_connection,queryresiduals)
	options(scipen=999)
	reslist<-as.numeric(reslist[,1])
	reslist
}

lrdata.FLLogRegr<-function(object){
		sqlQuery(object@datatable@odbc_connection,
				 paste("DATABASE",object@datatable@db_name))
		sqlstr<-paste0("SELECT a.Column_Name, 
							   a.Final_VarID 
						FROM fzzlRegrDataPrepMap a 
						WHERE a.AnalysisID = '",object@dataprepID,"' 
						ORDER BY a.Final_VarID;")
		mapframe<-sqlQuery((object@datatable)@odbc_connection,sqlstr)


		# Converting the data frame into a namedvector
		mapList<-as.numeric(mapframe$Final_VarID)
		names(mapList)<-as.character(mapframe$COLUMN_NAME)

		sqlstr2<-paste0("SELECT a.* 
						 FROM fzzlLogRegrCoeffs a 
						 WHERE a.AnalysisID = '",object@AnalysisID,"' 
						 ORDER BY COEFFID;")
		coeffframe<-sqlQuery(object@datatable@odbc_connection,sqlstr2)

		dependent <- all.vars(object@formula)[1]
		scoretable<-gen_score_table_name(object@table_name)
		queryscore<-paste0("CALL FLLogRegrScore ('",object@deeptablename,"',
												 'ObsID',
												 'VarID',
												 'Num_Val',
												 NULL,
												 '",object@AnalysisID,"',
												 '",scoretable,"',
												 'Scoring using model ",object@AnalysisID,"',
												 oAnalysisID);")
		err<-sqlQuery(object@datatable@odbc_connection,queryscore)
		queryminmax<-paste0("SELECT FLmin(c.deviation), 
									FLMax(c.deviation) 
							 FROM (SELECT (a.",dependent," - b.Y) AS deviation 
							 	   FROM ",object@table_name," AS a, 
							 	   		 (SELECT * 
							 	   		  FROM ",scoretable,") AS b 
								   WHERE a.ObsID = b.ObsID) AS c")
		minmax<-sqlQuery(object@datatable@odbc_connection,queryminmax)
		query1q<-paste0("WITH z (groupID,deviation) 
						 AS (SELECT 1,
						 			c.deviation 
						 	 FROM ((SELECT (a.",dependent," - b.Y) AS deviation 
						 	 		FROM ",object@table_name," AS a, 
						 	 			  (SELECT * 
						 	 			   FROM ",scoretable,") AS b 
									WHERE a.ObsID = b.ObsID) AS c)) 
							 SELECT q.* 
							 FROM (SELECT a.oPercVal 
							 	   FROM TABLE (FLPercUdt(z.groupID, z.deviation, 0.25) 
							 	   			   HASH BY z.groupID 
							 	   			   LOCAL ORDER BY z.groupID) AS a) AS q")
		oneq<-sqlQuery(object@datatable@odbc_connection,query1q)
		querymedian<-paste0("WITH z (groupID,deviation) 
							 AS (SELECT 1,
							 			c.deviation 
							 	 FROM ((SELECT (a.",dependent," - b.Y) AS deviation 
							 	 	    FROM ",object@table_name," AS a, 
							 	 	    	  (SELECT * 
							 	 	    	   FROM ",scoretable,") AS b 
										WHERE a.ObsID = b.ObsID) AS c)) 
								 SELECT q.* 
								 FROM (SELECT a.oPercVal 
								 	   FROM TABLE (FLPercUdt(z.groupID, z.deviation, 0.5) 
								 	   			   HASH BY z.groupID 
								 	   			   LOCAL ORDER BY z.groupID) AS a) AS q")
		median<-sqlQuery(object@datatable@odbc_connection,querymedian)
		query3q<-paste0("WITH z (groupID,deviation) 
						 AS (SELECT 1,
						 			c.deviation 
						 	 FROM ((SELECT (a.",dependent," - b.Y) AS deviation 
						 	 		FROM ",object@table_name," AS a, 
						 	 			  (SELECT * 
						 	 			   FROM ",scoretable,") AS b 
									WHERE a.ObsID = b.ObsID) AS c)) 
							 SELECT q.* 
							 FROM (SELECT a.oPercVal 
							 	   FROM TABLE (FLPercUdt(z.groupID, z.deviation, 0.75) 
							 	   HASH BY z.groupID 
							 	   LOCAL ORDER BY z.groupID) AS a) AS q")
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

print.FLLogRegr<-function(object){
	cat("Call: ")
	cat("glm.FLLogRegr(formula = ")
	cat(deparse(object@formula))
	cat(", data = ")
	cat(object@dataname)
	cat(",family = \"binomial\")")
	cat("\n\nCoefficients:\n")
	print(coefficients(object))
}

#overloading show.
setMethod("show","FLLogRegr",print.FLLogRegr)

coefficients<-function(table){
	UseMethod("coefficients",table)
}

coefficients.FLLogRegr<-function(object){
	data<-lrdata(object)
	coeffvector<-data$coeffframe$COEFFVALUE
	for(i in 1:length(data$coeffframe$COEFFVALUE)){
		j<-data$coeffframe$COEFFID[i]
		names(coeffvector)[i] = names(data$mapList[data$mapList==j])
	}
	coeffvector
}

summary.FLLogRegr<-function(object){
	data<-lrdata(object)
	coeffframe<-data$coeffframe
	if(!object@datatable@isDeep){
		for(i in 1:length(coeffframe$COEFFVALUE)){
			j<-coeffframe$COEFFID[i]
			rownames(coeffframe)[i] = names(data$mapList[data$mapList==j])
		}
	}

	coeffframe<-coeffframe[4:7]
	colnames(coeffframe)<-c("Estimate","Std. Error","Chi Sq","P Value")
	cat("Call:\n")
	cat("glm.FLLogRegr(formula = ")
	cat(paste0(deparse(object@formula),", data = ",object@dataname," family = \"binomial\")\n"))
	cat("\nDeviance Residuals:\n")
	print(data$residuals)
	cat("\n\nCoefficients:\n")
	print(coeffframe)
	cat("\n---\n")
	cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 '' 1\n")

}

predict<-function(object,new,...){
	UseMethod("predict",object)
}

predict.FLLogRegr<-function(object,new,type="response"){
	if(type!="response"){
		stop("Currently only type=response is supported")
	}
	independents <- all.vars(object@formula)[2:length(object@formula)]
	coeffsum<-c()
	coeffsum[1:nrow(new)] <- 0
	coeffs<-coefficients(object)
	for(i in 1:nrow(new)){
		coeffsum[i]<-coeffs["INTERCEPT"]
		for(j in 1:length(independents)){
			coeffsum[i]<-coeffsum[i] + coeffs[independents[j]]*new[independents[j]][i,1]
			j<-j+1
		}
		i<-i+1
	}
	coeffsum
	return(1/(1+exp(coeffsum*(-1))))
}