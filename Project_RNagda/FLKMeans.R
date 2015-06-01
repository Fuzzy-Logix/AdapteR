setOldClass("RODBC") 
library(stats)
library(RODBC)
setClass(
	"FLTable", 
	slots = list(	
		odbc_connection = "RODBC", 
		db_name         = "character", 
		table_name      = "character",
		primary_key="character"
	)
)

gen_deep_table_name <- function(TableName){
	random_no <- rnorm(1);
	paste(TableName,"Deep",round(as.numeric(Sys.time())),round(random_no*random_no*10000000),sep="_");
}
trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

FLTable <- function(connection, database, table,primary_key=character(0)) {

	# validate_args( 	list(database = database, table = table), 
	# 				list(database = "character", table = "character")
	# )
	sqlQuery(connection, paste("DATABASE", database))
	sqlQuery(connection, "SET ROLE ALL")

	new("FLTable", odbc_connection = connection,db_name = database, table_name = table,primary_key = primary_key)
}

names.FLTable <- function(object){
		connection = object@odbc_connection
		column_database = "dbc"
		sqlQuery(connection, paste("DATABASE", column_database))
		sqlQuery(connection, "SET ROLE ALL")
		sqlstr = paste0("SELECT columnname FROM dbc.columns WHERE tablename='",object@table_name,"' AND databasename='",object@db_name,"';")
		retobj = sqlQuery(connection,sqlstr)
		retobj<-trim(as.vector(retobj$ColumnName))
}

setClass(
	"FLKMeans",
	slots=list(
		no_of_centers="numeric",
		AnalysisID="character",
		odbc_connection="RODBC",
		table_name="character",
		clusterfetched="logical",
		centerfetched="logical",
		cluster="vector",
		centers="matrix",
		deeptablename="character"
	)
)

#overloading kmeans
kmeans <- function (x, ...) {

	UseMethod("kmeans", x)
 }

#kmeans performs normally for data frames
kmeans.data.frame<-stats::kmeans

#kmeans implementation for an FLKMeans object. Isdeep is a temporary argument, for testing. 
kmeans.FLTable<-function(table,centers,max.iter,isDeep=FALSE){

	database<-table@db_name
	sqlQuery(connection, paste("DATABASE", database))
	sqlQuery(connection, "SET ROLE ALL")
	if(!isDeep){
		deeptablename <- gen_deep_table_name(table@table_name)

		ret<-sqlQuery(table@odbc_connection,paste0("CALL FLWideToDeep('",table@table_name,"', '",table@primary_key,"', '",deeptablename,"', 'ObsID', 'VarID','Num_Val', NULL, NULL, NULL, AnalysisID);"))
	}
	else deeptablename<-table@table_name
	retobj<-sqlQuery(table@odbc_connection,paste("CALL FLKMeans( '",deeptablename,"','ObsID', 'VarID', 'Num_Val',NULL, ",centers," , ", max.iter, " , 1, 'KMeans, clusters=2, maxiter=20, hypothesis=2', AnalysisID );"))
	AnalysisID=as.character(retobj[1,1])
	new("FLKMeans",
		no_of_centers=centers,
		AnalysisID=AnalysisID,
		odbc_connection=table@odbc_connection,
		table_name=table@table_name,
		clusterfetched=FALSE,
		centerfetched=FALSE,
		deeptablename=deeptablename
	)
}

#Overloading the $ operator for an FLKMeans object. 

`$.FLKMeans`<-function(object,property){
	if(property=="cluster"){
		cluster(object)
	}
	else if(property=="centers"){
		centers(object)
	}
	else if(property=="tot.withinss"){
		tot.withinss(object)
	}
	else if(property=="betweenss"){
		betweenss(object)
	}
	else if(property=="totss"){
		totss(object)
	}
	else if(property=="withinss"){
		withinss(object)
	}
	else if(property=="size"){
		size(object)
	}
	else "That's not a valid property"
}

#Function to retrieve the cluster vector.

cluster <- function (x, ...) {
   UseMethod("cluster", x)
 }

cluster.FLKMeans<-function(object){
	if(object@clusterfetched){
		object@cluster
	}
	else{
		connection=object@odbc_connection
		AnalysisID=object@AnalysisID
		sqlstr<-paste0("SELECT ClusterID FROM fzzlKMeansClusterID WHERE AnalysisID = '",AnalysisID,"' ORDER BY ObsID;")
		retobj=sqlQuery(connection,sqlstr)
		clustervector<-as.vector(retobj$ClusterID)
		object<-object
		object@clusterfetched<-TRUE
		object@cluster<-clustervector
		clustervector<-as.integer(substr(clustervector,nchar(clustervector),nchar(clustervector)))
		clustervector
	}
}

#Function to retrieve the coordinates of the centroids.

centers <- function (x, ...) {
   UseMethod("centers", x)
 }

centers.FLKMeans<-function(object){
	connection=object@odbc_connection
	AnalysisID=object@AnalysisID
	sqlstr<-paste0("SELECT Centroid FROM fzzlKMeansDendrogram WHERE AnalysisID = '",AnalysisID,"' ORDER BY ClusterID,VarID;")
	retobj=sqlQuery(connection,sqlstr)
	centers<-as.vector(retobj$Centroid)

	row=object@no_of_centers
	col=length(centers)/row


	centers<-matrix(centers,nrow=row,ncol=col,byrow=TRUE)
}

#overloading the print function
print.FLKMeans<-function(object){
	clustervector<-cluster(object)
	centermatrix<-centers(object)
	i<-1
	centercount<-""
	while(i<=object@no_of_centers){
		centercount<-paste0(centercount,table(clustervector)[i],", ")
		i<-i+1
	}
	cat(paste0("K-Means clustering with ",object@no_of_centers," clusters of sizes ",substr(centercount,1,nchar(centercount)-2)," \n\nCluster Means:\n"))
	print(centermatrix)
	cat("\nClustering vector:\n")
	print(clustervector)
	cat("\nWithin cluster sum of squares by cluster\n")
	print(withinss(object))
	cat("(between_SS / total_SS = ")
	cat((100*betweenss(object))/totss(object))
	cat(" %)\n")
	cat("\nAvailable components\n")
	print(c("cluster","centers","totss","withinss","tot.withinss","betweenss","size"))
}

#overloading show. Because apparently that is the default function called when you just type the object name. 
setMethod("show","FLKMeans",print.FLKMeans)


#total within sum of squares.
tot.withinss<-function(object,...){
	UseMethod("tot.withinss",object)
}

tot.withinss.FLKMeans<-function(object){
	sqlstr<-paste0("SELECT sum(power((",object@deeptablename,".Num_Val - fzzlKMeansDendrogram.Centroid ),2)) FROM fzzlKMeansClusterID,",object@deeptablename,",fzzlKMeansDendrogram WHERE fzzlKMeansDendrogram.AnalysisID = '",object@AnalysisID,"' AND fzzlKMeansClusterID.AnalysisID = '",object@AnalysisID,"' AND ",object@deeptablename,".VarID=fzzlKMeansDendrogram.VarID AND fzzlKMeansClusterID.ClusterID = fzzlKMeansDendrogram.ClusterID AND fzzlKMeansClusterID.ObsID = ",object@deeptablename,".ObsID")
	result<-sqlQuery(object@odbc_connection,sqlstr)
	result[1,1]
}

withinss<-function(object){
	UseMethod("withinss",object)
}

withinss.FLKMeans<-function(object){
	sqlstr<-paste0("SELECT sum(power((",object@deeptablename,".Num_Val - fzzlKMeansDendrogram.Centroid ),2)) as withinssvector FROM fzzlKMeansClusterID,",object@deeptablename,",fzzlKMeansDendrogram WHERE fzzlKMeansDendrogram.AnalysisID = '",object@AnalysisID,"' AND fzzlKMeansClusterID.AnalysisID = '",object@AnalysisID,"' AND ",object@deeptablename,".VarID=fzzlKMeansDendrogram.VarID AND fzzlKMeansClusterID.ClusterID = fzzlKMeansDendrogram.ClusterID AND fzzlKMeansClusterID.ObsID = ",object@deeptablename,".ObsID GROUP BY fzzlKMeansClusterID.ClusterID ORDER BY fzzlKMeansClusterID.ClusterID")
	result<-sqlQuery(object@odbc_connection,sqlstr)
	result$withinssvector
}

betweenss<-function(object){
	UseMethod("betweenss",object)
}

betweenss.FLKMeans<-function(object){
	sqlstr<-paste0("SELECT sum(power((a.valavg - fzzlKMeansDendrogram.Centroid),2)) FROM (SELECT VarID,average(",object@deeptablename,".Num_Val) as valavg FROM ",object@deeptablename," Group By VarID) as a, fzzlKMeansClusterID, fzzlKMeansDendrogram WHERE fzzlKMeansDendrogram.AnalysisID = '",object@AnalysisID,"' AND fzzlKMeansClusterID.AnalysisID = '",object@AnalysisID,"' AND ",object@deeptablename,".VarID=fzzlKMeansDendrogram.VarID AND fzzlKMeansClusterID.ClusterID = fzzlKMeansDendrogram.ClusterID AND fzzlKMeansClusterID.ObsID = ",object@deeptablename,".ObsID AND a.VarID = ",object@deeptablename,".VarID")
	result<-sqlQuery(object@odbc_connection,sqlstr)
	result[1,1]
}

totss<-function(object){
	UseMethod("totss",object)
}

totss.FLKMeans<-function(object){
	sqlstr<-paste0("Select sum(power((",object@deeptablename,".Num_Val - a.valavg),2)) FROM (SELECT VarID,average(",object@deeptablename,".Num_Val) as valavg FROM ",object@deeptablename," Group By VarID) as a, ",object@deeptablename," WHERE a.VarID = ",object@deeptablename,".VarID;")
	result<-sqlQuery(object@odbc_connection,sqlstr)
	result[1,1]
}

size<-function(object){
	UseMethod("size",object)
}

size.FLKMeans<-function(object){
	clustervector=cluster(object)
	sizevector<-c()
	i<-1
	while(i<=object@no_of_centers){
		sizevector[i]<-table(clustervector)[i]
		i<-i+1
	}
	sizevector	
}

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
	print(object@formula)	
	cat(", data = TODO)")
	cat("\n\nCoefficients:\n")	
}

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