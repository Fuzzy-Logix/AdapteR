#' @include utilities.R
#' @include FLTable.R
NULL
#' An S4 class to represent FLKMeans
#'
#' @slot no_of_centers A numeric vector containing the number of clusters, say k
#' @slot AnalysisID A character output used to retrieve the results of analysis
#' @slot odbc_connection ODBC connectivity for R
#' @slot table_name A character
#' @slot clusterfetched A logical vector either TRUE or FALSE
#' @slot centerfetched A logical vector either TRUE or FALSE
#' @slot cluster A vector data type
#' @slot centers A matrix data type
#' @slot deeptablename A character vector containing a deeptable(either conversion from a widetable or IsDeep=TRUE)
#' @method cluster FLKMeans
#' @param object retrieves the cluster vector
#' @method centers FLKMeans
#' @param object retrieves the coordinates of the centroids
#' @method print FLKMeans
#' @param object overloads the print function
#' @method tot.withinss FLKMeans
#' @param object total within sum of squares
#' @method withinss FLKMeans
#' @param object within sum of squares
#' @method betweenss FLKMeans
#' @param object between sum of squares
#' @method totss FLKMeans
#' @param object total sum of squares
#' @method size FLKMeans
#' @param object size vector
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
kmeans <- function (x, ...) {
  UseMethod("kmeans", x)
}
kmeans.data.frame<-stats::kmeans
#' K-Means Clustering.
#'
#' \code{kmeans} performs k-means clustering on FLTable objects.
#'
#' The wrapper overloads kmeans and implicitly calls FLKMeans.
#' @method kmeans FLTable
#' @param table an object of class FLTable
#' @param centers the number of clusters
#' @param max.iter the maximum number of iterations allowed
#' @param isDeep optional for widetable
#' @section Constraints:
#' None
#' @return \code{kmeans} performs k-means clustering and replicates equivalent R output.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' widetable  <- FLTable(connection, "FL_TRAIN", "tblAbaloneWide", "ObsID")
#' kmeans(widetable,3,20)
#' @export
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

setMethod("show","FLKMeans",print.FLKMeans)

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
