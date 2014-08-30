FLkmeans <- function( x,
                      centers,
                      iter.max = 10, 
                      nstart = 1,
                      Note = "From RWrapper For DBLytix"){

	ObsIDColName <- "ObsID";
	VarIDColName <- "VarID";
	ValueColName <- "Num_Val";
	WhereClause <- "";
	DBConnection <- x[["connection"]];
	SQLStr <- paste(	"CALL WorkaroundKMeans('", 
						x[["DeepTableName"]], "',", "'",
						ObsIDColName, "',", "'", 
						VarIDColName, "',", "'", 
						ValueColName, "','", 
						WhereClause, "','", 
						toString(centers), "','", 
						toString(iter.max),"','", 
						toString(nstart),"',", "'", 
						Note,"')", sep="")

	#run KMeans
    KMeansRes <- sqlQuery(DBConnection, SQLStr);
	AnalysisID <- toString(KMeansRes$ANALYSISID);
	
	SQLStr <- paste("SELECT HypothesisID,Level,ClusterID,VarID,Centroid FROM fzzlKMeansDendrogram WHERE AnalysisID = '", AnalysisID,"' ORDER BY 1,2,3,4",sep = "");
	KMeansDendrogram <- sqlQuery(DBConnection, SQLStr);
	
	SQLStr <- paste("SELECT HypothesisID,ObsID,ClusterID FROM fzzlKMeansClusterID WHERE AnalysisID = '", AnalysisID,"' ORDER BY 1,2,3",sep = "");
	KMeansClusterID <- sqlQuery(DBConnection, SQLStr);

	RetData = list(AnalysisID = AnalysisID, Dendrogram = KMeansDendrogram, ClusterID = KMeansClusterID);
	
	return(RetData);
}