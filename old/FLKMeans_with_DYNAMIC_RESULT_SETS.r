is.naturalNumber <- function(x)
{
	if (!is.numeric(x)) 
        return(FALSE);
		
    if (ceiling(x) != floor(x)) 
        return(FALSE);
		
	if (x<=0) 
        return(FALSE);
		
	return(TRUE);
}

FLKMeans <- function(TableName, ObsIDColName, VarIDColName, ValueColName, WhereClause, Clusters, Iterations, Hypothesis, Note)
{
	#check TableName
    if (!is.character(TableName)) 
        return("First parameter is the table name and it should be a string");
	
	#check ObsIDColName
    if (!is.character(ObsIDColName)) 
        return("Second parameter is the name of the observation ID column and it should be a string");
		
	#check VarIDColName
    if (!is.character(VarIDColName)) 
        return("Third parameter is the name of the Variable ID column and it should be a string");
		
	#check ValueColName
    if (!is.character(ValueColName)) 
        return("Fourth parameter is the name of the Value Column and it should be a string");
		
	#check WhereClause
    if (!is.character(WhereClause)) 
        return("Fifth parameter is an Additional filter for restricting input data and it should be a string");
		
    #check notes
    if (!is.character(Note)) 
        return("Ninth parameter is the user defined comment and it should be a string");
	
	#check Clusters 
    if (!is.naturalNumber(Clusters)) 
        return("Seventh parameter is Number of clusters and it should be an integer greater than zero");
		
	#check Iterations 
    if (!is.naturalNumber(Iterations)) 
        return("Seventh parameter is maximum iterations and it should be an integer greater than zero");
		
	#check Hypothesis 
    if (!is.naturalNumber(Hypothesis)) 
        return("Eighth parameter is Number of Hypotheses and it should be an integer greater than zero");
		
	#Generate SQL
	SQLStr <- paste("CALL RohitKMeans('", TableName, "',", "'", ObsIDColName, "',", "'", VarIDColName, "',", "'", ValueColName, "','", WhereClause, "','", toString(Clusters), "','", toString(Iterations),"','", toString(Hypothesis),"',", "'", Note,"')", sep="")

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