setOldClass("RODBC");

# define fetch.results Generic
setGeneric("fetch.results", function(object) {
  standardGeneric("fetch.results")
})

# define FLKMeans Class
setClass("FLKMeans", 
		slots = list(	ODBCConnection = "RODBC",
						AnalysisID     = "character", 
						centers        = "data.frame", 
						cluster        = "data.frame"))


# fetch_results method for KMeans
setMethod("fetch.results",
          signature("FLKMeans"),
          function(object) {
      DBConnection <- object@ODBCConnection;            
      #Fetch Centers Dendrogram
			SQLStr           <- paste("SELECT HypothesisID,Level,ClusterID,VarID,Centroid FROM fzzlKMeansDendrogram WHERE AnalysisID = '", object@AnalysisID,"' ORDER BY 1,2,3,4",sep = "");
			KMeansDendrogram <- sqlQuery(DBConnection, SQLStr);
				
			#Fetch ClusterID Arrays
			SQLStr           <- paste("SELECT HypothesisID,ObsID,ClusterID FROM fzzlKMeansClusterID WHERE AnalysisID = '", object@AnalysisID,"' ORDER BY 1,2,3",sep = "");
			KMeansClusterID  <- sqlQuery(DBConnection, SQLStr);

			object@centers = KMeansDendrogram;
			object@cluster = KMeansClusterID;
			#print(paste(object@AnalysisID));
			object
          }
)
# define FLDecisionTree Class
setClass("FLDecisionTree", 
		slots = list(	ODBCConnection = "RODBC",
						AnalysisID     = "character", 
						decision_tree  = "data.frame"))
						
# fetch_results method for FLDecisionTree
setMethod("fetch.results",
          signature("FLDecisionTree"),
          function(object) {
      DBConnection <- object@ODBCConnection;            
      #Fetch Decision Tree Analysis Result Table
			SQLStr <- paste("SELECT NodeID,SplitVarID,SplitVal,ParentNodeID,ChildType,Lvl,IsLeaf,ClassValue,NodeSize,EventRate FROM fzzlDecisionTree WHERE AnalysisID = '", object@AnalysisID,"' ORDER BY 1,2,3,4",sep = "");
			FinalResult <- sqlQuery(DBConnection, SQLStr);
			object@decision_tree = FinalResult;
			
			#print(paste(object@AnalysisID));
			object
          }
)