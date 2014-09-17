setOldClass("RODBC");
setClass("FLKMeans", 
		slots = list(	ODBCConnection = "RODBC",
						AnalysisID     = "character", 
						centers        = "data.frame", 
						cluster        = "data.frame"))

setGeneric("fetch.results", function(object) {
  standardGeneric("fetch.results")
})

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