setOldClass("RODBC");
setClass("FLDecisionTree", 
		slots = list(	ODBCConnection = "RODBC",
						AnalysisID     = "character", 
						decision_tree  = "data.frame"))

setGeneric("fetch.results", function(object) {
  standardGeneric("fetch.results")
})

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