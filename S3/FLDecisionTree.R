FLDecisionTree <- function( x,
                      NumOfSplits,
                      MaxLevel, 
                      PurityThreshold,
                      Note = ""){
	
	ObsIDColName <- "ObsID";
	VarIDColName <- "VarID";
	ValueColName <- "Val";
	DBConnection <- x[["connection"]];
	SQLStr <- paste(	"CALL WorkaroundDecisionTree('", 
						x[["DeepTableName"]], "', '",
						ObsIDColName, "', '", 
						VarIDColName, "', '", 
						ValueColName, "', '", 
						toString(NumOfSplits), "', '", 
						toString(MaxLevel),"', '", 
						toString(PurityThreshold),"', ' ", 
						Note,"')", sep="")
	print(SQLStr)
	#run DecisionTree
    DecisionTreeRes <- sqlQuery(DBConnection, SQLStr,stringsAsFactors = FALSE);
	AnalysisID <- DecisionTreeRes[[1,"AnalysisID"]];
	
	SQLStr <- paste("SELECT NodeID,SplitVarID,SplitVal,ParentNodeID,ChildType,Lvl,IsLeaf,ClassValue,NodeSize,EventRate FROM fzzlDecisionTree WHERE AnalysisID = '", AnalysisID,"' ORDER BY 1,2,3,4",sep = "");
	FinalResult <- sqlQuery(DBConnection, SQLStr);
	
	RetData = list(AnalysisID = AnalysisID, FinalResult = FinalResult);
	
	return(RetData);
}