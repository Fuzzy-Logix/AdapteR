FLDecisionTree <- function( 	x,
                  		NumOfSplits,
						MaxLevel, 
						PurityThreshold,
						Note     = "From RWrapper For DBLytix",
						PrimaryKey   = FLPrimaryKey(x),
						Exclude      = c(),
						ClassSpec    = list()){

	ObsIDColName  <- "ObsID";
	VarIDColName  <- "VarID";
	ValueColName  <- "Val";
	
	DBConnection  <- x@ODBCConnection;
	DeepTableName <- x@TableName
	
	SQLStr        <- "CALL WorkaroundDecisionTree('";
	SQLParameters <- paste(	DeepTableName,
							ObsIDColName,  
							VarIDColName, 
							ValueColName, 
							toString(NumOfSplits),  
							toString(MaxLevel), 
							toString(PurityThreshold),
							Note, sep="','")
	SQLStr           <- paste(SQLStr, SQLParameters,"')", sep="")
	print(SQLStr)
	
	#run FLDecisionTree
	DecisionTreeRes  <- sqlQuery(DBConnection, SQLStr);
	AnalysisID       <- toString(DecisionTreeRes[[1,"AnalysisID"]]);

	RetData = new("FLDecisionTree",AnalysisID = AnalysisID, ODBCConnection = DBConnection);
	
	return(RetData);
}