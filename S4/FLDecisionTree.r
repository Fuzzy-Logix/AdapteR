FLDecisionTree <- function( 	x,
                  		MinObsforParent,
						MaxLevel, 
						PurityThreshold,
						Note     = "From RWrapper For DBLytix",
						PrimaryKey   = FLPrimaryKey(x),
						Exclude      = c(),
						ClassSpec    = list()){

	ObsIDColName  <- "ObsID";
	VarIDColName  <- "VarID";
	ValueColName  <- "Num_Val";
	
	DBConnection  <- x@ODBCConnection;
	DeepTableName <- x@TableName
	
	SQLStr        <- "CALL WorkaroundDecisionTree('";
	SQLParameters <- paste(	DeepTableName,
							ObsIDColName,  
							VarIDColName, 
							ValueColName, 
							toString(MinObsforParent),  
							toString(MaxLevel), 
							toString(PurityThreshold),
							Note, sep="','")
	SQLStr           <- paste(SQLStr, SQLParameters,"')", sep="")
	print(SQLStr)
		
	#run FLDecisionTree
	DecisionTreeRes  <- sqlQuery(DBConnection, SQLStr);
	print(DecisionTreeRes)
	AnalysisID <- toString(DecisionTreeRes[[1,"ANALYSISID"]]);

	RetData = new("FLDecisionTree",AnalysisID = AnalysisID, ODBCConnection = DBConnection);
	
	return(RetData);
}