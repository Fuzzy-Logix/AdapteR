FLLogRegr <- function( 	x,
                  		MaxIterations,
						pThreshold,
						Note     = "From RWrapper For DBLytix",
						PrimaryKey   = FLPrimaryKey(x),
						Exclude      = c(),
						ClassSpec    = list()){

	ObsIDColName  <- "ObsID";
	VarIDColName  <- "VarID";
	ValueColName  <- "Num_Val";
	
	DBConnection  <- x@ODBCConnection;
	DeepTableName <- x@TableName
	
	SQLStr        <- "CALL WorkaroundLogRegr('";
	SQLParameters <- paste(	DeepTableName,
							ObsIDColName,  
							VarIDColName, 
							ValueColName, 
							toString(MaxIterations),  
							toString(pThreshold),
							Note, sep="','")
	SQLStr           <- paste(SQLStr, SQLParameters,"')", sep="")
	print(SQLStr)
		
	#run FLLogRegr
	LogRegrRes  <- sqlQuery(DBConnection, SQLStr);
	#print(LogRegrRes)
	AnalysisID <- toString(LogRegrRes[[1,"ANALYSISID"]]);

	RetData = new("FLLogRegr",AnalysisID = AnalysisID, ODBCConnection = DBConnection);
	
	return(RetData);
}