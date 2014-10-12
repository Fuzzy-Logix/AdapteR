FLLDA <- function( 	x,
                  		Note     = "From RWrapper For DBLytix",
						PrimaryKey   = FLPrimaryKey(x),
						Exclude      = c(),
						ClassSpec    = list()){

	ObsIDColName  <- "ObsID";
	VarIDColName  <- "VarID";
	ValueColName  <- "Num_Val";
	
	DBConnection  <- x@ODBCConnection;
	DeepTableName <- x@TableName
	
	SQLStr        <- "CALL WorkaroundLDA('";
	SQLParameters <- paste(	DeepTableName,
							ObsIDColName,  
							VarIDColName, 
							ValueColName, 
							Note, sep="','")
	SQLStr           <- paste(SQLStr, SQLParameters,"')", sep="")
	print(SQLStr)
		
	#run FLLDA
	LDARes  <- sqlQuery(DBConnection, SQLStr);
	#print(LogRegrRes)
	AnalysisID <- toString(LDARes[[1,"ANALYSISID"]]);

	RetData = new("FLLDA",AnalysisID = AnalysisID, ODBCConnection = DBConnection);
	
	return(RetData);
}