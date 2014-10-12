FLMDA <- function( 	x,
						Subclasses,
						Iterations,
						Initialization,
						Hypotheses,
						WhereClause = NULL,
                  		Note     = "From RWrapper For DBLytix",
						PrimaryKey   = FLPrimaryKey(x),
						Exclude      = c(),
						ClassSpec    = list()){

	ObsIDColName  <- "ObsID";
	VarIDColName  <- "VarID";
	ValueColName  <- "Num_Val";
	
	DBConnection  <- x@ODBCConnection;
	DeepTableName <- x@TableName
	
	SQLStr        <- "CALL WorkaroundMDA('";
	SQLParameters <- paste(	DeepTableName,
							ObsIDColName,  
							VarIDColName, 
							ValueColName, 
							WhereClause,
							toString(Subclasses),
							toString(Iterations),
							toString(Initialization),
							toString(Hypotheses),
							Note, sep="','")
	SQLStr           <- paste(SQLStr, SQLParameters,"')", sep="")
	print(SQLStr)
		
	#run FLMDA
	MDARes  <- sqlQuery(DBConnection, SQLStr);
	#print(MDARes)
	AnalysisID <- toString(MDARes[[1,"ANALYSISID"]]);

	RetData = new("FLMDA",AnalysisID = AnalysisID, ODBCConnection = DBConnection);
	
	return(RetData);
}