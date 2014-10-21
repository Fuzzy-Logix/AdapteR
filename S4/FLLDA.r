FLLDA <- function( 	x,
					DepCol,
					PrimaryKey,
                  	Note     = "From RWrapper For DBLytix",
					Exclude      = c(),
					ClassSpec    = list(),
					WhereClause  = "")
{
	ObsIDColName  <- "ObsID";
	VarIDColName  <- "VarID";
	ValueColName  <- "Num_Val";
	
	DataPrepRes <- FLRegrDataPrep( 	x,
									DepCol,
									ObsIDColName = ObsIDColName,
									VarIDColName = VarIDColName,
									ValueColName = ValueColName,
									PrimaryKey   = PrimaryKey,
									Exclude      = Exclude,
									ClassSpec    = ClassSpec,
									WhereClause  = WhereClause);
	
	DeepTableName        <- DataPrepRes$DeepTableName;
	WidetoDeepAnalysisID <- DataPrepRes$WidetoDeepAnalysisID;
	DBConnection         <- x@ODBCConnection;
	
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