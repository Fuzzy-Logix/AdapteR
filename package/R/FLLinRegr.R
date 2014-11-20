FLLinRegr <- function( 	table,
						primary_key,
						response,
						Note     = "From RWrapper For DBLytix",						
						Exclude      = c(),
						ClassSpec    = list(),
						WhereClause  = ""){

	ObsIDColName  <- "ObsID";
	VarIDColName  <- "VarID";
	ValueColName  <- "Num_Val";
	
	DataPrepRes <- FLRegrDataPrep( 	table,
									response,
									ObsIDColName = ObsIDColName,
									VarIDColName = VarIDColName,
									ValueColName = ValueColName,
									PrimaryKey   = PrimaryKey,
									Exclude      = Exclude,
									ClassSpec    = ClassSpec,
									WhereClause  = WhereClause);
	DeepTableName        <- DataPrepRes$DeepTableName
	WidetoDeepAnalysisID <- DataPrepRes$WidetoDeepAnalysisID
	DBConnection         <- table@ODBCConnection;

	SQLParameters <- list(	DeepTableName = DeepTableName,
							ObsIDColName  = ObsIDColName,  
							VarIDColName  = VarIDColName, 
							ValueColName  = ValueColName, 							
							Note          = Note )
	
	#run LinRegr
	LinRegRes        <- runsql(DBConnection, "SQL//FLLinRegr.sql", SQLParameters);
	AnalysisID       <- toString(LinRegRes[1,"AnalysisID"]);

	RetData = new("FLLinRegr",AnalysisID = AnalysisID, ODBCConnection = DBConnection, DeepTableName = DeepTableName, WidetoDeepAnalysisID = WidetoDeepAnalysisID);
	
	return(RetData);
}