FLVIF <- function( 	Tbl,
					DepCol,
					PrimaryKey,
					Note     = "From RWrapper For DBLytix",					
					Exclude      = c(),
					ClassSpec    = list(),
					WhereClause  = ""){

	ObsIDColName  <- "ObsID";
	VarIDColName  <- "VarID";
	ValueColName  <- "Num_Val";
	
	DataPrepRes <- FLRegrDataPrep( 	Tbl,
									DepCol,
									ObsIDColName = ObsIDColName,
									VarIDColName = VarIDColName,
									ValueColName = ValueColName,
									PrimaryKey   = PrimaryKey,
									Exclude      = Exclude,
									ClassSpec    = ClassSpec,
									WhereClause  = WhereClause);
	DeepTableName        <- DataPrepRes$DeepTableName
	WidetoDeepAnalysisID <- DataPrepRes$WidetoDeepAnalysisID
	DBConnection         <- Tbl@ODBCConnection;
	SQLStr               <- "CALL FLVIF('";
	SQLParameters <- paste(	DeepTableName,
							ObsIDColName,  
							VarIDColName, 
							ValueColName, 							
							Note, sep="','")
	SQLStr           <- paste(SQLStr, SQLParameters,"', AnalysisID)", sep="")
	
	#print(SQLStr)
	#run VIF
	VIFRes        <- sqlQuery(DBConnection, SQLStr);
	#print(VIFRes)
	AnalysisID       <- toString(VIFRes[1,"AnalysisID"]);

	RetData = new("FLVIF",AnalysisID = AnalysisID, ODBCConnection = DBConnection, DeepTableName = DeepTableName, WidetoDeepAnalysisID = WidetoDeepAnalysisID);
	
	return(RetData);
}