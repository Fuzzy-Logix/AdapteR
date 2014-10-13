FLLinRegr <- function( 	Tbl,
						DepCol,
						Note     = "From RWrapper For DBLytix",
						PrimaryKey   = FLPrimaryKey(x),
						Exclude      = c(),
						ClassSpec    = list(),
						WhereClause  = ""){

	ObsIDColName  <- "ObsID";
	VarIDColName  <- "VarID";
	ValueColName  <- "Num_Val";
	WhereClause   <- "";

	DataPrepRes <- FLRegrDataPrep( 	Tbl,
									DepCol,
									ObsIDColName =ObsIDColName,
									VarIDColName =VarIDColName,
									ValueColName =ValueColName,
									PrimaryKey   = PrimaryKey,
									Exclude      = Exclude,
									ClassSpec    = ClassSpec,
									WhereClause  = WhereClause);
	DeepTableName <- DataPrepRes$DeepTableName
	DBConnection  <- Tbl@ODBCConnection;
	SQLStr        <- "CALL FLLinRegr('";
	SQLParameters <- paste(	DeepTableName,
							ObsIDColName,  
							VarIDColName, 
							ValueColName, 							
							Note, sep="','")
	SQLStr           <- paste(SQLStr, SQLParameters,"', AnalysisID)", sep="")
	
	#print(SQLStr)
	#run LinRegr
	LinRegRes        <- sqlQuery(DBConnection, SQLStr);
	#print(LinRegRes)
	AnalysisID       <- toString(LinRegRes[1,"AnalysisID"]);

	RetData = new("FLLinRegr",AnalysisID = AnalysisID, ODBCConnection = DBConnection, DeepTableName = DeepTableName);
	
	return(RetData);
}