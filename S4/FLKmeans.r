FLKMeans <- function( 	Tbl,
                  		centers,
						iter.max = 10, 
						nstart   = 1,
						Note     = "From RWrapper For DBLytix",
						PrimaryKey   = FLPrimaryKey(x),
						Exclude      = c(),
						ClassSpec    = list(),
						WhereClause  = ""){

	ObsIDColName  <- "ObsID";
	VarIDColName  <- "VarID";
	ValueColName  <- "Num_Val";
	WhereClause   <- "";

	DeepTableName <- FLWideToDeep(Tbl,
								ObsIDColName =ObsIDColName,
								VarIDColName =VarIDColName,
								ValueColName =ValueColName,
								PrimaryKey   = PrimaryKey,
								Exclude      = Exclude,
								ClassSpec    = ClassSpec,
								WhereClause  = WhereClause);
	DBConnection  <- Tbl@ODBCConnection;
	SQLStr        <- "CALL WorkaroundKMeans('";
	SQLParameters <- paste(	DeepTableName,
							ObsIDColName,  
							VarIDColName, 
							ValueColName, 
							WhereClause, 
							toString(centers),  
							toString(iter.max), 
							toString(nstart),
							Note, sep="','")
	SQLStr           <- paste(SQLStr, SQLParameters,"')", sep="")
	
	#run KMeans
	KMeansRes        <- sqlQuery(DBConnection, SQLStr);
	AnalysisID       <- toString(KMeansRes$ANALYSISID);

	RetData = new("FLKMeans",AnalysisID = AnalysisID, ODBCConnection = DBConnection, DeepTableName = DeepTableName);
	
	return(RetData);
}