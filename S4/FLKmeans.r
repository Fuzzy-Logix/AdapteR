FLKMeans <- function( 	x,
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

	DeepTableName <- FLDataPrep(x,
								ObsIDColName =ObsIDColName,
								VarIDColName =VarIDColName,
								ValueColName =ValueColName,
								PrimaryKey   = PrimaryKey,
								Exclude      = Exclude,
								ClassSpec    = ClassSpec,
								WhereClause  = WhereClause);
	DBConnection  <- x@ODBCConnection;
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
	KMeansRes
	str(KMeansRes)
	AnalysisID       <- toString(KMeansRes$ANALYSISID);

	RetData = new("FLKMeans",AnalysisID = AnalysisID, ODBCConnection = DBConnection);
	
	return(RetData);
}