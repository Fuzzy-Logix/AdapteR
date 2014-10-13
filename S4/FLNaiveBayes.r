FLNaiveBayes <- function( 	Tbl,
							DepCol,
							laplace     = 0,
							Note        = "From RWrapper For DBLytix",
							PrimaryKey  = FLPrimaryKey(x),
							Exclude     = c(),
							ClassSpec   = list(),
							WhereClause = ""){

	ObsIDColName  <- "ObsID";
	VarIDColName  <- "VarID";
	ValueColName  <- "Num_Val";
	WhereClause   <- "";

	DataPrepRes <- FLRegrDataPrep( 	Tbl,
									DepCol,
									ObsIDColName = ObsIDColName,
									VarIDColName = VarIDColName,
									ValueColName = ValueColName,
									PrimaryKey   = PrimaryKey,
									Exclude      = Exclude,
									ClassSpec    = ClassSpec,
									WhereClause  = WhereClause);
	DeepTableName <- DataPrepRes$DeepTableName
	DBConnection  <- Tbl@ODBCConnection;
	SQLStr        <- "CALL FLNaiveBayesModel('";
	SQLParameters <- paste(	DeepTableName,
							ObsIDColName,  
							VarIDColName, 
							ValueColName,
							laplace, 							
							Note, sep="','")
	SQLStr           <- paste(SQLStr, SQLParameters,"', AnalysisID)", sep="")
	
	#run NaiveBayes
	NaiveBayesRes        <- sqlQuery(DBConnection, SQLStr);
	
	AnalysisID       <- toString(NaiveBayesRes[1,"AnalysisID"]);

	RetData = new("FLNaiveBayes",AnalysisID = AnalysisID, ODBCConnection = DBConnection, DeepTableName = DeepTableName);
	
	return(RetData);
}