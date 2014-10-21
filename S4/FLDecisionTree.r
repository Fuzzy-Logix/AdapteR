FLDecisionTree <- function( x,
							DepCol,	
							PrimaryKey,
							MinObsforParent,
							MaxLevel, 
							PurityThreshold,
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
	
	SQLStr        <- "CALL WorkaroundDecisionTree('";
	SQLParameters <- paste(	DeepTableName,
							ObsIDColName,  
							VarIDColName, 
							ValueColName, 
							toString(MinObsforParent),  
							toString(MaxLevel), 
							toString(PurityThreshold),
							Note, sep="','")
	SQLStr           <- paste(SQLStr, SQLParameters,"')", sep="")
	#print(SQLStr)
		
	#run FLDecisionTree
	DecisionTreeRes  <- sqlQuery(DBConnection, SQLStr);
	print(DecisionTreeRes)
	AnalysisID <- toString(DecisionTreeRes[[1,"ANALYSISID"]]);

	RetData = new("FLDecisionTree",AnalysisID = AnalysisID, ODBCConnection = DBConnection);
	
	return(RetData);
}