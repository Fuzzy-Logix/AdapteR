#############################################################################################
# score function for FLLogRegr
setGeneric("fl.predict", function(FLLogRegrObject, FLTableObject) {
  standardGeneric("fl.predict")
})

setMethod("fl.predict", 
		  signature(FLLogRegrObject = "FLLogRegr", FLTableObject = "FLTable"),
		  function(FLLogRegrObject, 
				   FLTableObject)
				   {
							#Exclude = c();
							#DepCol = FLLogRegrObject@DepCol;
							ObsIDColName  <- "ObsID";
							VarIDColName  <- "VarID";
							ValueColName  <- "Num_Val";
							PrimaryKey <- FLLogRegrObject@PrimaryKey;
							Exclude <- FLLogRegrObject@Exclude;
							ClassSpec <- FLLogRegrObject@ClassSpec;	
							InAnalysisID = FLLogRegrObject@WidetoDeepAnalysisID;
							WhereClause = "";

							##SQLStr <- paste("SELECT COLUMN_NAME FROM fzzlRegrDataPrepMap  WHERE ANALYSISID = '", FLLogRegrObject@WidetoDeepAnalysisID, "' AND  VARID = -1;", sep = "");
							##DepCol <- sqlQuery(DBConnection, SQLStr);
							##SQLStr <- paste("SELECT COLUMN_NAME FROM fzzlRegrDataPrepMap  WHERE ANALYSISID = '", FLLogRegrObject@WidetoDeepAnalysisID, "' AND  Exclude_var = 1;", sep = "");
														
							DataPrepRes <- FLRegrDataPrepScore( FLTableObject,
															ObsIDColName = ObsIDColName,
															VarIDColName = VarIDColName,
															ValueColName = ValueColName,
															PrimaryKey   = PrimaryKey,
															Exclude      = Exclude,
															ClassSpec    = ClassSpec,
															WhereClause  = WhereClause,
															InAnalysisID = InAnalysisID);
							
							DeepTableName        <- DataPrepRes$DeepTableName;
							DBConnection         <- FLTableObject@ODBCConnection;
							WidetoDeepAnalysisID <- DataPrepRes$WidetoDeepAnalysisID;
							RegrAnalysisID <- FLLogRegrObject@AnalysisID;
							OutTable <- "OutTable";
							#SQLStr               <- "CALL FLLogRegrScore('";
							SQLStr <- "Call FLLogRegrScore('%s', '%s','%s','%s','%s','%s', 1,OutTable"
							SQLStr <- sprintf(SQLStr, 
											  DeepTableName,
											  ObsIDColName,  
											  VarIDColName, 
											  ValueColName, 							
											  WhereClause, 
											  RegrAnalysisID)					
							#SQLParameters <- paste(	DeepTableName,
							#						ObsIDColName,  
							#						VarIDColName, 
							#						ValueColName, 							
							#						WhereClause, 
							#						RegrAnalysisID,
							#						toString(0),
							#						OutTable,
							#						sep="','")
	#SQLStr           <- paste(SQLStr,"',OutTable)",sep="");
	SQLStr           <- paste(SQLStr,")", sep="");
	print(SQLStr)
	#run LogRegrScore
	LogRegrScoreRes      <- sqlQuery(DBConnection, SQLStr)
	OutTableName <- toString(LogRegrScoreRes[[1]]);
	SQLStr <- paste("SELECT * FROM", OutTableName, "ORDER BY 1", sep = " ");
	ScoreTbl <- sqlQuery(DBConnection, SQLStr);
	#SQLStr <- "SELECT ObsID, Num_Val FROM %s WHERE %s = -1 ORDER BY 1"
	#SQLStr <- sprintf(SQLStr, ObsIDColName, ValueColName, DeepTableName, VarIDColName)
	#ObsTbl <- sqlQuery(DBConnection, SQLStr)
	return(ScoreTbl);
	}
)