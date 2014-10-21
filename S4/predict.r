# Scoring Functions 
setGeneric("FL.predict", function(AnalysisObject, FLTableObject) {
  standardGeneric("FL.predict")
});

#############################################################################################
# score function for FLLogRegr
setMethod("FL.predict", 
		  signature(AnalysisObject = "FLLogRegr", FLTableObject = "FLTable"),
		  function(AnalysisObject, 
				   FLTableObject)
				   {
							#Exclude = c();
							#DepCol = AnalysisObject@DepCol;
							ObsIDColName <- "ObsID";
							VarIDColName <- "VarID";
							ValueColName <- "Num_Val";
							PrimaryKey   <- AnalysisObject@PrimaryKey;
							Exclude      <- AnalysisObject@Exclude;
							ClassSpec    <- AnalysisObject@ClassSpec;	
							InAnalysisID <- AnalysisObject@WidetoDeepAnalysisID;
							WhereClause  <- "";

							##SQLStr <- paste("SELECT COLUMN_NAME FROM fzzlRegrDataPrepMap  WHERE ANALYSISID = '", AnalysisObject@WidetoDeepAnalysisID, "' AND  VARID = -1;", sep = "");
							##DepCol <- sqlQuery(DBConnection, SQLStr);
							##SQLStr <- paste("SELECT COLUMN_NAME FROM fzzlRegrDataPrepMap  WHERE ANALYSISID = '", AnalysisObject@WidetoDeepAnalysisID, "' AND  Exclude_var = 1;", sep = "");
														
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
							RegrAnalysisID       <- AnalysisObject@AnalysisID;
							OutTable             <- "OutTable";
							#SQLStr              <- "CALL FLLogRegrScore('";
							SQLStr               <- "Call FLLogRegrScore('%s', '%s','%s','%s','%s','%s', 1,OutTable"
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
							#SQLStr         <- paste(SQLStr,"',OutTable)",sep="");
							SQLStr          <- paste(SQLStr,")", sep="");
							#print(SQLStr)
							#run LogRegrScore
							LogRegrScoreRes <- sqlQuery(DBConnection, SQLStr)
							OutTableName    <- toString(LogRegrScoreRes[[1]]);
							SQLStr          <- paste("SELECT * FROM", OutTableName, "ORDER BY 1", sep = " ");
							ScoreTbl        <- sqlQuery(DBConnection, SQLStr);
							#SQLStr <- "SELECT ObsID, Num_Val FROM %s WHERE %s = -1 ORDER BY 1"
							#SQLStr <- sprintf(SQLStr, ObsIDColName, ValueColName, DeepTableName, VarIDColName)
							#ObsTbl <- sqlQuery(DBConnection, SQLStr)
							return(ScoreTbl);
	}
);
#
##############################################################################################

#############################################################################################
# score function for FLNaiveBayes

setMethod("FL.predict", 
		  signature(AnalysisObject = "FLNaiveBayes", FLTableObject = "FLTable"),
		  function(FLNaiveBayesObject, FLTableObject, PredictTable = "")
				   {
							
							ObsIDColName <- "ObsID";
							VarIDColName <- "VarID";
							ValueColName <- "Num_Val";
							PrimaryKey   <- AnalysisObject@PrimaryKey;
							Exclude      <- AnalysisObject@Exclude;
							ClassSpec    <- AnalysisObject@ClassSpec;	
							InAnalysisID <- AnalysisObject@WidetoDeepAnalysisID;
							WhereClause  <- "";														
							DataPrepRes  <- FLRegrDataPrepScore( FLTableObject,
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
							RegrAnalysisID       <- AnalysisObject@AnalysisID;
							if(PredictTable = "")
							{
								PredictTable <- GenOutTable("NaiveBayes",RegrAnalysisID);	
							}												
							SQLStr <- "Call FLNaiveBayesPredict('%s','%s','%s','%s','%s','%s','From RWrapper for DBLytix', OutAnalysisID)";
							SQLStr <- sprintf(SQLStr, 
											  DeepTableName,
											  ObsIDColName,  
											  VarIDColName, 
											  ValueColName, 																		  
											  RegrAnalysisID,
											  PredictTable);											
							#run NaiveBayesScore
							NaiveBayesScoreRes <- sqlQuery(DBConnection, SQLStr);							
							SQLStr             <- paste("SELECT * FROM", PredictTable, "ORDER BY 1", sep = " ");
							ScoreTbl           <- sqlQuery(DBConnection, SQLStr);							
							return(ScoreTbl);
	}
);
#
##############################################################################################