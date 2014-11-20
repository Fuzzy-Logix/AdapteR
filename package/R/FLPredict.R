# Scoring Functions 
setGeneric("FLPredict", function(	analysis, 
									table, 
									... ) {
  standardGeneric("FLPredict")
});

#############################################################################################
# score function for FLLogRegr
setMethod("FLPredict", 
		  signature(analysis = "FLLogRegr", table = "FLTable"),
		  function(analysis, 
				   table)  {
									ObsIDColName <- "ObsID";
									VarIDColName <- "VarID";
									ValueColName <- "Num_Val";
									PrimaryKey   <- analysis@PrimaryKey;
									Exclude      <- analysis@Exclude;
									ClassSpec    <- analysis@ClassSpec;	
									InAnalysisID <- analysis@WidetoDeepAnalysisID;
									WhereClause  <- "";

							DataPrepRes <- FLRegrDataPrepScore( table,
																ObsIDColName = ObsIDColName,
																VarIDColName = VarIDColName,
																ValueColName = ValueColName,
																PrimaryKey   = PrimaryKey,
																Exclude      = Exclude,
																ClassSpec    = ClassSpec,
																WhereClause  = WhereClause,
																InAnalysisID = InAnalysisID);
							
							DeepTableName        <- DataPrepRes$DeepTableName;
							DBConnection         <- table@ODBCConnection;
							WidetoDeepAnalysisID <- DataPrepRes$WidetoDeepAnalysisID;
							RegrAnalysisID 		 <- analysis@AnalysisID;
							OutTable 			 <- "OutTable";
							SQLStr               <- "Call FLLogRegrScore('%s', '%s','%s','%s','%s','%s', 1,OutTable"
							SQLStr 				 <- sprintf(SQLStr, 
															DeepTableName,
															ObsIDColName,  
															VarIDColName, 
															ValueColName, 							
															WhereClause, 
															RegrAnalysisID)					
							SQLStr            <- paste(SQLStr,")", sep="");
							#print(SQLStr)
							#run LogRegrScore
							LogRegrScoreRes <- sqlQuery(DBConnection, SQLStr)
							OutTableName    <- toString(LogRegrScoreRes[[1]]);
							SQLStr          <- paste("SELECT * FROM", OutTableName, "ORDER BY 1", sep = " ");
							ScoreTbl        <- sqlQuery(DBConnection, SQLStr);
							return(ScoreTbl);
	}
);
############################################################################################

############################################################################################
#score function for FLNaiveBayes

setMethod("FLPredict", 
 	  signature(analysis = "FLNaiveBayes", table = "FLTable"),
		  function(analysis, table, PredictTable = "")
				   {8
							
							ObsIDColName <- "ObsID";
							VarIDColName <- "VarID";
							ValueColName <- "Num_Val";
							PrimaryKey   <- analysis@PrimaryKey;
							Exclude      <- analysis@Exclude;
							ClassSpec    <- analysis@ClassSpec;	
							InAnalysisID <- analysis@WidetoDeepAnalysisID;
							WhereClause  <- "";														
							DataPrepRes  <- FLRegrDataPrepScore( table,
																ObsIDColName = ObsIDColName,
																VarIDColName = VarIDColName,
																ValueColName = ValueColName,
																PrimaryKey   = PrimaryKey,
																Exclude      = Exclude,
																ClassSpec    = ClassSpec,
																WhereClause  = WhereClause,
																InAnalysisID = InAnalysisID);
							
							DeepTableName        <- DataPrepRes$DeepTableName;
							DBConnection         <- table@ODBCConnection;
							WidetoDeepAnalysisID <- DataPrepRes$WidetoDeepAnalysisID;
							RegrAnalysisID       <- analysis@AnalysisID;
							if(PredictTable == "")
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

#############################################################################################

#############################################################################################
# score function for FLLinRegr
setMethod("FLPredict", 
		  signature(analysis = "FLLinRegr", table = "FLTable"),
		  function(analysis, 
				   table)  {
									ObsIDColName <- "ObsID";
									VarIDColName <- "VarID";
									ValueColName <- "Num_Val";
									PrimaryKey   <- analysis@PrimaryKey;
									Exclude      <- analysis@Exclude;
									ClassSpec    <- analysis@ClassSpec;	
									InAnalysisID <- analysis@WidetoDeepAnalysisID;
									WhereClause  <- "";

							DataPrepRes <- FLRegrDataPrepScore( table,
																ObsIDColName = ObsIDColName,
																VarIDColName = VarIDColName,
																ValueColName = ValueColName,
																PrimaryKey   = PrimaryKey,
																Exclude      = Exclude,
																ClassSpec    = ClassSpec,
																WhereClause  = WhereClause,
																InAnalysisID = InAnalysisID);
							
							DeepTableName        <- DataPrepRes$DeepTableName;
							DBConnection         <- table@ODBCConnection;
							WidetoDeepAnalysisID <- DataPrepRes$WidetoDeepAnalysisID;
							RegrAnalysisID 		 <- analysis@AnalysisID;
							OutTable 			 <- "OutTable";
							SQLStr               <- "Call FLLinRegrScore('%s', '%s','%s','%s','%s','%s', 1,OutTable"
							SQLStr 				 <- sprintf(SQLStr, 
															DeepTableName,
															ObsIDColName,  
															VarIDColName, 
															ValueColName, 							
															WhereClause, 
															RegrAnalysisID)					
							SQLStr            <- paste(SQLStr,")", sep="");
							#print(SQLStr)
							#run LinRegrScore
							LinRegrScoreRes <- sqlQuery(DBConnection, SQLStr)
							OutTableName    <- toString(LinRegrScoreRes[[1]]);
							SQLStr          <- paste("SELECT * FROM", OutTableName, "ORDER BY 1", sep = " ");
							ScoreTbl        <- sqlQuery(DBConnection, SQLStr);
							return(ScoreTbl);
	}
);
############################################################################################