FLLogRegrStep <- function( 	Tbl,
							DepCol,
							PrimaryKey,
							Type,
							pThreshold,
							MaxIterations,
							HighestpAllow = 0.05,
							HighestpAllow1 = 0.3,
							HighestpAllow2 = 0.05,
							StepwiseDecrease = 0.05,
							TopN = 5,
							Note     = "From RWrapper For DBLytix",
							Include      = c(),
							Exclude      = c(),
							ClassSpec    = list(),
							WhereClause  = ""){
	
	# Types of Stepwise Regression
	# BW  - Backward
	# FB  - Fast Backward
	# UFB - Ultra Fast Backward
	# SW  - Stepwise  
	types <- c("BW","FB","SW","UFB")
	if(Type %in% types)
	{
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
		SpecID <- "";
		
		if(length(Include) > 0)
		{
			SpecID      <- GenSpecID(Tbl@TableName);
			VarMapQuery <- VarNameToID(WidetoDeepAnalysisID,Include);
			res         <- sqlQuery(DBConnection,VarMapQuery,stringsAsFactors = FALSE);		
			VarIds      <- unlist(res$VarID);
			queries     <- sapply(VarIds,function(col) paste("INSERT INTO fzzlLogRegrModelVarSpec VALUES ('",SpecID,"',",col,",'I')",sep="") );
			insertions  <- sapply(queries,function(query) sqlQuery(DBConnection,query) );
		}


		if(Type == "BW")
		{
			SQLStr        <- "CALL FLLogRegrBW('";
			SQLParameters <- paste(	DeepTableName,
									ObsIDColName,  
									VarIDColName, 
									ValueColName,
									MaxIterations,
									pThreshold,
									SpecID,
									HighestpAllow, 							
									Note, sep="','")
			SQLStr	<- paste(SQLStr, SQLParameters,"', AnalysisID)", sep="")
		}
		if(Type == "FB")
		{
			SQLStr        <- "CALL FLLogRegrFB('";
			SQLParameters <- paste(	DeepTableName,
									ObsIDColName,  
									VarIDColName, 
									ValueColName,
									MaxIterations,
									pThreshold,
									SpecID,
									HighestpAllow1, 							
									HighestpAllow2,
									Note, sep="','")
			SQLStr	<- paste(SQLStr, SQLParameters,"', AnalysisID)", sep="")
		}
		if(Type == "UFB")
		{
			SQLStr        <- "CALL FLLogRegrUFB('";
			SQLParameters <- paste(	DeepTableName,
									ObsIDColName,  
									VarIDColName, 
									ValueColName,
									pThreshold,
									MaxIterations,
									SpecID,
									HighestpAllow1,
									HighestpAllow2,
									StepwiseDecrease, 							
									Note, sep="','")
			SQLStr	<- paste(SQLStr, SQLParameters,"', AnalysisID)", sep="")
		}
		if(Type == "SW")
		{
			SQLStr        <- "CALL FLLogRegrSW('";
			SQLParameters <- paste(	DeepTableName,
									ObsIDColName,  
									VarIDColName, 
									ValueColName,
									MaxIterations,
									pThreshold,
									TopN,																	
									HighestpAllow, 							
									Note, sep="','")
			SQLStr	<- paste(SQLStr, SQLParameters,"', AnalysisID)", sep="")
		}
        
		print(SQLStr)
		LogRegRes        <- sqlQuery(DBConnection, SQLStr);		
		AnalysisID       <- toString(LogRegRes[1,"AnalysisID"]);
		RetData = new("FLLogRegr",AnalysisID = AnalysisID, ODBCConnection = DBConnection, DeepTableName = DeepTableName, WidetoDeepAnalysisID = WidetoDeepAnalysisID);
	}
	else
	{
		stop("Incorrect value for Type parameter. Type must be in {\"BW\",\"FB\",\"UFB\",\"SW\"} ")
	}
	
}
