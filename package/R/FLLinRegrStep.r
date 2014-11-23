FLLinRegrStep <- function( 	table,
							primary_key,
							response,
							type,
							highest_p_allow = 0.05,
							highest_p_allow1 = 0.3,
							highest_p_allow2 = 0.05,
							stepwise_decrease = 0.05,
							top_n = 5,
							include      = c(),
							exclude      = c(),
							class_spec    = list(),
							where_clause  = "",
							note     = "From RWrapper For DBLytix")
{
	
	# Types of Stepwise Regression
	# BW  - Backward
	# FB  - Fast Backward
	# UFB - Ultra Fast Backward
	# SW  - Stepwise  
	types <- c("BW","FB","SW","UFB")
	if(type %in% types)
	{
		obsID  <- "ObsID";
		varID  <- "VarID";
		value  <- "Num_Val";
		
		dataPrepRes <- regr_data_prep( 	table,
										response,
										obs_id = obsID,
										var_id = varID,
										value = value,
										primary_key   = primary_key,
										exclude      = exclude,
										class_spec    = class_spec,
										where_clause  = where_clause);

		deepTableName        	<- dataPrepRes$deepTableName
		wideToDeepAnalysisID 	<- dataPrepRes$wideToDeepAnalysisID
		connection         		<- table@odbc_connection;
		specID <- "";
		
		if(length(include) > 0)
		{
			specID      <- gen_spec_ID(table@table_name);
			varMapQuery <- var_name_to_ID(wideToDeepAnalysisID,include);
			res         <- sqlQuery(connection,varMapQuery,stringsAsFactors = FALSE);		
			varIDs      <- unlist(res$VarID);
			queries     <- sapply(varIDs,function(col) paste("INSERT INTO fzzlLinRegrModelVarSpec VALUES ('",specID,"',",col,",'I')",sep="") );
			insertions  <- sapply(queries,function(query) sqlQuery(connection,query) );
		}


		if(type == "BW")
		{
			sql        		<- "CALL FLLinRegrBW('";
			sqlParameters 	<- paste(	deepTableName,
										obsID,  
										varID, 
										value,
										specID,
										highest_p_allow, 							
										note, sep="','")
			sql				<- paste(sql, sqlParameters,"', AnalysisID)", sep="")
		}
		if(type == "FB")
		{
			sql        		<- "CALL FLLinRegrFB('";
			sqlParameters 	<- paste(	deepTableName,
									obsID,  
									varID, 
									value,
									specID,
									highest_p_allow1, 							
									highest_p_allow2,
									note, sep="','")
			sql				<- paste(sql, sqlParameters,"', AnalysisID)", sep="")
		}
		if(type == "UFB")
		{
			sql        		<- "CALL FLLinRegrUFB('";
			sqlParameters 	<- paste(	deepTableName,
									obsID,  
									varID, 
									value,
									specID,
									highest_p_allow1,
									highest_p_allow2,
									stepwise_decrease, 							
									note, sep="','")
			sql				<- paste(sql, sqlParameters,"', AnalysisID)", sep="")
		}
		if(type == "SW")
		{
			sql        		<- "CALL FLLinRegrSW('";
			sqlParameters 	<- paste(	deepTableName,
									obsID,  
									varID, 
									value,
									top_n,																	
									highest_p_allow, 							
									note, sep="','")
			sql				<- paste(sql, sqlParameters,"', AnalysisID)", sep="")
		}

		linRegrRes        	<- sqlQuery(connection, sql);		
		analysisID       	<- toString(linRegrRes[1,"AnalysisID"]);
		retData = new("FLLinRegr",analysis_id = analysisID, odbc_connection = connection, deep_table_name = deepTableName, wide_to_deep_analysis_id = wideToDeepAnalysisID);
	}
	else
	{
		stop("Incorrect value for type parameter. Type must be in {\"BW\",\"FB\",\"UFB\",\"SW\"} ")
	}
	
}
