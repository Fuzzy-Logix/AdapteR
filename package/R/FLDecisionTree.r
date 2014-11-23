FLDecisionTree <- function( table,
							primary_key,
							response,
							min_obs_for_parent,
							max_level, 
							purity_threshold,
							exclude      = c(),
							class_spec    = list(),
							where_clause  = "",
							note     = "From RWrapper For DBLytix")
{
	obsID  <- "ObsID";
	varID  <- "VarID";
	value  <- "Num_Val";
	
	dataPrepRes 			<- regr_data_prep( 	table,
												response,
												obs_id = obsID,
												var_id = varID,
												value = value,
												primary_key   = primary_key,
												exclude      = exclude,
												class_spec    = class_spec,
												where_clause  = where_clause);
	
	deepTableName        	<- dataPrepRes$deepTableName;
	wideToDeepAnalysisID 	<- dataPrepRes$wideToDeepAnalysisID;
	connection         		<- table@odbc_connection;
	
	sql       				<- "CALL WorkaroundDecisionTree('";
	sqlParameters 			<- paste(	deepTableName,
										obsID,  
										varID, 
										value, 
										toString(min_obs_for_parent),  
										toString(max_level), 
										toString(purity_threshold),
										note, sep="','")
	sql           			<- paste(sql, sqlParameters,"')", sep="")
	#print(sql)
		
	#run FLDecisionTree
	decisionTreeRes  		<- sqlQuery(connection, sql);
	print(decisionTreeRes)
	analysisID 				<- toString(decisionTreeRes[[1,"ANALYSISID"]]);

	retData = new("FLDecisionTree",analysis_id = analysisID, wide_to_deep_analysis_id = wideToDeepAnalysisID, deep_table_name = deepTableName, class_spec = class_spec, primary_key = primary_key, exclude = as.character(exclude), odbc_connection = connection);
	
	return(retData);
}