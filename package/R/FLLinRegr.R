FLLinRegr <- function( 	table,
						primary_key,
						response,												
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
	deepTableName        	<- dataPrepRes$deepTableName
	wideToDeepAnalysisID 	<- dataPrepRes$wideToDeepAnalysisID
	connection         		<- table@odbc_connection;

	sqlParameters 			<- list(	deepTableName = deepTableName,
										obs_id  = obsID,  
										var_id  = varID, 
										value  = value, 							
										note          = note )
	
	#run LinRegr
	linRegrRes        		<- runsql(connection, "SQL//FLLinRegr.sql", sqlParameters);
	analysisID       		<- toString(linRegrRes[1,"AnalysisID"]);

	retData = new("FLLinRegr",analysis_id = analysisID, odbc_connection = connection, deep_table_name = deepTableName, wide_to_deep_analysis_id = wideToDeepAnalysisID);
	
	return(retData);
}