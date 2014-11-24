FLVIF <- function( 	table,
					primary_key,
					response,
					exclude = c(),
					class_spec = list(),
					where_clause = "",
					note = "From RWrapper For DBLytix")
{

	obsID  <- "ObsID";
	varID  <- "VarID";
	value  <- "Num_Val";
	
	dataPrepRes 			<- regr_data_prep( 	table,
												response,
												obs_id = obsID,
												var_id = varID,
												value = value,
												primary_key = primary_key,
												exclude = exclude,
												class_spec = class_spec,
												where_clause = where_clause);
	deepTableName        	<- dataPrepRes$deepTableName
	wideToDeepAnalysisID 	<- dataPrepRes$wideToDeepAnalysisID
	connection         		<- table@odbc_connection;
	sql               		<- "CALL FLVIF('";
	sqlParameters 			<- paste(	deepTableName,
										obsID,  
										varID, 
										value, 							
										note, sep = "','")
	sql           			<- paste(sql, sqlParameters,"', AnalysisID)", sep = "")
	
	#print(sql)
	#run VIF
	vifRes        			<- sqlQuery(connection, sql);
	#print(vifRes)
	analysisID       		<- toString(vifRes[1,"AnalysisID"]);

	retData = new("FLVIF",analysis_id = analysisID, odbc_connection = connection, deep_table_name = deepTableName, wide_to_deep_analysis_id = wideToDeepAnalysisID);
	
	return(retData);
}