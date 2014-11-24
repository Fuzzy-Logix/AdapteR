FLLDA <- function( 	table,
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
	
	deepTableName        	<- dataPrepRes$deepTableName;
	wideToDeepAnalysisID 	<- dataPrepRes$wideToDeepAnalysisID;
	connection         		<- table@odbc_connection;
	
	sql        				<- "CALL WorkaroundLDA('";
	sqlParameters 			<- paste(	deepTableName,
										obsID,  
										varID, 
										value, 
										note, sep="','")
	sql          			<- paste(sql, sqlParameters,"')", sep="")
	print(sql)
		
	#run FLLDA
	ldaRes  				<- sqlQuery(connection, sql);
	analysisID 				<- toString(ldaRes[[1,"ANALYSISID"]]);
	retData = new("FLLDA",analysis_id = analysisID, odbc_connection = connection);
	
	return(retData);
}