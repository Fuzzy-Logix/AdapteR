FLMDA <- function( 	table,
					primary_key,
					response,
					subclasses,
					max_iter,
					initialization,
					hypotheses,
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
												
	deepTableName       	<- dataPrepRes$deepTableName;
	wideToDeepAnalysisID 	<- dataPrepRes$wideToDeepAnalysisID;
	connection         	 	<- table@odbc_connection;
	
	sql        				<- "CALL WorkaroundMDA('";
	sqlParameters 			<- paste(	deepTableName,
										obsID,  
										varID, 
										value, 
										where_clause,
										toString(subclasses),
										toString(max_iter),
										toString(initialization),
										toString(hypotheses),
										note, sep="','")
	sql           			<- paste(sql, sqlParameters,"')", sep="")
	print(sql)
		
	#run FLMDA
	mdaRes 			 		<- sqlQuery(connection, sql);
	#print(mdaRes)
	analysisID 				<- toString(mdaRes[[1,"ANALYSISID"]]);
	retData = new("FLMDA",analysis_id = analysisID, odbc_connection = connection);
	
	return(retData);
}