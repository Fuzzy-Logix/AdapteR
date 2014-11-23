FLKMeans <- function( 	table,
						primary_key,
                  		centers,
						max_iter = 10, 
						nstart   = 1,
						exclude      = c(),
						class_spec    = list(),
						where_clause  = "",
						note     = "From RWrapper For DBLytix")
{

	obsID  <- "ObsID";
	varID  <- "VarID";
	value  <- "Num_Val";
	
	deepTableName 	<- wide_to_deep(	table,
										obs_id = obsID,
										var_id = varID,
										value =  value,
										primary_key  = primary_key,
										exclude      = exclude,
										class_spec   = class_spec,
										where_clause = where_clause);
	connection  	<- table@odbc_connection;
	sql        		<- "CALL WorkaroundKMeans('";
	sqlParameters 	<- paste(	deepTableName,
								obsID,  
								varID, 
								value, 
								where_clause, 
								toString(centers),  
								toString(max_iter), 
								toString(nstart),
								note, sep="','")
	sql           	<- paste(sql, sqlParameters,"')", sep="")
	
	#run KMeans
	kMeansRes       <- sqlQuery(connection, sql);
	analysisID      <- toString(kMeansRes$ANALYSISID);

	retData = new("FLKMeans",analysis_id = analysisID, odbc_connection = connection, deep_table_name = deepTableName);
	
	return(retData);
}