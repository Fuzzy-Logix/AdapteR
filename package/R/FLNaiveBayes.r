FLNaiveBayes <- function( 	table,
							primary_key,
							response,
							laplace     = 0,
							exclude     = c(),
							class_spec   = list(),
							where_clause = "",
							note        = "From RWrapper For DBLytix")
{

	obsID  <- "ObsID";
	varID  <- "VarID";
	value  <- "Num_Val";
	
	dataPrepRes 	<- regr_data_prep( 	table,
										response,
										obs_id = obsID,
										var_id = varID,
										value = value,
										primary_key   = primary_key,
										exclude      = exclude,
										class_spec    = class_spec,
										where_clause  = where_clause);
	deepTableName 	<- dataPrepRes$deepTableName
	connection 		<- table@odbc_connection;
	sql        		<- "CALL FLNaiveBayesModel('";
	sqlParameters 	<- paste(	deepTableName,
								obsID,  
								varID, 
								value,
								laplace, 							
								note, sep="','")
	sql           	<- paste(sql, sqlParameters,"', AnalysisID)", sep="")
	
	#run NaiveBayes
	naiveBayesRes  	<- sqlQuery(connection, sql);
	
	analysisID      <- toString(naiveBayesRes[1,"AnalysisID"]);

	retData = new("FLNaiveBayes",analysis_id = analysisID, odbc_connection = connection, deep_table_name = deepTableName);
	
	return(retData);
}