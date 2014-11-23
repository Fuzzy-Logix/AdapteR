wide_to_deep <- function( 	table,
							primary_key,
							obs_id 	= "ObsID",
							var_id 	= "VarID",
							value  	= "Num_Val",					
							exclude = c(),
							class_spec = list(),
							where_clause = "")
{	
	
	deepTableName   <- gen_deep_table_name(table@table_name)
	excludeString   <- list_to_exclude_clause(exclude)
	classSpecString <- list_to_class_spec(class_spec)
	path            <- "SQL//WideToDeep.sql";
	stopifnot(file.exists(path));
	sql 			<- readChar(path, nchar = file.info(path)$size);
	sql 			<- sprintf(	sql, 
								table@table_name,
								primary_key, 
								deepTableName,
								obs_id,
								var_id,
								value,
								excludeString,
								classSpecString,
								where_clause);
	sql 			<- gsub("[\r\n]", "", sql);
	#print(sql);
	#sqlQuery(table@odbc_connection,paste("DROP TABLE",deepTableName));
	res  			<- sqlQuery(table@odbc_connection, sql, stringsAsFactors = FALSE);
	#print(res);
	deepTableName
}

regr_data_prep <- function( table,
							primary_key   = FLPrimaryKey(table),
							response,
							obs_id = "ObsID",
							var_id = "VarID",
							value = "Num_Val",							
							exclude      = c(),
							class_spec    = list(),
							perform_norm = 0,
							perform_var_reduc = 0,
							make_data_sparse = 1,
							min_std_dev = 0,
							max_correl = 0,
							where_clause  = "")
{
		
	deepTableName   <- gen_deep_table_name(table@table_name)
	excludeString   <- list_to_exclude_clause(exclude)
	classSpecString <- list_to_class_spec(class_spec)
	catToDummy 		<- calc_cat_to_dummy(class_spec)
	path            <- "SQL//FLRegrDataPrep.sql";
	stopifnot(file.exists(path));
	sql 			<- readChar(path, nchar = file.info(path)$size);
	sql 			<- sprintf(	sql, 
								table@table_name,
								primary_key, 
								response,
								deepTableName,
								obs_id,
								var_id,
								value,
								catToDummy,
								perform_norm,
								perform_var_reduc,
								make_data_sparse,
								min_std_dev,
								max_correl,
								toString(0),
								excludeString,
								classSpecString,
								where_clause);
	sql 			<- gsub("[\r\n]", "", sql);
	#print(sql);
	#stop("DEBUG");
	sqlQuery(table@odbc_connection,paste("DROP TABLE",deepTableName));
	res <- sqlQuery(table@odbc_connection, sql, stringsAsFactors = FALSE);
	#str(res);
	#print(res);
	analysisID <- as.character(res[1,"OutAnalysisID"]);
	list( deepTableName = deepTableName, wideToDeepAnalysisID = analysisID)
}

regr_data_prep_score <- function( table,
								 obs_id = "ObsID",
								 var_id = "VarID",
								 value = "Num_Val",
								 primary_key,
								 exclude      = c(),
								 class_spec    = list(),
								 perform_norm = 0,
								 perform_var_reduc = 0,
								 make_data_sparse = 1,
								 min_std_dev = 0,
								 max_correl = 0,
								 where_clause  = "",
								 in_analysis_id)
{
		
	deepTableName   <- gen_deep_table_name(table@table_name)
	excludeString   <- list_to_exclude_clause(exclude)
	classSpecString <- list_to_class_spec(class_spec)
	catToDummy 		<- calc_cat_to_dummy(class_spec)
	path            <- "SQL//FLRegrDataPrepScore.sql";
	stopifnot(file.exists(path));
	sql 			<- readChar(path, nchar = file.info(path)$size);
	sql 			<- sprintf(	sql, 
								table@table_name,
								primary_key, 
								deepTableName,
								obs_id,
								var_id,
								value,
								catToDummy,
								perform_norm,
								perform_var_reduc,
								make_data_sparse,
								min_std_dev,
								max_correl,
								toString(1),
								excludeString,
								classSpecString,
								where_clause,
								in_analysis_id);
	sql 			<- gsub("[\r\n]", "", sql);
	#print(sql);
	#stop("DEBUG");
	sqlQuery(table@odbc_connection,paste("DROP TABLE",deepTableName));
	res 			<- sqlQuery(table@odbc_connection, sql, stringsAsFactors = FALSE);
	analysisID 		<- as.character(res[1,"OutAnalysisID"]);
	list( deepTableName = deepTableName, wideToDeepAnalysisID = analysisID)
}