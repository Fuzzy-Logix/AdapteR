#' @import utilities.R
 
wide_to_deep <- function( 	table,
							primary_key,
							obs_id       = "ObsID",
							var_id       = "VarID",
							value        = "Num_Val",					
							exclude      = c(),
							class_spec   = list(),
							where_clause = "")
{	
	
	deepTableName   <- gen_deep_table_name(table@table_name)
	excludeString   <- list_to_exclude_clause(exclude)
	classSpecString <- list_to_class_spec(class_spec)
	connection      <- table@odbc_connection
	path            <- "WideToDeep.sql"
	sqlParameters <- list( 	tableName       = table@table_name,
							primaryKey      = primary_key,
							deepTableName   = deepTableName,
							obsID           = obs_id,
							varID           = var_id,
							value           = value,
							excludeString   = excludeString,
							classSpecString = classSpecString,
							whereClause     = where_clause						
							)	
	res <- run_sql(connection, path, sqlParameters)
	
	#sqlQuery(table@odbc_connection,paste("DROP TABLE",deepTableName))
	#print(res)
	deepTableName
}

regr_data_prep <- function( table,
							primary_key,
							response,
							obs_id            = "ObsID",
							var_id            = "VarID",
							value             = "Num_Val",							
							exclude           = c(),
							class_spec        = list(),
							perform_norm      = 0,
							perform_var_reduc = 0,
							make_data_sparse  = 1,
							min_std_dev       = 0,
							max_correl        = 0,
							where_clause      = "")
{
		
	deepTableName   <- gen_deep_table_name(table@table_name)
	excludeString   <- list_to_exclude_clause(exclude)
	classSpecString <- list_to_class_spec(class_spec)
	catToDummy 		<- calc_cat_to_dummy(class_spec)
	connection      <- table@odbc_connection
	path            <- "FLRegrDataPrep.sql"

	sqlParameters <- list( 	tableName       = table@table_name,
							primaryKey      = primary_key,
							response        = response,
							deepTableName   = deepTableName,
							obsID           = obs_id,
							varID           = var_id,
							value           = value,
							catToDummy      = catToDummy,
							performNorm     = perform_norm,
							performVarReduc = perform_var_reduc,
							makeDataSparse  = make_data_sparse,
							minStdDev       = min_std_dev,
							maxCorrel       = max_correl,
							excludeString   = excludeString,
							classSpecString = classSpecString,
							whereClause     = where_clause						
							)	
	res        <- run_sql(connection, path, sqlParameters)
	analysisID <- as.character(res[1,"OutAnalysisID"])
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
	path            <- "FLRegrDataPrepScore.sql"
	connection      <- table@odbc_connection
	sqlParameters <- list( 	tableName       = table@table_name,
							primaryKey      = primary_key,
							response        = response,
							deepTableName   = deepTableName,
							obsID           = obs_id,
							varID           = var_id,
							value           = value,
							catToDummy      = catToDummy,
							performNorm     = perform_norm,
							performVarReduc = perform_var_reduc,
							makeDataSparse  = make_data_sparse,
							minStdDev       = min_std_dev,
							maxCorrel       = max_correl,
							excludeString   = excludeString,
							classSpecString = classSpecString,
							whereClause     = where_clause,
							inAnalysisID    = in_analysis_id						
							)	
	res        <- run_sql(connection, path, sqlParameters)
	analysisID <- as.character(res[1,"OutAnalysisID"])
	list( deepTableName = deepTableName, wideToDeepAnalysisID = analysisID)
}