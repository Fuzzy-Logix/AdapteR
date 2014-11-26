FLLogRegr <- function( 	table,
						primary_key,
						response,
						max_iter,
						purity_threshold,
						exclude      = as.character(c()),
						class_spec   = list(),
						where_clause = "",
						note         = "From RWrapper For DBLytix")
{
	#Type validation
	max_iter <- ifelse(	is_number(max_iter),
						as.integer(max_iter),
						stop("max_iter should be an integer"))

	argList  <- as.list(environment())
	typeList <- list(	table            = "character",
						primary_key      = "character",
						response         = "character",
						max_iter         = "integer",
						purity_threshold = "double",												
						exclude          = "character",
						class_spec       = "list",
						where_clause     = "character",
						note             = "character")
	validate_args(argList, typeList)

	obsID  <- "ObsID";
	varID  <- "VarID";
	value  <- "Num_Val";
	
	dataPrepRes 			<- regr_data_prep( 	table,
												response,
												obs_id = obsID,
												var_id = varID,
												value  = value,
												primary_key  = primary_key,
												exclude      = exclude,
												class_spec   = class_spec,
												where_clause = where_clause);
	
	deepTableName       	<- dataPrepRes$deepTableName
	wideToDeepAnalysisID	<- dataPrepRes$wideToDeepAnalysisID
	connection	        	<- table@odbc_connection;
	
	sqlParameters 			<- list(	deepTableName   = deepTableName,
										obsID           = obsID,
										varID           = varID,
										value           = value,
										maxIter         = toString(max_iter),
										purityThreshold = toString(purity_threshold),
										note            = note )
	
	#run FLLogRegr
	logRegrRes  <- run_sql(connection, "SQL//FLLogRegr.sql", sqlParameters)
	analysisID  <- toString(logRegrRes[[1,"ANALYSISID"]])

	retData = new("FLLogRegr",	analysis_id           	 = analysisID,
								wide_to_deep_analysis_id = wideToDeepAnalysisID, 
								deep_table_name        	 = deepTableName, 
								class_spec           	 = class_spec, 
								primary_key          	 = primary_key, 
								exclude              	 = as.character(exclude), 
								odbc_connection       	 = connection)
								
	return(retData)
}