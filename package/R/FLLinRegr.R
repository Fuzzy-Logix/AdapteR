FLLinRegr <- function( 	table,
						primary_key,
						response,												
						exclude      = as.character(c()),
						class_spec    = list(),
						where_clause  = "",
						note     = "From RWrapper For DBLytix")
{
	#Type validation
	argList  <- as.list(environment())
	typeList <- list(	table        = "character",
						primary_key  = "character",
						response     = "character",												
						exclude      = "character",
						class_spec   = "list",
						where_clause = "character",
						note         = "character")
	validate_args(argList, typeList)

	#Data prep
	obsID  <- "ObsID";
	varID  <- "VarID";
	value  <- "Num_Val"	
	dataPrepRes 			<- regr_data_prep( 	table,
									response,
									obs_id       = obsID,
									var_id       = varID,
									value        = value,
									primary_key  = primary_key,
									exclude      = exclude,
									class_spec   = class_spec,
									where_clause = where_clause);
	deepTableName        	<- dataPrepRes$deepTableName
	wideToDeepAnalysisID 	<- dataPrepRes$wideToDeepAnalysisID

	#Query Execution: run FLLinRegr
	connection         		<- table@odbc_connection
	sqlParameters 			<- list(	deepTableName = deepTableName,
										obsID  = obsID,  
										varID  = varID, 
										value  = value, 							
										note   = note )
	linRegrRes        		<- runsql(connection, "SQL//FLLinRegr.sql", sqlParameters)
	analysisID       		<- toString(linRegrRes[1,"AnalysisID"])

	analysis <- new("FLLinRegr",analysis_id = analysisID, 
								odbc_connection = connection, 
								deep_table_name = deepTableName, 
								wide_to_deep_analysis_id = wideToDeepAnalysisID)
	
	return(analysis);
}