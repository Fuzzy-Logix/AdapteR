#' @include utilities.R
#' @include data_prep.R
#' @include FLFetch.R
NULL

#' Variance Inflation Factor
#' 
#' Performs variance inflation factor analysis on data
#'
#' @details Variance Inflation Factor is used to identify redundant variables in
#' a dataset. The square root of the variance inflation factor tells you how 
#' much larger the standard error is, compared with what it would be if that 
#' variable were uncorrelated with the other predictor variables in the model.
#'
#' @param table an object of class \code{FLTable}
#' @param primary_key name of primary key column of the table mapped to \code{table}
#' @param response name of the dependent variable column
#' @param exclude vector of names of the columns which are to be excluded
#' @param class_spec list that identifies the value of the categorical variable
#' which is to be used as reference when converting to dummy binary variables
#' @param where_clause condition to filter out data from the table
#' @param note free form string that will be stored with the results, typically 
#' used to document the purpose of the analysis
#'
#' @return an object of class \code{FLVIF}
#'
#' @examples
#' \dontrun{
#' connection <- odbcConnect("Gandalf")
#' table <- FLTable(connection, "FL_R_WRAP", "tblAutoMpg")
#' result <- FLVIF(	table, primary_key = "ObsID", response = "MPG", exclude = c("CarNum","CarNumber"), class_spec = list(CarName = "Audi"))
#' vifResult <- FLFetch(result)
#' }
#'
#' @export
FLVIF <- function( 	table,
					primary_key,
					response,
					exclude = c(),
					class_spec = list(),
					where_clause = "",
					note = "From RWrapper For DBLytix")
{
	#Type validation
	argList  <- as.list(environment())
	typeList <- list(	primary_key  = "character",
						response     = "character",
						exclude      = "character",
						class_spec   = "list",
						where_clause = "character",
						note         = "character")
	classList <- list(	table        = "FLTable")
	validate_args(argList, typeList, classList)
	
	obsID  <- "ObsID"
	varID  <- "VarID"
	value  <- "Num_Val"
	
	dataPrepRes 			<- regr_data_prep( 	table,
												response,
												obs_id       = obsID,
												var_id       = varID,
												value        = value,
												primary_key  = primary_key,
												exclude      = exclude,
												class_spec   = class_spec,
												where_clause = where_clause)
	deepTableName        	<- dataPrepRes$deepTableName
	wideToDeepAnalysisID 	<- dataPrepRes$wideToDeepAnalysisID
	connection         		<- table@odbc_connection
	sqlParameters <- list(  deepTableName  = deepTableName,
							obsID          = obsID,
							varID          = varID,
							value          = value,
							note           = note )
	#print(sql)
	#run VIF
	vifRes     <- run_sql(connection, "FLVIF.sql", sqlParameters)
	#print(vifRes)
	analysisID <- toString(vifRes[1,"AnalysisID"])

	retData = new("FLVIF",analysis_id = analysisID, odbc_connection = connection, deep_table_name = deepTableName, wide_to_deep_analysis_id = wideToDeepAnalysisID)
	
	return(retData)
}