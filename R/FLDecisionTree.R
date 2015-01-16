#' @include utilities.R
#' @include data_prep.R
#' @include FLFetch.R
NULL

#' Decision Tree
#'
#' Performs decision tree analysis of the data.
#' @details Decision tree is one of the most widely used classification algorithms.
#' Input data includes a set of Y (dependent variable, also known as target
#' variable or class) values and multiple sets of X (independent variables,
#' also known as predictor variables) values.
#' The input data table, also known as a training data set, is a wide table.
#' All values in such a table are numeric. The class variable is categorical,
#' with two or more categories.
#' Depending on different independent variable types, the function \code{FLDecisionTree}
#' will use a different algorithm to build the decision tree. If the independent
#' variables have continuous values, a method that is based on the CART algorithm
#' is used. During this process, independent variables can be reused if needed.
#' If the independent variables have binary values, a method based on the ID3 algorithm
#' is used. Variables are not reused in this process. In both cases, a binary
#' decision tree is constructed by splitting a node into two child nodes repeatedly.
#'
#' @param table an object of class \code{FLTable}
#' @param primary_key name of primary key column of the table mapped to \code{table}
#' @param response name of the dependent variable column of the table mapped to
#' \code{table}
#' @param min_obs_for_parent minimum number of observations for a given node to
#' be a parent node. It is used for early termination of some iterations once a
#' given node has less than \code{min_obs_for_parent} number of observations.
#' Note that \code{min_obs_for_parent} > 1
#' @param max_level positive integer specifying the maximum tree level this
#' tree can grow to
#' @param purity_threshold threshold used for early termination of some iteration
#' once a certain percentage (specified by this threshold) of records has the
#' same class value.
#' Note that  1 > \code{purity_threshold} > 0
#' @param exclude vector of names of the columns which are to be excluded
#' @param class_spec list that identifies the value of the categorical variable
#' which is to be used as reference when converting to dummy binary variables
#' @param where_clause condition to filter out data from the table
#' @param note note

#' @return \code{FLDecisionTree} returns an object of class \code{FLDecisionTree}.
#' The components of this class mentioned below can be pulled in R using the
#' generic \code{FLFetch}.
#' \item{node_info}{a \code{data.frame} which stores all nodes of the decision tree built, including leave
#' nodes and decision nodes (also known as splitting nodes).}
#' \item{classification}{a \code{data.frame} which stores observations' classifications}

#' @examples
#' \dontrun{
#' connection <- odbcConnect("Gandalf")
#' db_name    <- "FL_R_WRAP"
#' table_name <- "tblAutoMpg"
#' # Create FLTable object
#' table      <-  FLTable(connection, db_name, table_name)
#' # Perform Decision Tree Analysis
#' result     <- FLDecisionTree(table,
#'                              primary_key = "ObsID",
#'                              response = "Weight",
#'                              min_obs_for_parent = 10,
#'                              max_level = 5,
#'                              purity_threshold = 0.95,
#'                              exclude = c("CarNum", "CarNumber"),
#'                              class_spec = list(CarName = "BMW"))
#' # Fetch reults in R
#' decisionTreeResult <- FLFetch(result)
#' }

#' @export
FLDecisionTree <- function( table,
							primary_key,
							response,
							min_obs_for_parent,
							max_level,
							purity_threshold,
							exclude      = c(),
							class_spec    = list(),
							where_clause  = "",
							note     = "From RWrapper For DBLytix")
{
	#Type validation
	min_obs_for_parent <- ifelse(	is_number(min_obs_for_parent), 
									as.integer(min_obs_for_parent), 
									stop("min_obs_for_parent should be an integer"))
	
	max_level          <- ifelse(	is_number(max_level),
									as.integer(max_level),
									stop("max_level should be an integer"))

	argList  <- as.list(environment())
	typeList <- list(	table              = "FLTable",
						primary_key        = "character",
						response           = "character",
						min_obs_for_parent = "integer",
						max_level          = "integer",
						purity_threshold   = "double",												
						exclude            = "character",
						class_spec         = "list",
						where_clause       = "character",
						note               = "character")
	validate_args(argList, typeList)

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

	deepTableName        <- dataPrepRes$deepTableName
	wideToDeepAnalysisID <- dataPrepRes$wideToDeepAnalysisID
	connection           <- table@odbc_connection
	
	file          <- "FLDecisionTree.sql"
	sqlParameters <- list(	deepTableName   = deepTableName,
							obsID           = obsID,
							varID           = varID,
							value           = value,
							whereClause     = where_clause,
							minObsForParent = min_obs_for_parent,
							maxLevel        = max_level,
							purityThreshold = purity_threshold,
							note            = note )

	#run FLDecisionTree
	decisionTreeRes <- run_sql(connection, file, sqlParameters)
	analysisID      <- toString(decisionTreeRes[[1,"ANALYSISID"]])

	retData = new("FLDecisionTree",analysis_id = analysisID, wide_to_deep_analysis_id = wideToDeepAnalysisID, deep_table_name = deepTableName, class_spec = class_spec, primary_key = primary_key, exclude = as.character(exclude), odbc_connection = connection)

	return(retData)
}
