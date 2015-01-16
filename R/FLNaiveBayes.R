#' @include utilities.R
#' @include data_prep.R
#' @include FLFetch.R
NULL

#' Naive Bayes Classifier
#'
#' Naive Bayes is a simple probabilistic classifier that applies the Bayes'
#' theorem to compute conditional a-posterior probabilities of a categorical
#' class variable under the independence assumption -- the presence
#' (or absence) of a particular feature of a class is unrelated to the
#' presence (or absence) of any other feature.
#'
#' @details Laplacian Correction is used to avoid the issue of zero probability
#' for a given attribute by adding 1 to the numerator. In order to compensate
#' for this addition, the denominator is also incremented by the total number
#' of discrete values for the attribute
#'
#' @param table an object of class \code{FLTable}
#' @param primary_key name of primary key column of the table mapped to \code{table}
#' @param response name of the dependent variable column
#' @param laplace indicates whether Laplacian Correction is to be used (1 for
#' true and 0 for false)
#' @param exclude vector of names of the columns which are to be excluded
#' @param class_spec list that identifies the value of the categorical variable
#' which is to be used as reference when converting to dummy binary variables
#' @param where_clause condition to filter out data from the table
#' @param note free form string that will be stored with the results, typically
#' used to document the purpose of the analysis
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export
FLNaiveBayes <- function( 	table,
							primary_key,
							response,
							laplace      = 0,
							exclude      = c(),
							class_spec   = list(),
							where_clause = "",
							note         = "From RWrapper For DBLytix")
{
	laplace <- ifelse(	(laplace == 0 || laplace == 1),
						as.integer(laplace),
						stop("laplace should be 1 or 0"))

	argList  <- as.list(environment())
	typeList <- list(	table        = "FLTable",
						primary_key  = "character",
						response     = "character",
						laplace      = "integer",
						exclude      = "character",
						class_spec   = "list",
						where_clause = "character",
						note         = "character")
	validate_args(argList, typeList)

	obsID <- "ObsID"
	varID <- "VarID"
	value <- "Num_Val"

	dataPrepRes <- regr_data_prep( 	table,
										response,
										obs_id = obsID,
										var_id = varID,
										value = value,
										primary_key   = primary_key,
										exclude      = exclude,
										class_spec    = class_spec,
										where_clause  = where_clause)
	deepTableName <- dataPrepRes$deepTableName
	connection    <- table@odbc_connection

	file <- "FLNaiveBayesModel.sql"
	sqlParameters <- list(	deepTableName = deepTableName,
							obsID         = obsID,
							varID         = varID,
							value         = value,
							laplace       = toString(laplace),
							note          = note )
	#run NaiveBayes
	naiveBayesRes <- run_sql(connection, file, sqlParameters)

	analysisID    <- toString(naiveBayesRes[1,"AnalysisID"])

	retData = new("FLNaiveBayes",analysis_id = analysisID, odbc_connection = connection, deep_table_name = deepTableName)

	return(retData)
}
