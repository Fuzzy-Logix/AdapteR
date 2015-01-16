#' @include utilities.R
#' @include data_prep.R
#' @include FLFetch.R
NULL

#' Logistic Regression
#'
#' Performs Logistic Regression
#'
#' @details The function will perform iterative calculations until convergence
#' is achieved or the maximum number of iterations as specified by \code{max_iter} is crossed
#'
#'@param table an object of class \code{FLTable}
#'@param primary_key name of primary key column of the table mapped to \code{table}
#'@param response name of the dependent variable column
#'@param max_iter Maximum number of iterations done before terminating the algorithm
#'@param threshold threshold for False positive value used to calculate
#' the false positives and false negatives.
#'@param exclude vector of names of the columns which are to be excluded
#'@param class_spec list that identifies the value of the categorical variable
#' which is to be used as reference when converting to dummy binary variables
#'@param where_clause condition to filter out data from the table
#'@param note note (a string)
#'
#'@return returns an object of class \code{FLLogRegr} whose components can be
#' pulled to R by running FLFetch. The class will then have 2 slots:
#'
#' \code{stats}:
#' \item{Concordant}{Concordant Pairs}
#' \item{Discordant}{Discordant Pairs}
#' \item{Tied}{Tied Pairs}
#' \item{TotalPairs}{Total Pairs}
#' \item{GiniCoeff}{(Concordant -Discordant)/TotalPairs}
#' \item{CStatistic}{(GiniCoeff +1)/2}
#' \item{Gamma}{(Concordant - Discordant)/(Concordant + Discordant)}
#' \item{HighestPValue}{Highest P-Value}
#' \item{Events}{Number of Events (the 1's)}
#' \item{NonEvents}{Number of Non-Events (the 0's)}
#' \item{FalsePositive}{Number of false positives}
#' \item{FalseNegative}{Number of False negatives}
#'
#' \code{coeffs}:
#' \item{COEFFID}{Coefficient ID}
#' \item{VAR_TYPE}{Variable Type}
#' \item{COLUMN_NAME}{Variable Name}
#' \item{CATVALUE}{Category Name represented by dummy variable}
#' \item{CoeffValue}{They are coefficients, one for each explanatory variable, that represent the strength and type of relationship the explanatory variable has to the dependent variable.}
#' \item{StdErr}{Standard Error(an estimate of the standard deviation of the coefficient)}
#' \item{CHISQ}{Chi-Square estimate of the StdErr}
#' \item{PValue}{P-Value for the Chi-Square estimate}
#'
#'@examples
#' \dontrun{
#'  
#' 	LogRegModel <- FLLogRegr(TblIris, 'ObsID', 'species', max_iter = 20, threshold = 0.8, note = "RWrappers Example")
#' 	LogRegModel <- FLFetch(LogRegModel)
#'  }
#'
#'@export
FLLogRegr <- function( 	table,
						primary_key,
						response,
						max_iter,
						threshold,
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
	typeList <- list(	table        = "FLTable",
						primary_key  = "character",
						response     = "character",
						max_iter     = "integer",
						threshold    = "double",
						exclude      = "character",
						class_spec   = "list",
						where_clause = "character",
						note         = "character")
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

	sqlParameters 			<- list(	deepTableName = deepTableName,
										obsID         = obsID,
										varID         = varID,
										value         = value,
										maxIter       = toString(max_iter),
										threshold     = toString(threshold),
										note          = note )

	#run FLLogRegr
	logRegrRes  <- run_sql(connection, "FLLogRegr.sql", sqlParameters)
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
