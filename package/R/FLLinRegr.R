#' @include utilities.R
#' @include data_prep.R
#' @include FLFetch.R
NULL

#' Linear Regression
#'
#' Performs Linear Regression
#' 
#' @param table an object of class \code{FLTable}
#' @param primary_key name of primary key column of the table mapped to \code{table}
#' @param response name of the dependent variable column
#' @param exclude vector of names of the columns which are to be excluded
#' @param class_spec list that identifies the value of the categorical variable
#' which is to be used as reference when converting to dummy binary variables 
#' @param where_clause condition to filter out data from the table
#' @param note note (a string)
#'
#' @return returns an object of class \code{FLLinRegr} whose components can be 
#' pulled to R by running FLFetch. The class will then have 2 slots:
#'
#' \code{@stats}:
#' \item{RSquared}          {R-Square}
#' \item{AdjRSquared}       {Adjusted R-Square}
#' \item{StdErr}            {Standard Error}
#' \item{DFRegression}      {Regression Degree of Freedom}
#' \item{DFResidual}        {Residual Degree of Freedom}
#' \item{DFTotal}           {Total Degree of Freedom}
#' \item{SSRegression}      {Regression Sum Squared}
#' \item{SSResidual}        {Residual Sum Squared}
#' \item{SSTotal}           {Total Sum Squared}
#' \item{MSRegression}      {Regression Mean Sum Squared}
#' \item{MSResidual}        {Residual Mean Sum Squared}
#' \item{FStat}             {F-Statistic}
#' \item{SigFStat}          {Significance of F-Statistic}
#' \item{ResidualAutoCorrel}{Lag-1 Auto Correlation between Residuals}
#' \item{WStat}             {Durbin-Watson Test statistic}
#' \item{BPStat}            {Breusch Pagan Test statistic}
#'
#' \code{@coeffs}:
#' \item{COEFFID}       {Coefficient ID}
#' \item{VAR_TYPE}      {Variable Type}
#' \item{COLUMN_NAME}   {Variable Name}
#' \item{CATVALUE}      {Category Name represented by dummy variable}
#' \item{CoeffValue}    {They are coefficients, one for each explanatory variable, that represent the strength and type of relationship the explanatory variable has to the dependent variable.}
#' \item{StdErr}        {Standard Error(an estimate of the standard deviation of the coefficient)}
#' \item{TStat}         {t-Statistic(coefficient divided by its standard error.)}
#' \item{PValue}        {P-Value of t-Statistic}
#' \item{NonZeroDensity}{Proportion of non-zero values for the explanatory variable}
#' \item{CorrelWithRes} {Correlation of the explanatory variable with the residuals}
#'
#' @examples
#' \dontrun{
#' }
#'
#'
#'@export
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
	linRegrRes        		<- run_sql(connection, "FLLinRegr.sql", sqlParameters)
	analysisID       		<- toString(linRegrRes[1,"AnalysisID"])

	analysis <- new("FLLinRegr",analysis_id = analysisID, 
								odbc_connection = connection, 
								deep_table_name = deepTableName, 
								wide_to_deep_analysis_id = wideToDeepAnalysisID)
	
	return(analysis);
}