#' @include utilities.R
NULL

FLMakeAncova <- function(ancovaRes,variable)
{
	DBLytixRows  <- c("between","within")
	RRows        <- c(variable,"Residuals")
	
	DBLytixCols  <- c("df","ss","ms")
	RCols        <- c("Df","Sum Sq","Mean Sq")
	RFStat       <- c("F value","Pr(>F)")
	RCols        <- c(RCols,RFStat)
	DBLytixFStat <- c("f_stat","p_value")

	RetValue     <- data.frame()

	for (i in 1:2 ) {
		temp <- list()
		for (j in 1:3) {
			#str(ancovaRes)
			name <- paste(DBLytixCols[j],DBLytixRows[i],sep="_")
			temp <- c(temp,ancovaRes[1,name])
		}
		temp <- c(temp,ifelse(i < 2,ancovaRes[1,"f_stat"],NA),ifelse(i < 2,ancovaRes[1,"p_value"],NA))
		RetValue <- rbind(RetValue,temp)
	}
	colnames(RetValue) <- RCols
	rownames(RetValue) <- RRows
	RetValue
}
#' Analysis of Co-Variance
#' 
#' Performs analysis of co-variance(ANCOVA).
#
#' @details ANCOVA is used to test if population means of a dependent variable
#' are equal across levels of a categorical independent variable, while 
#' controlling for the effects of other continuous variables (known as 
#' covariates) that are not of primary interest.
#' 
#' @param table An object of class \code{FLTable}
#' @param response column name for the data values
#' @param variable column name for the categorical independent variable
#' @param control column name for continuous independent variable (covariate)
#' @param where_clause condition to filter out data from the table
#'
#' @return A \code{data.frame} with the following columns:
#' \item{Df}{Degrees of Freedom}
#' \item{Sum Sq}{Squared sum from mean}
#' \item{Mean Sq}{Mean squared sum}
#' \item{F value}{Value of the F-Statistic}
#' \item{Pr(>F)}{P-Value}
#' There are two rows in the result, one for the variable term in the model,
#' plus one for \code{Residuals} if there are any. 
#'
#' If all the values in all the categories are the same then the Sum Sq and 
#' Mean Sq for \code{Residuals} are zero and the F Value and P Value are \code{NA}
#' 
#' @examples
#' \dontrun{
#'
#' FLAncova(table = tbl, response = "MPG", variable = "CarName", control = "Weight") 
#' # Select American brands only
#' FLAncova(	table = tbl, 
#'				response = "MPG", 
#'				variable = "CarName", 
#'				control = "Weight", 
#'				where_clause = "CarName IN ('Chrysler', 'Dodge', 'Ford')" )
#'
#' }
#'
#'@export
FLAncova <- function(	table,
						response,
						variable,
						control,						
						where_clause = "")
{
	#Type validation
	argList  <- as.list(environment())
	typeList <- list(	response     = "character",
						variable     = "character",
						control      = "character",
						where_clause = "character")
	classList <- list(	table        = "FLTable")
	validate_args(argList, typeList, classList)

	tableName   <- table@table_name
	whereClause <- ifelse(nchar(where_clause) > 1, where_clause, "1=1")
	
	sqlParameters <- list(	tableName   = tableName,
							variable    = variable,
							control     = control,
							response    = response,							
							whereClause = whereClause)
	connection <- table@odbc_connection
	path       <- "FLAncovaUdt.sql"
	ancovaRes  <- run_sql(connection, path, sqlParameters)
	return( FLMakeAncova(ancovaRes,variable) )
}