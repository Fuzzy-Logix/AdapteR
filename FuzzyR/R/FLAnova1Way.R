#' @include utilities.R
NULL

make_anova <- function(anovaRes,variable)
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
			#str(anovaRes)
			name <- paste(DBLytixCols[j],DBLytixRows[i],sep="_")
			temp <- c(temp,anovaRes[1,name])
		}
		temp <- c(temp,ifelse(i < 2,anovaRes[1,"f_stat"],NA),ifelse(i < 2,anovaRes[1,"p_value"],NA))
		RetValue <- rbind(RetValue,temp)
	}
	colnames(RetValue) <- RCols
	rownames(RetValue) <- RRows
	RetValue
}

#' One-Way Analysis of Variance
#' 
#' Performs one-way analysis of variance(ANOVA).
#
#' @details One-way analysis of variance is used to test the difference between
#' two or more groups in a dataset based on a single attribute.
#' 
#' @param table An object of class \code{FLTable}
#' @param response column name for the data values
#' @param variable column name for the categorical variable
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
#' FLAnova1Way(table = tbl, response = "MPG", variable = "CarName") 
#' # Select American brands only
#' FLAnova1Way(	table = tbl, 
#' 				response = "MPG", 
#'				variable = "CarName", 
#'				where_clause = "CarName IN ('Chrysler', 'Dodge', 'Ford')" )
#'
#' }
#'
#'@export
FLAnova1Way <- function(	table,
				            response,
				            variable,
				            where_clause = "")
	{
		tableName     <- table@table_name
		whereClause   <- ifelse(nchar(where_clause) > 1, where_clause, "1=1")
		connection    <- table@odbc_connection
		sqlParameters <- list( 	tableName   = tableName,
								response    = response, 
								variable    = variable, 
								whereClause = whereClause)
		anovaRes      <- run_sql(connection, "FLAnova1WayUdt.sql", sqlParameters)
		return( make_anova(anovaRes,variable) );
}
