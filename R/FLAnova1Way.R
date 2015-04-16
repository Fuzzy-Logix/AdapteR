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
		if(i < 2) {
		  temp <- c(temp, anovaRes[1, "f_stat"], anovaRes[1, "p_value"])
		} else {
		  temp <- c(temp, NA, NA)
		}
    #temp <- c(temp,ifelse(i < 2,anovaRes[1,"f_stat"],NA),ifelse(i < 2,anovaRes[1,"p_value"],NA))
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
#' connection <- odbcConnect("Gandalf")
#' db_name    <- "FL_R_WRAP"
#' table_name <- "tblAutoMpg"
#' # Create FLTable object
#' table      <-  FLTable(connection, db_name, table_name)
#' anova1WayResult <- FLAnova1Way(table, response = "MPG", variable = "CarName")
#' # Select American brands only
#' anova1WayResult <- FLAnova1Way(	table, response = "MPG", variable = "CarName", where_clause = "CarName IN ('Chrysler', 'Dodge', 'Ford')" )
#' }
#'
#'@export
FLAnova1Way <- function(	table,
				            response,
				            variable,
				            where_clause = "")
{
		#Type validation
	argList  <- as.list(environment())
	typeList <- list(	response     = "character",
						variable     = "character",						
						where_clause = "character")
	classList <- list(	table        = "FLTable")
	validate_args(argList, typeList, classList)

	tableName     <- table@table_name
	
  if(nchar(where_clause) > 1) {
	  whereClause <- where_clause
	} else {
	  whereClause <- "1=1" 
	}
  #whereClause   <- ifelse(nchar(where_clause) > 1, where_clause, "1=1")
	
  connection    <- table@odbc_connection
	sqlParameters <- list( 	tableName   = tableName,
							response    = response, 
							variable    = variable, 
							whereClause = whereClause)
	anovaRes      <- run_sql(connection, "FLAnova1WayUdt.sql", sqlParameters)
	return( make_anova(anovaRes,variable) );
}
