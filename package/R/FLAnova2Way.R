#' @import utilities.R
NULL

make_anova2way <- function(anovaRes,variable1,variable2)
{
	DBLytixRows  <- c(variable1,variable2,"Interaction","Error")
	RRows        <- c(variable1,variable2,paste(variable1,variable2,sep=":"),"Residuals")
	
	DBLytixCols  <- c("DF","SS","MS")
	RCols        <- c("Df","Sum Sq","Mean Sq")
	RFStat       <- c("F value","Pr(>F)")
	RCols        <- c(RCols,RFStat)
	DBLytixFStat <- "FStat"
	DBLytixPVal <- "PValue"

	RetValue     <- data.frame()
	for (i in 1:4 ) {
		temp <- list()
		for (j in 1:3) {
			
			name <- paste(DBLytixRows[i],DBLytixCols[j],sep="_")
			temp <- c(temp,anovaRes[1,name])
		}
		
		f_stat <- paste(DBLytixRows[i],DBLytixFStat,sep="_")
		p_value <- paste(DBLytixRows[i],DBLytixPVal,sep="_")
		temp <- c(temp,ifelse(i <= 3,anovaRes[1, f_stat],NA),ifelse(i <= 3,anovaRes[1,p_value],NA))
		RetValue <- rbind(RetValue,temp)
	}
	colnames(RetValue) <- RCols
	rownames(RetValue) <- RRows
	RetValue
}
#' Two-Way Analysis of Variance
#' 
#' Performs two-way analysis of variance(ANOVA).
#
#' @details Two-way analysis of variance is used to test the difference between
#' two or more groups in a dataset based on two attribute.
#' 
#' @param table An object of class \code{FLTable}
#' @param response column name for the data values
#' @param variable1 column name for the first categorical variable
#' @param variable2 column name for the second categorical variable
#'
#' @return A \code{data.frame} with the following columns:
#' \item{Df}{Degrees of Freedom}
#' \item{Sum Sq}{Squared sum from mean}
#' \item{Mean Sq}{Mean squared sum}
#' \item{F value}{Value of the F-Statistic}
#' \item{Pr(>F)}{P-Value}
#' There are three rows in the result, one for each of the variable terms in the
#' model, plus one for \code{Residuals} if there are any. 
#'
#' If all the values in all the categories are the same then the Sum Sq and 
#' Mean Sq for \code{Residuals} are zero and the F Value and P Value are \code{NA}
#' 
#' @examples
#' \dontrun{
#'
#' FLAnova2Way(table = tbl, response = "MPG", variable1 = "CarName", variable2 = "Origin") 
#'
#' }
#'
#'@export
FLAnova2Way <- function(	table,
							response,
							variable1,
							variable2 )
	{
		tableName     <- table@table_name
		#whereClause  <- ifelse(nchar(where_clause) > 1, where_clause, "1=1");
		
		path          <- "FLAnova2Way.sql";
		connection    <- table@odbc_connection
		sqlParameters <- list(	tableName    = tableName,
								response     = response,   
								variable1    = variable1,   
								variable2    = variable2)
		anovaVolTable <- run_sql(connection, path, sqlParameters)
		anovaRes      <- sqlQuery(connection, paste("SELECT * FROM",anovaVolTable[1,1]), stringsAsFactors = FALSE)
		return( make_anova2way(anovaRes,variable1,variable2) )
}