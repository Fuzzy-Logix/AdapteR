#' z-Test
#' 
#' Performs one and two sample z-tests on vectors of data. 
#'
#' @details One-sample Z-test is used to test if population mean of the sample is 
#' significantly different from a given reference value.
#' Two-sample Z-test is used to determine if the population means from the two
#' samples are equal.
#' 
#' @param table An object of class \code{FLTable}
#' @param primary_key primary key column of the table mapped to FLTable
#' @param input1 column name for first sample input vector
#' @param input2 (only in case of two-sample test) column name for second sample input vector
#' @param mu (only in case of one-sample test) reference mean
#' @param num_tails number of tails in test distribution. This decides whether
#' one-tailed test or two-tailed test is performed \cr Ref: \url{http://www.ats.ucla.edu/stat/mult_pkg/faq/general/tail_tests.htm}
#'
#' @return returns the value of z-test statistic and probability (P-Value) of test statistic
#'
#' If the input vector contains zero non-null observations, the function returns a NULL. Also, if the standard 
#' deviation of values in the input vector is zero, the function returns a NULL value.
#' 
#' @examples
#' \dontrun{
#' FLtTest(table = tbl, primary_key = "ObsID", input1 = "Var1", mu = 0.45, num_tails = 2)
#' FLtTest(table = tbl, primary_key = "ObsID", input1 = "Var1", input2 = "Var2", num_tails = 2)
#' }
FLzTest <- function(table, 
					primary_key = "ObsID",
					input1, 
					input2 = NULL, 
					mu = 0, 
					num_tails = 2) 
{
	if(length(input2) == 0) 
	{
		connection    <- table@odbc_connection
		path <- "SQL//FLzTest1S.sql"		
		sqlParameters <- list( 	tableName = table@table_name,
								input1    = input1,
								mu        = mu,
								numTails  = num_tails)		
		res <- run_sql(connection, path, sqlParameters)
		return(res)
	}
	else {
		# For 2 sample ZTest we take a wide table as input 
		# and convert it to a deep table
		connection    <- table@odbc_connection
		obsIDColName  <- "ObsID"
		varIDColName  <- "GroupID"
		valueColName  <- "Num_Val"
		whereClause   <- ""
		deepTableName <- wide_to_deep(	table,
										primary_key  = primary_key,
										obs_id       = obsIDColName,
										var_id       = varIDColName,
										value        = valueColName,
										exclude      = c(),
										class_spec   = list(),
										where_clause = whereClause)
		path <- "SQL//FLzTest2S.sql"
		sqlParameters <- list( 	tableName = table@table_name,								
								numTails  = num_tails)		
		res <- run_sql(connection, path, sqlParameters)
		sqlQuery(connection,paste("DROP TABLE",deepTableName));
		return(res)
	}
}