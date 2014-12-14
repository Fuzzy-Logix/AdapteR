#' Fetch Table 
#'
#' Pulls a Table from a database into R
#'
#' \code{FLFetchTable} retrieves a table (from the database) to which a FLTable object is
#' mapped.
#'
#' @param table an object of class FLTable.
#' @return a data frame with each column of the table as one variable
#'
#' @export
FLFetchTable <- function(table)
{
	connection    <- table@odbc_connection
	file          <- "FLFetchTable.sql"
	sqlParameters <- list(tableName = table@table_name)

	res    <- run_sql(connection, file, sqlParameters)
	res
}