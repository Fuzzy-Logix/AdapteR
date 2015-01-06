#' @include utilities.R

setOldClass("RODBC") 

#'@export
setClass("FLTable", 
		slots = list(	odbc_connection = "RODBC", 
						db_name         = "character", 
						table_name      = "character"))

#' Constructor function for FLTable
#'
#' \code{FLTable} constructs an object of class \code{FLTable}. This object is
#' used as the input for analytic functions
#'
#' @param connection ODBC connection handle as returned by \code{\link[RODBC]{odbcConnect}}
#' @param database name of the database in \code{Teradata} which contains the table
#' @param table name of the  table
#' @return \code{FLTable} returns an object of class FLTable mapped to a table 
#' in Teradata.
#' @examples
#' \dontrun{
#'
#' 		connection <- odbcConnect("Gandalf")
#' 		database   <- "FL_R_WRAP"
#' 		table_name <- "tblAutoMpg"
#' 		tbl        <- FLMatrix(connection, database, table_name)
#'
#' }
#' @export
FLTable <- function(connection, database, table) {

	validate_args( 	list(database = database, table = table),
					list(database = "character", table = "character"))

	sqlQuery(connection, paste("DATABASE", database))
	sqlQuery(connection, "SET ROLE ALL")

	new("FLTable", odbc_connection = connection,db_name = database, table_name = table)
}