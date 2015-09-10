#' @include utilities.R
#' @include FLTable.R
NULL
#' An S4 class to represent FLVector
#'
#' @slot table object of class FLTable
#' @slot col_name A character column name
setClass(
	"FLVector",
	slots = list(
		table = "FLTable",
		col_name = "character"
	)
)

is.FLVector <- function(object)
{
	ifelse(class(object)=="FLVector",TRUE,FALSE)
	
}

FLVector <- function(table, col_name)
{
	sqlQuery(table@odbc_connection, paste0(" DATABASE ",table@db_name,"; SET ROLE ALL;"))
	new("FLVector", table = table, col_name = col_name)
}
