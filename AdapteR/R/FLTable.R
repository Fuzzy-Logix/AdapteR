#' @include utilities.R
NULL
setOldClass("RODBC")
#' An S4 class to represent FLTable
#'
#' @slot odbc_connection ODBC connectivity for R
#' @slot db_name character
#' @slot table_name character
#' @slot primary_key character
#' @slot var_id_name character 
#' @slot num_val_name character
#' @slot isDeep logical
#' @method names FLTable
#' @param object retrieves the column names of FLTable object
setClass(
	"FLTable",
	slots = list(
		odbc_connection = "ANY",
		db_name         = "character",
		table_name      = "character",
		primary_key="character",
		var_id_name = "character",
		num_val_name = "character",
		isDeep = "logical"
	)
)
#' Constructor function for FLTable.
#'
#' \code{FLTable} constructs an object of class \code{FLTable}.
#'
#' \code{FLTable} refers to an in-database table. This is equivalent to data.frame object. 
#' This object is commonly used as input for data mining functions.
#' @param connection ODBC connection handle as returned by \code{\link[RODBC]{odbcConnect}}
#' @param database name of the database
#' @param table name of the table
#' @param primary_key column name set as primary key
#' @param var_id_name column name where variable id's are stored if \code{FLTable} is deep
#' @param num_val_name column name where cell values are stored if \code{FLTable} is deep
#' @return \code{FLTable} returns an object of class FLTable mapped to a table
#' in Teradata.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' widetable  <- FLTable(connection, "FL_TRAIN", "tblAbaloneWide", "ObsID")
#' names(widetable)
#' @export
FLTable <- function(connection,
				    database, 
				    table,
				    primary_key,
				    var_id_name = character(0), 
				    num_val_name=character(0))
{

	# validate_args( 	list(database = database, table = table),
	# 				list(database = "character", table = "character")
	# )

	if(xor(length(var_id_name) , length(num_val_name)))
	{
		stop("Unable to identify whether table is deep or wide")
	}
	else if(length(var_id_name) && length(num_val_name))
	{
		new("FLTable",
			 odbc_connection = connection,
			 db_name = database, 
			 table_name = table,
			 primary_key = primary_key,
			 var_id_name = var_id_name,
			 num_val_name=num_val_name,
			 isDeep = TRUE)
	}
	else
	{
		new("FLTable", 
			 odbc_connection = connection,
			 db_name = database, 
			table_name = table,
			primary_key = primary_key,
			var_id_name = var_id_name,
			num_val_name=num_val_name,
			isDeep = FALSE)
	}
}

#' @describeIn names 
#' Gives the column names of FLTable object
names.FLTable <- function(object)
{
	connection = object@odbc_connection
	column_database = "dbc"

	if(!object@isDeep)
	{
		sqlstr <- paste0("SELECT columnname 
						  FROM dbc.columns
						  WHERE tablename='",object@table_name,"' 
						  AND databasename='",object@db_name,"';")
		retobj <- sqlQuery(connection,sqlstr)
		retobj <- trim(as.vector(retobj$ColumnName))
		retobj
	}
	else
	{
		sqlstr <- paste0("SELECT DISTINCT(",object@var_id_name,") as VarID 
						  FROM ",getRemoteTableName(object@db_name,
                                                    object@table_name))
		retobj <- sqlQuery(connection,sqlstr)
		retobj <- retobj$VarID
		retobj
	}
}
