#' @include utilities.R
NULL
setOldClass("RODBC")
#' An S4 class to represent FLTable
#'
#' @slot odbc_connection ODBC connectivity for R
#' @slot db_name A character
#' @slot table_name A character
#' @slot primary_key A character
#' @method names FLTable
#' @param object retrieves the column names of FLTable object
setClass(
	"FLTable",
	slots = list(
		odbc_connection = "RODBC",
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
#' This object is used as input for data mining functions.
#' @param connection ODBC connection handle as returned by \code{\link[RODBC]{odbcConnect}}
#' @param database name of the database
#' @param table name of the table
#' @param primary_key column name set as primary key
#' @return \code{FLTable} returns an object of class FLTable mapped to a table
#' in Teradata.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' widetable  <- FLTable(connection, "FL_TRAIN", "tblAbaloneWide", "ObsID")
#' names(widetable)
#' @export
FLTable <- function(connection, database, table,primary_key,var_id_name = character(0), num_val_name=character(0)) {

	# validate_args( 	list(database = database, table = table),
	# 				list(database = "character", table = "character")
	# )

	if(xor(length(var_id_name) , length(num_val_name))){
		stop("Unable to identify whether table is deep or wide")
	}
	else if(length(var_id_name) && length(num_val_name)){
		sqlQuery(connection, paste("DATABASE", database))
	sqlQuery(connection, "SET ROLE ALL")

	new("FLTable", odbc_connection = connection,db_name = database, table_name = table,primary_key = primary_key,var_id_name = var_id_name,num_val_name=num_val_name,isDeep = TRUE)
	}
	else{

	sqlQuery(connection, paste("DATABASE", database))
	sqlQuery(connection, "SET ROLE ALL")

	new("FLTable", odbc_connection = connection,db_name = database, table_name = table,primary_key = primary_key,var_id_name = var_id_name,num_val_name=num_val_name,isDeep = FALSE)
	}
}

names.FLTable <- function(object){
		connection = object@odbc_connection
		column_database = "dbc"

		if(!object@isDeep){
			sqlQuery(connection, paste("DATABASE", column_database))
			sqlQuery(connection, "SET ROLE ALL")
			sqlstr <- paste0("SELECT columnname FROM dbc.columns WHERE tablename='",object@table_name,"' AND databasename='",object@db_name,"';")
			retobj <- sqlQuery(connection,sqlstr)
			retobj <- trim(as.vector(retobj$ColumnName))
			retobj
		}
		else{
			sqlQuery(connection, paste("DATABASE", object@db_name))
			sqlQuery(connection, "SET ROLE ALL")
			sqlstr <- paste0("SELECT DISTINCT(",object@var_id_name,") as VarID FROM ",object@table_name)
			retobj <- sqlQuery(connection,sqlstr)
			retobj <- retobj$VarID
			retobj

		}
}


is.FLTable <- function(object)
{
	ifelse(class(object)=="FLTable",TRUE,FALSE)
}
