#
# Fuzzy Logix Table Object
# @param  {RODBC}     odbc_connection		  [description]
# @param  {character} database    		  [description]
# @param  {character} table_name 		  [description]
#
setOldClass("RODBC") 
setClass("FLTable", 
		slots = list(	odbc_connection = "RODBC", 
						db_name         = "character", 
						table_name      = "character"))

FLTable <- function(connection, database, table) {

	validate_args( 	list(database = database, table = table),
					list(database = "character", table = "character"))

	sqlQuery(odbc_connection, paste("DATABASE", database));
	sqlQuery(odbc_connection, "SET ROLE ALL");

	new("FLTable", odbc_connection = connection,db_name = database, table_name = table)
}
