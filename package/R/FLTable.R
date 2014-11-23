#
# Fuzzy Logix Table Object
# @param  {RODBC}     odbc_connection		  [description]
# @param  {character} db_name    		  [description]
# @param  {character} table_name 		  [description]
#
setOldClass("RODBC") 
setClass("FLTable", 
		slots = list(	odbc_connection = "RODBC", 
						db_name        = "character", 
						table_name     = "character"))

FLTable <- function(odbc_connection,db_name,table_name) {

	validate_args(list(db_name = db_name, table_name = table_name),list(db_name = "character", table_name = "character"))

	sqlQuery(odbc_connection, paste("DATABASE", DBName));
	sqlQuery(odbc_connection, "SET ROLE ALL");

	new("FLTable", odbc_connection = odbc_connection,db_name = db_name, table_name = table_name)
}
