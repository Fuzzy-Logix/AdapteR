#
# Fuzzy Logix Table Object
# @param  {RODBC}     ODBCConnection	[description]
# @param  {character} DBName    		  [description]
# @param  {character} TableName 		  [description]
#
setOldClass("RODBC") 
setClass("FLTable", 
		slots = list(	ODBCConnection = "RODBC", 
						DBName        = "character", 
						TableName     = "character"))

FLTable <- function(Connection,DBName,TableName) {

	#if (!is.character(DSN)) 		
	#stop("DSN must be a string")
	if (!is.character(DBName)) 		
	stop("DBName must be a string")
	if (!is.character(TableName))	
	stop("TableNamemust be a string")

	sqlQuery(Connection, "SET ROLE ALL");
	sqlQuery(Connection, paste("DATABASE", DBName));

	new("FLTable", ODBCConnection = Connection,DBName = DBName, TableName = TableName)
}
