#
# Fuzzy Logix Matrix Object
# @param  {RODBC}     Connection		  [description]
# @param  {character} DBName    		  [description]
# @param  {character} MatrixTableName	  [description]
# @param  {numeric} MatrixID 		  [description]
setOldClass("RODBC") 
setClass("FLMatrix", 
		slots = list(	ODBCConnection = "RODBC", 
						DBName        = "character",
						MatrixTableName = "character",
						MatrixID     = "numeric",
						RowIDColName = "character",
						ColIDColName = "character",
						CellValColName = "character"))

FLMatrix <- function(Connection, DBName, MatrixTableName, MatrixID, RowIDColName = "Row_ID", ColIDColName = "Col_ID", CellValColName = "Cell_Val") {

	#if (!is.character(DSN)) 		
	#stop("DSN must be a string")
	if (!is.character(DBName)) 		
	stop("DBName must be a string")
	if (!is.character(MatrixTableName))	
	stop("MatrixTableName must be a string")
	if (!is.numeric(MatrixID) || MatrixID <= 0)	
	stop("MatrixID must be a positive integer")

	sqlQuery(Connection, paste("DATABASE", DBName));
	sqlQuery(Connection, "SET ROLE ALL");

	new("FLMatrix", ODBCConnection = Connection, DBName = DBName, MatrixTableName = MatrixTableName, MatrixID = MatrixID, RowIDColName = RowIDColName, ColIDColName = ColIDColName, CellValColName = CellValColName)
}
