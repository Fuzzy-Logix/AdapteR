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
						MatrixIDColName = "character",
						RowIDColName = "character",
						ColIDColName = "character",
						CellValColName = "character"))
						
setClass("FLLUDecomp",
		slots = list(	L_Matrix = "FLMatrix",
						U_Matrix = "FLMatrix",
						Perm_Matrix = "FLMatrix"))

FLMatrix <- function(Connection, DBName, MatrixTableName, MatrixID, MatrixIDColName = "Matrix_ID", RowIDColName = "Row_ID", ColIDColName = "Col_ID", CellValColName = "Cell_Val") {

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

	new("FLMatrix", ODBCConnection = Connection, DBName = DBName, MatrixTableName = MatrixTableName, MatrixID = MatrixID, MatrixIDColName = MatrixIDColName, RowIDColName = RowIDColName, ColIDColName = ColIDColName, CellValColName = CellValColName)
}
# fetch.matrix method for FLMatrix Object
setGeneric("fetch.matrix", function(object) {
  standardGeneric("fetch.matrix")
})

# fetch_matrix method
setMethod("fetch.matrix",
          signature("FLMatrix"),
          function(object) {
      		DBConnection <- object@ODBCConnection;            
      		SQLStr           <- paste("SELECT ",object@RowIDColName,", ",object@ColIDColName,", ",object@CellValColName, " FROM ", object@MatrixTableName, " WHERE ", object@MatrixIDColName, " = ",object@MatrixID," ORDER BY 1,2",sep = "");
			#print(SQLStr)
			OutputMatrix     <- sqlQuery(DBConnection, SQLStr);
				
			return(OutputMatrix)
          }
)
