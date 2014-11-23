#
# Fuzzy Logix Matrix Object
# @param  {RODBC}	Connection		  [description]
# @param  {character} database    		  [description]
# @param  {character} matrix_table	  [description]
# @param  {numeric} matrix_id_value		  [description]
setOldClass("RODBC") 
setClass("FLMatrix", 
		slots = list(	ODBC_connection = "RODBC", 
						database        = "character",
						matrix_table    = "character",
						matrix_id_value = "numeric",
						matrix_id       = "character",
						row_id          = "character",
						column_id       = "character",
						cell_value      = "character"))
						
setClass("FLLUDecomp",
		slots = list(	l_matrix = "FLMatrix",
						u_matrix = "FLMatrix",
						perm_matrix = "FLMatrix"))

setClass("FLSVD",
		slots = list(	u_matrix = "FLMatrix",
						s_matrix = "FLMatrix",
						v_matrix = "FLMatrix"))

FLMatrix <- function(connection, database, matrix_table, matrix_id_value, matrix_id = "Matrix_ID", row_id = "Row_ID", column_id = "Col_ID", cell_value = "Cell_Val") 
{

	if (!is.character(database)) 		
	stop("Database must be a string")
	if (!is.character(matrix_table))	
	stop("Matrix_Table must be a string")
	if (!is.numeric(matrix_id_value) || matrix_id_value <= 0)	
	stop("Matrix_ID_Value must be a positive integer")

	sqlQuery(connection, paste("DATABASE", database));
	sqlQuery(connection, "SET ROLE ALL");

	new("FLMatrix", ODBC_connection = connection, database = database, matrix_table = matrix_table, matrix_id_value = matrix_id_value, matrix_id = matrix_id, row_id = row_id, column_id = column_id, cell_value = cell_value)
}
	
# FLFetchMatrix method for FLMatrix Object
setGeneric("FLFetchMatrix", function(object) 
{
  standardGeneric("FLFetchMatrix")
})

# FLFetchMatrix Method Definition
setMethod("FLFetchMatrix",
          signature("FLMatrix"),
          function(object) 
		  {
      		DBConnection 	 <- object@ODBC_connection;            
      		SQLStr           <- paste("SELECT ",object@row_id,", ",object@column_id,", ",object@cell_value, " FROM ", object@matrix_table, " WHERE ", object@matrix_id, " = ",object@matrix_id_value," ORDER BY 1,2",sep = "");
			#print(SQLStr)
			outputMatrix     <- sqlQuery(DBConnection, SQLStr);
				
			return(outputMatrix)
          }
)
