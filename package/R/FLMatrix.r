#
# Fuzzy Logix Matrix Object
# @param  {RODBC}	Connection		  [description]
# @param  {character} database  [description]
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

FLMatrix <- function(connection, database, matrix_table, matrix_id_value,
					 matrix_id = "Matrix_ID", row_id = "Row_ID", column_id = "Col_ID",
					 cell_value = "Cell_Val") 
{

	#Type validation
	matrix_id_value <- ifelse(	is_number(matrix_id_value),
						as.integer(matrix_id_value),
						stop("matrix_id_value should be an integer"))

	argList  <- as.list(environment())
	typeList <- list(	connection      = "integer",
						database        = "character",
						matrix_table    = "character",
						matrix_id_value = "integer",										
						matrix_id       = "character",
						row_id          = "character",
						column_id       = "character",
						cell_value      = "character" )
	validate_args(argList, typeList)

	sqlQuery(connection, paste("DATABASE", database));
	sqlQuery(connection, "SET ROLE ALL");

	new("FLMatrix", ODBC_connection = connection, database = database, matrix_table = matrix_table, matrix_id_value = matrix_id_value, matrix_id = matrix_id, row_id = row_id, column_id = column_id, cell_value = cell_value)
}
	
# FLFetchMatrix method for FLMatrix Object
setGeneric("FLFetchMatrix1", function(object) 
{
  standardGeneric("FLFetchMatrix1")
})

# FLFetchMatrix Method Definition
setMethod("FLFetchMatrix1",
          signature("FLMatrix"),
          function(object) 
		  {
      		connection 	 <- object@ODBC_connection       
      		sqlParameters <- list(	row_id          = object@row_id,
									column_id       = object@column_id,
									cell_value      = object@cell_value,
									matrix_table    = object@matrix_table,
									matrix_id       = object@matrix_id,
									matrix_id_value = object@matrix_id_value)
			outputMatrix  <- run_sql(connection, "SQL//FLFetchMatrix.sql", sqlParameters)
				
			return(outputMatrix)
          }
)

FLFetchMatrix <- function(matrix)
{

	connection <- matrix@ODBC_connection
	sqlParameters <- list(	row_id          = matrix@row_id,
							column_id       = matrix@column_id,
							cell_value      = matrix@cell_value,
							matrix_table    = matrix@matrix_table,
							matrix_id       = matrix@matrix_id,
							id_value = matrix@matrix_id_value)
	outputMatrix  <- run_sql(connection, "SQL//FLFetchMatrix.sql", sqlParameters)
	
	return(outputMatrix)
	
}
