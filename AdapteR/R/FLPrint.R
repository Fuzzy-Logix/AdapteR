# Print the objects

# Prints FLMatrix object
print.FLMatrix <- function(object)
{
	sqlQuery(object@odbc_connection, paste0("DATABASE", object@db_name,"; SET ROLE ALL;"))
	nrows <- object@nrow
	valuedf <- sqlQuery(object@odbc_connection, paste0("SELECT * FROM ",object@matrix_table," WHERE ",object@matrix_id_colname,"=",
		object@matrix_id_value," ORDER BY 1,2,3"))
	print(matrix(valuedf[,object@cell_val_colname],nrows,length(valuedf[,object@cell_val_colname])/nrows,byrow=T,dimnames = object@dimnames))
}

# Prints FLSparseMatrix object
print.FLSparseMatrix <- function(object)
{
	sqlQuery(object@odbc_connection, paste0("DATABASE", object@db_name,"; SET ROLE ALL;"))
	nrow <- object@nrow
	valuedf <- sqlQuery(object@odbc_connection, paste0("SELECT * FROM ",object@matrix_table," WHERE ",object@matrix_id_colname,"=",
						object@matrix_id_value," ORDER BY 1,2,3"))
	sparseMatrix(i=valuedf[,object@row_id_colname],j=valuedf[,object@col_id_colname],x=valuedf[,object@cell_val_colname],dimnames = object@dimnames)
}



# Prints FLVector object
print.FLVector <- function(object)
{
	sqlQuery(object@table@odbc_connection, paste0("DATABASE", object@table@db_name,"; SET ROLE ALL;"))
	
	if(object@table@isDeep && length(object@vector_id_value))
	{
		valuedf <- sqlQuery(object@table@odbc_connection, paste0("SELECT * FROM ",object@table@table_name," WHERE ",object@table@primary_key,"=",
		object@vector_id_value," ORDER BY ",object@table@var_id_name))
		print(as.vector(valuedf[,object@col_name]))
	}
	else if(!object@table@isDeep)
	{
		valuedf <- sqlQuery(object@table@odbc_connection, paste0("SELECT ",object@table@primary_key,",",object@col_name," FROM ",
							object@table@table_name," ORDER BY ",object@table@primary_key))
		print(as.vector(valuedf[,object@col_name]))
	}
}

