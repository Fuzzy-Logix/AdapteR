setOldClass("RODBC") 
setClass(
	"FLMatrix", 
	slots = list(	
		odbc_connection = "RODBC", 
		db_name = "character", 
		matrix_table = "character",
		matrix_id_value	= "numeric",
		matrix_id_colname = "character",
		row_id_colname = "character",
		col_id_colname = "character",
		cell_val_colname = "character"
	)
)

FLMatrix <- function(connection, database, matrix_table, matrix_id_value, 
					matrix_id_colname = "MATRIX_ID", row_id_colname = "ROW_ID", col_id_colname = "COL_ID", cell_val_colname = "CELL_VAL") 

{
	sqlQuery(connection, paste("DATABASE", database))
	sqlQuery(connection, "SET ROLE ALL")
	
	new("FLMatrix", odbc_connection = connection, db_name = database, matrix_table = matrix_table, matrix_id_value = matrix_id_value, matrix_id_colname = matrix_id_colname, row_id_colname = row_id_colname, col_id_colname = col_id_colname, cell_val_colname = cell_val_colname)
}

#overloading det
det <- function (x, ...){
	UseMethod("det", x)
}

det.FLMatrix<-function(object){
	connection <- object@odbc_connection
	sqlstr <- paste0("WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) AS (SELECT a.",object@matrix_id_colname,", a.",object@row_id_colname,", a.",object@col_id_colname,", a.",object@cell_val_colname," FROM  ",object@matrix_table," a WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") SELECT a.* FROM TABLE (FLMatrixDetUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) HASH BY z.Matrix_ID LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a ORDER BY 1;")
	retobj <- sqlQuery(connection,sqlstr)
	result <- retobj$OutputDetVal
	result
} 


	
