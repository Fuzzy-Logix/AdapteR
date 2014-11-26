FLMatrixInv <- function(matrix)  
{
		outTable      <- GenOutMatrixTable("MatrixInv",matrixTable, matrixIDValue)
		connection    <- matrix@ODBC_connection
		sqlParameters <- list(	matrixIDValue = toString(matrix@matrix_id_value),
								rowID         = matrix@row_id,
								columnID      = matrix@column_id,
								cellValue     = matrix@cellValue,
								matrixTable   = matrix@matrix_table,
								outTable      = outTable )
		run_sql(connection, "SQL//FLMatrixInv.sql", sqlParameters)
		
		retData = new(	"FLMatrix", ODBC_connection = connection, database = matrix@database, 
						matrix_table = outTable, matrix_id_value = matrixIDValue, 
						matrix_id = "OutputMatrixID", row_id = "OutputRowNum", column_id = "OutputColNum", cell_value = "OutputVal")
		return(retData)
}
		
								

	