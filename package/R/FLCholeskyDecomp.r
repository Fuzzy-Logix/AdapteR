FLCholeskyDecomp <- function(matrix)  
{	
	connection    <- matrix@ODBC_connection
	outTable      <- gen_out_matrix_table("CholeskyDecomp",matrixTable, matrixIDValue)
	
	sqlParameters <- list(	matrixIDValue = toString(matrix@matrix_id_value),
							rowID         = matrix@row_id,
							columnID      = matrix@column_id,
							cellValue     = matrix@cellValue,
							matrixTable   = matrix@matrix_table,
							outTable      = outTable)
	runsql(connection, "SQL//FLCholeskyDecomp.sql", sqlParameters)

	retData = new("FLMatrix", 	ODBC_connection = connection, database = matrix@database,
								matrix_table = outTable, matrix_id_value = matrix@matrix_id_value,
								matrix_id = "OutputMatrixID", row_id = "OutputRowNum",
								column_id = "OutputColNum", cell_value = "OutputVal")
	return(retData)
}
		
								

	