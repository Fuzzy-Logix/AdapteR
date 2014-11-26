FLLUDecomp <- function(matrix)  
{

		outTable 		<- GenOutMatrixTable("LUDecomp",matrixTable, matrixIDValue)
		connection <- matrix@ODBC_connection
		sqlParameters <- list(	matrixIDValue = toString(matrix@matrix_id_value),
								rowID         = matrix@row_id,
								columnID      = matrix@column_id,
								cellValue     = matrix@cellValue,
								matrixTable   = matrix@matrix_table,
								outTable      = outTable )
		run_sql(connection, "SQL//FLLUDecomp.sql", sqlParameters)
		
		lMatrix    	<- FLMatrix(connection,
								database        = matrix@database, 
								matrix_table    = outTable,
								matrix_id_value = matrixIDValue, 
								matrix_id       = "OutputMatrixID",
								row_id          = "OutputRowNum", 
								column_id       = "OutputColNum", 
								cell_value      = "OutputValL")

		uMatrix    	<- FLMatrix(connection, 
								database        = matrix@database, 
								matrix_table    = outTable, 
								matrix_id_value = matrixIDValue, 
								matrix_id       = "OutputMatrixID", 
								row_id          = "OutputRowNum", 
								column_id       = "OutputColNum", 
								cell_value      = "OutputValU")
		
		permMatrix 	<- FLMatrix(connection, 
								database        = matrix@database, 
								matrix_table    = outTable, 
								matrix_id_value = matrixIDValue, 
								matrix_id       = "OutputMatrixID", 
								row_id          = "OutputRowNum", 
								column_id       = "OutputColNum", 
								cell_value      = "OutputPermut")

		retData = new("FLLUDecomp", l_matrix = lMatrix, u_matrix = uMatrix, perm_matrix = permMatrix)
		return(retData)
}
		
								

	