FLEigen <- function(matrix)  
{	
	matrixTable   <- matrix@matrix_table,
	matrixIDValue <- toString(matrix@matrix_id_value)
	connection    <- matrix@ODBC_connection
	outTable   <- gen_out_matrix_table("FLEigenVectors",matrixTable, matrixIDValue)
	sqlParameters <- list(	matrixIDValue = matrixIDValue,
							rowID         = matrix@row_id,
							columnID      = matrix@column_id,
							cellValue     = matrix@cellValue,
							matrixTable   = matrixTable,
							outTable      = outTable )
	run_sql(connection, "SQL//FLEigenVectorUdt.sql", sqlParameters)
	retData$vectors = new(	"FLMatrix", 
							ODBC_connection = connection,
							database        = matrix@database, 
							matrix_table    = outTable, 
							matrix_id_value = matrix@matrix_id_value, 
							matrix_id       = "OutputMatrixID", 
							row_id          = "OutputRowNum", 
							column_id       = "OutputColNum", 
							cell_value      = "OutputVal")


	outTable 		<- gen_out_matrix_table("FLEigenValues",matrixTable, matrixIDValue)		
	sqlParameters$outTable <- outTable
	run_sql(connection, "SQL//FLEigenValueUdt.sql", sqlParameters)
	retData$values = new(	"FLMatrix", 
							ODBC_connection = connection,
							database        = matrix@database, 
							matrix_table    = outTable, 
							matrix_id_value = matrixIDValue, 
							matrix_id       = "OutputMatrixID", 
							row_id          = "OutputRowNum", 
							column_id       = "OutputColNum", 
							cell_value      = "OutputVal")
		
	return(retData);
}	