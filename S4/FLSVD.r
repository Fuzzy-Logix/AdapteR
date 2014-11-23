FLSVD <- function(matrix)  
{
	matrixIDValue   <- matrix@matrix_id_value;
	rowID      		<- matrix@row_id;
	columnID      	<- matrix@column_id;
	cellValue    	<- matrix@cell_value;
	matrixTable 	<- matrix@matrix_table;
	outTable 		<- GenOutMatrixTable("FLSVD",matrixTable, matrixIDValue)
		
	path        	<- "SQL//FLSVDUdt.sql";
	stopifnot(file.exists(path));
	sql 			<- readChar(path, nchar = file.info(path)$size);
	sql 			<- sprintf(	sql, 
								outTable,
								rowID,   
								columnID,   
								cellValue,   
								matrixTable,
								toString(matrixIDValue));
	sql 			<- gsub("[\r\n\t]", " ", sql);
	#print(sql)
	connection  	<- matrix@ODBC_connection
	sqlQuery(connection, sql, stringsAsFactors = FALSE);
	uMatrix 		<- FLMatrix(connection, database = matrix@database, matrix_table = outTable, matrix_id_value = matrixIDValue, matrix_id = "OutputMatrixID", row_id = "OutputRowNum", column_id = "OutputColNum", cell_value = "OutputUVal");
	sMatrix 		<- FLMatrix(connection, database = matrix@database, matrix_table = outTable, matrix_id_value = matrixIDValue, matrix_id = "OutputMatrixID", row_id = "OutputRowNum", column_id = "OutputColNum", cell_value = "OutputSVal");
	vMatrix 		<- FLMatrix(connection, database = matrix@database, matrix_table = outTable, matrix_id_value = matrixIDValue, matrix_id = "OutputMatrixID", row_id = "OutputRowNum", column_id = "OutputColNum", cell_value = "OutputVVal");
	retData = new("FLSVD", u_matrix = uMatrix, s_matrix = sMatrix, v_matrix = vMatrix)
	return(retData);
}