FLLUDecomp <- function(matrix)  
{

		matrixIDValue   <- matrix@matrix_id_value;
		rowID      		<- matrix@row_id;
		columnID   		<- matrix@column_id;
		cellValue    	<- matrix@cell_value;
		matrixTable 	<- matrix@matrix_table;
		outTable 		<- GenOutMatrixTable("LUDecomp",matrixTable, matrixIDValue)
		
		path        	<- "SQL//FLLUDecomp.sql";
		stopifnot(file.exists(path));
		sql 			<- readChar(path, nchar = file.info(path)$size);
		sql 			<- sprintf(	sql, 
								outTable,
								rowID,   
								columnID,   
								cellValue,   
								matrixTable,
								toString(matrixIDValue));
		sql 			<- gsub("[\r\n]", "", sql);
		#print(sql)
		connection  	<- matrix@ODBC_connection
		sqlQuery(connection, sql, stringsAsFactors = FALSE);
		lMatrix    	<- FLMatrix(connection, database = matrix@database, matrix_table = outTable, matrix_id_value = matrixIDValue, matrix_id = "OutputMatrixID", row_id = "OutputRowNum", column_id = "OutputColNum", cell_value = "OutputValL");
		uMatrix    	<- FLMatrix(connection, database = matrix@database, matrix_table = outTable, matrix_id_value = matrixIDValue, matrix_id = "OutputMatrixID", row_id = "OutputRowNum", column_id = "OutputColNum", cell_value = "OutputValU");
		permMatrix 	<- FLMatrix(connection, database = matrix@database, matrix_table = outTable, matrix_id_value = matrixIDValue, matrix_id = "OutputMatrixID", row_id = "OutputRowNum", column_id = "OutputColNum", cell_value = "OutputPermut");
		retData = new("FLLUDecomp", l_matrix = lMatrix, u_matrix = uMatrix, perm_matrix = permMatrix)
		return(retData);
}
		
								

	