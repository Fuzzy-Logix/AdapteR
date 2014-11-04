FLLUDecomp <- function(FLMatrix)  {

		Matrix_ID   <- FLMatrix@MatrixID;
		Row_ID      <- FLMatrix@RowIDColName;
		Col_ID      <- FLMatrix@ColIDColName;
		Cell_Val    <- FLMatrix@CellValColName;
		MatrixTable <- FLMatrix@MatrixTableName;
		OutTable 	<- GenOutMatrixTable("LUDecomp",MatrixTable, Matrix_ID)
		
		path        <- "SQL//FLLUDecomp.sql";
		stopifnot(file.exists(path));
		sql 		<- readChar(path, nchar = file.info(path)$size);
		sql 		<- sprintf(	sql, 
								OutTable,
								Row_ID,   
								Col_ID,   
								Cell_Val,   
								MatrixTable,
								toString(Matrix_ID));
		sql <- gsub("[\r\n]", "", sql);
		#print(sql)
		Connection  <- FLMatrix@ODBCConnection
		sqlQuery(Connection, sql, stringsAsFactors = FALSE);
		L_Matrix = FLMatrix(Connection, DBName = FLMatrix@DBName, MatrixTableName = OutTable, MatrixID = Matrix_ID, MatrixIDColName = "OutputMatrixID", RowIDColName = "OutputRowNum", ColIDColName = "OutputColNum", CellValColName = "OutputValL");
		U_Matrix = FLMatrix(Connection, DBName = FLMatrix@DBName, MatrixTableName = OutTable, MatrixID = Matrix_ID, MatrixIDColName = "OutputMatrixID", RowIDColName = "OutputRowNum", ColIDColName = "OutputColNum", CellValColName = "OutputValU");
		Perm_Matrix = FLMatrix(Connection, DBName = FLMatrix@DBName, MatrixTableName = OutTable, MatrixID = Matrix_ID, MatrixIDColName = "OutputMatrixID", RowIDColName = "OutputRowNum", ColIDColName = "OutputColNum", CellValColName = "OutputPermut");
		RetData = new("FLLUDecomp", L_Matrix = L_Matrix, U_Matrix = U_Matrix, Perm_Matrix = Perm_Matrix)
		return(RetData);
}
		
								

	