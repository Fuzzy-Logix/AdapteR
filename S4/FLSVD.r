FLSVD <- function(FLMatrix)  {

		Matrix_ID   <- FLMatrix@MatrixID;
		Row_ID      <- FLMatrix@RowIDColName;
		Col_ID      <- FLMatrix@ColIDColName;
		Cell_Val    <- FLMatrix@CellValColName;
		MatrixTable <- FLMatrix@MatrixTableName;
		OutTable 	<- GenOutMatrixTable("FLSVD",MatrixTable, Matrix_ID)
		
		path        <- "SQL//FLSVDUdt.sql";
		stopifnot(file.exists(path));
		sql 		<- readChar(path, nchar = file.info(path)$size);
		sql 		<- sprintf(	sql, 
								OutTable,
								Row_ID,   
								Col_ID,   
								Cell_Val,   
								MatrixTable,
								toString(Matrix_ID));
		sql <- gsub("[\r\n\t]", " ", sql);
		#print(sql)
		Connection  <- FLMatrix@ODBCConnection
		sqlQuery(Connection, sql, stringsAsFactors = FALSE);
		U_Matrix <- FLMatrix(Connection, DBName = FLMatrix@DBName, MatrixTableName = OutTable, MatrixID = Matrix_ID, MatrixIDColName = "OutputMatrixID", RowIDColName = "OutputRowNum", ColIDColName = "OutputColNum", CellValColName = "OutputUVal");
		S_Matrix <- FLMatrix(Connection, DBName = FLMatrix@DBName, MatrixTableName = OutTable, MatrixID = Matrix_ID, MatrixIDColName = "OutputMatrixID", RowIDColName = "OutputRowNum", ColIDColName = "OutputColNum", CellValColName = "OutputSVal");
		V_Matrix <- FLMatrix(Connection, DBName = FLMatrix@DBName, MatrixTableName = OutTable, MatrixID = Matrix_ID, MatrixIDColName = "OutputMatrixID", RowIDColName = "OutputRowNum", ColIDColName = "OutputColNum", CellValColName = "OutputVVal");
		RetData = new("FLSVD", U_Matrix = U_Matrix, S_Matrix = S_Matrix, V_Matrix = Perm_Matrix)
		return(RetData);
}