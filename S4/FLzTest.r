FLz.Test <- function(x, InVal1, InVal2 = NULL, mu = 0, NumTails = 2, PrimaryKey = "ObsID") {
	if(length(InVal2) == 0) {
		path <- "SQL//FLzTest1S.sql";
		stopifnot(file.exists(path));
		sql  <- readChar(path, nchar = file.info(path)$size);
		sql  <- sprintf(sql, 
						toString(mu),
						InVal1, 
						toString(NumTails),
						toString(mu),
						InVal1, 
						toString(NumTails),
						x@TableName);
		sql  <- gsub("[\r\n]", "", sql);
		res <- sqlQuery(x@ODBCConnection, sql, stringsAsFactors = FALSE);
		return(res)
	}
	else {
		ObsIDColName  <- "ObsID";
		VarIDColName  <- "GroupID";
		ValueColName  <- "Num_Val";
		WhereClause   <- "";
		DeepTableName <- FLWideToDeep(x,
									ObsIDColName = ObsIDColName,
									VarIDColName = VarIDColName,
									ValueColName = ValueColName,
									PrimaryKey   = PrimaryKey,
									Exclude      = c(),
									ClassSpec    = list(),
									WhereClause  = WhereClause);
		path <- "SQL//FLzTest2S.sql";
		stopifnot(file.exists(path));
		sql  <- readChar(path, nchar = file.info(path)$size);
		sql  <- sprintf(sql, 
						toString(NumTails),
						toString(NumTails),
						DeepTableName);
		sql  <- gsub("[\r\n]", "", sql);
		res <- sqlQuery(x@ODBCConnection, sql, stringsAsFactors = FALSE);
		sqlQuery(x@ODBCConnection,paste("DROP TABLE",DeepTableName));
		return(res)
	}
}