FLWideToDeep <- function( x,
						ObsIDColName = "ObsID",
						VarIDColName = "VarID",
						ValueColName = "Num_Val",
						PrimaryKey   = FLPrimaryKey(x),
						Exclude      = c(),
						ClassSpec    = list(),
						WhereClause  = ""){
	
	random_no <- rnorm(1);
	DeepTableName   <- paste(x@TableName,"Deep",round(as.numeric(Sys.time())),round(random_no*random_no*10000000),sep="_");
	ExcludeString   <- list.to.excludeClause(Exclude)
	ClassSpecString <- list.to.classSpec(ClassSpec)
	path            <- "SQL//WideToDeep.sql";
	stopifnot(file.exists(path));
	sql <- readChar(path, nchar = file.info(path)$size);
	sql <- sprintf(	sql, 
					x@TableName,
					PrimaryKey, 
					DeepTableName,
					ObsIDColName,
					VarIDColName,
					ValueColName,
					ExcludeString,
					ClassSpecString,
					WhereClause);
	sql <- gsub("[\r\n]", "", sql);
	#print(sql);
	sqlQuery(x@ODBCConnection,paste("DROP TABLE",DeepTableName));
	res <- sqlQuery(x@ODBCConnection, sql, stringsAsFactors = FALSE);
	#print(res);
	DeepTableName
}