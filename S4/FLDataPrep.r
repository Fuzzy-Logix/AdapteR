FLWideToDeep <- function( x,
						ObsIDColName = "ObsID",
						VarIDColName = "VarID",
						ValueColName = "Num_Val",
						PrimaryKey   = FLPrimaryKey(x),
						Exclude      = c(),
						ClassSpec    = list(),
						WhereClause  = ""){
	
	
	DeepTableName   <- GenDeepTableName(x@TableName)
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
	#sqlQuery(x@ODBCConnection,paste("DROP TABLE",DeepTableName));
	res <- sqlQuery(x@ODBCConnection, sql, stringsAsFactors = FALSE);
	#print(res);
	DeepTableName
}

FLRegrDataPrep <- function( x,
							DepCol,
							ObsIDColName = "ObsID",
							VarIDColName = "VarID",
							ValueColName = "Num_Val",
							PrimaryKey   = FLPrimaryKey(x),
							Exclude      = c(),
							ClassSpec    = list(),
							PerformNorm = 0,
							PerformVarReduc = 0,
							MakeDataSparse = 1,
							MinStdDev = 0,
							MaxCorrel = 0,
							WhereClause  = ""){
		
	DeepTableName   <- GenDeepTableName(x@TableName)
	ExcludeString   <- list.to.excludeClause(Exclude)
	ClassSpecString <- list.to.classSpec(ClassSpec)
	CatToDummy <- CalcCatToDummy(ClassSpec)
	path            <- "SQL//FLRegrDataPrep.sql";
	stopifnot(file.exists(path));
	sql <- readChar(path, nchar = file.info(path)$size);
	sql <- sprintf(	sql, 
					x@TableName,
					PrimaryKey, 
					DepCol,
					DeepTableName,
					ObsIDColName,
					VarIDColName,
					ValueColName,
					CatToDummy,
					PerformNorm,
					PerformVarReduc,
					MakeDataSparse,
					MinStdDev,
					MaxCorrel,
					toString(0),
					ExcludeString,
					ClassSpecString,
					WhereClause);
	sql <- gsub("[\r\n]", "", sql);
	#print(sql);
	#sqlQuery(x@ODBCConnection,paste("DROP TABLE",DeepTableName));
	res <- sqlQuery(x@ODBCConnection, sql, stringsAsFactors = FALSE);
	AnalysisID <- as.character(res[1,"OutAnalysisID"]);
	list( DeepTableName = DeepTableName, WidetoDeepAnalysisID = AnalysisID)
}