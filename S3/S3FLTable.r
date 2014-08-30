trim.trailing <- function (x) sub("\\s+$", "", x)

list.to.string <- function (x) {
  xString <- paste(names(x),x,sep="(",collapse="), ")
  xString <- paste(xString,")",collapse="",sep="")
  xString <- ifelse(nchar(xString) > 1, xString, "");
  xString
}


select.ten <- function(x){
	SQLStr <- paste("SELECT TOP 10 * FROM ", x[["TableName"]], sep='');
	Res <- sqlQuery(x[["connection"]], SQLStr);
	Res
}

FLTable <- function(DSN,DBName,TableName) {

  if (!is.character(DSN)) 		
	stop("DSN must be a string")
  if (!is.character(DBName)) 		
	stop("DBName must be a string")
  if (!is.character(TableName))	
	stop("TableNamemust be a string")

  DBConnection <- odbcConnect(DSN);
  sqlQuery(DBConnection, "SET ROLE ALL");
  sqlQuery(DBConnection, paste("DATABASE ", DBName, sep=""));

  structure(list( connection=DBConnection, 
                  DBName=DBName, 
                  TableName=TableName, 
                  PrimaryKey=c(""), 
                  colnames=c(""), 
                  DeepTableName="", 
                  WideToDeepAnalysisID="" ), class = "FLTable")
}

FLcolnames <- function(x){
  returnval <- x[["colnames"]];
  SQLStr <- paste("SELECT TOP 1 * FROM ", x[["TableName"]], sep='');
  if(x[["colnames"]]==c(""))
    returnval <- colnames(sqlQuery(x[["connection"]], SQLStr))
  returnval
}

FLPrimaryKey <- function(x){
  path = "../SQL/PrimaryKey.sql"
  stopifnot(file.exists(path));
  SQLStr <- readChar(path, nchar = file.info(path)$size);
  SQLStr <- sprintf(SQLStr, x[["TableName"]], x[["DBName"]]);
  if(x[["PrimaryKey"]]==c("")) 
  {
    Res <- sqlQuery(x[["connection"]], SQLStr, stringsAsFactors = FALSE);
    x[["PrimaryKey"]] <- trim.trailing(Res[Res$IndexType == "P","ColumnName"])
  }
  x[["PrimaryKey"]]
}

FLClose <- function(x){
  close(x[["connection"]])
}

FLDataPrep <- function( x,
                        PrimaryKey=FLPrimaryKey(x),
                        Include=setdiff(FLcolnames(x),FLPrimaryKey(x)),
                        Exclude=c(),
                        ClassSpec=list(),
                        WhereClause=''){
  DeepTableName <- paste(x[["TableName"]],"Deep",sep="");
  ExcludeString <- paste(Exclude, collapse=", ")
  ClassSpecString <- list.to.string(ClassSpec)
  path <- "../SQL/WideToDeep.sql";
  stopifnot(file.exists(path));
  sql <- readChar(path, nchar = file.info(path)$size);
  sql <- sprintf(sql, x[["TableName"]], PrimaryKey, DeepTableName, ExcludeString, ClassSpecString, WhereClause);
  sql <- gsub("[\r\n]", "", sql);
  sqlQuery(x[["connection"]],paste("DROP TABLE",DeepTableName));
  res <- sqlQuery(x[["connection"]], sql, stringsAsFactors = FALSE);
  x[["PrimaryKey"]] = PrimaryKey
  x[["colnames"]] = FLcolnames(x)
  x[["DeepTableName"]] = DeepTableName
  x[["WideToDeepAnalysisID"]] = res$OutAnalysisID
  x
}