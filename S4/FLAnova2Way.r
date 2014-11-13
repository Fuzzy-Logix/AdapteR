FLMakeAnova2Way <- function(AnovaRes,CategoryCol1Name,CategoryCol2Name)
{
	DBLytixRows  <- c(CategoryCol1Name,CategoryCol2Name,"Interaction","Error")
	RRows        <- c(CategoryCol1Name,CategoryCol2Name,paste(CategoryCol1Name,CategoryCol2Name,sep=":"),"Residuals")
	
	DBLytixCols  <- c("DF","SS","MS")
	RCols        <- c("Df","Sum Sq","Mean Sq")
	RFStat       <- c("F value","Pr(>F)")
	RCols        <- c(RCols,RFStat)
	DBLytixFStat <- "FStat"
	DBLytixPVal <- "PValue"

	RetValue     <- data.frame()
	for (i in 1:4 ) {
		temp <- list()
		for (j in 1:3) {
			
			name <- paste(DBLytixRows[i],DBLytixCols[j],sep="_")
			temp <- c(temp,AnovaRes[1,name])
		}
		
		f_stat <- paste(DBLytixRows[i],DBLytixFStat,sep="_")
		p_value <- paste(DBLytixRows[i],DBLytixPVal,sep="_")
		temp <- c(temp,ifelse(i <= 3,AnovaRes[1, f_stat],NA),ifelse(i <= 3,AnovaRes[1,p_value],NA))
		RetValue <- rbind(RetValue,temp)
	}
	colnames(RetValue) <- RCols
	rownames(RetValue) <- RRows
	RetValue
}
FLAnova2Way <- function(	Tbl,
							ValueColName,
							CategoryCol1Name,
							CategoryCol2Name,
							WhereClause = "")
	{
		TableName   <- Tbl@TableName;
		#WhereClause <- ifelse(nchar(WhereClause) > 1, WhereClause, "1=1");
		
		path        <- "SQL//FLAnova2Way.sql";
		stopifnot(file.exists(path));
		sql         <- readChar(path, nchar = file.info(path)$size);
		sql         <- sprintf(	sql, 
								TableName,
								ValueColName,   
								CategoryCol1Name,   
								CategoryCol2Name,
								WhereClause);
		sql        <- gsub("[\r\n\t]", " ", sql)
		Connection <- Tbl@ODBCConnection
		AnovaVolTable   <- sqlQuery(Connection, sql, stringsAsFactors = FALSE);
		AnovaRes <- sqlQuery(Connection, paste("SELECT * FROM",AnovaVolTable[1,1]), stringsAsFactors = FALSE);
		return( FLMakeAnova2Way(AnovaRes,CategoryCol1Name,CategoryCol2Name) );
}
		
								

	