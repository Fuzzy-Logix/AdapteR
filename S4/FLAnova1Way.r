FLMakeAnova <- function(AnovaRes,CategoryColName)
{
	DBLytixRows  <- c("between","within")
	RRows        <- c(CategoryColName,"Residuals")
	
	DBLytixCols  <- c("df","ss","ms")
	RCols        <- c("Df","Sum Sq","Mean Sq")
	RFStat       <- c("F value","Pr(>F)")
	RCols        <- c(RCols,RFStat)
	DBLytixFStat <- c("f_stat","p_value")

	RetValue     <- data.frame()

	for (i in 1:2 ) {
		temp <- list()
		for (j in 1:3) {
			#str(AnovaRes)
			name <- paste(DBLytixCols[j],DBLytixRows[i],sep="_")
			temp <- c(temp,AnovaRes[1,name])
		}
		temp <- c(temp,ifelse(i < 2,AnovaRes[1,"f_stat"],NA),ifelse(i < 2,AnovaRes[1,"p_value"],NA))
		RetValue <- rbind(RetValue,temp)
	}
	colnames(RetValue) <- RCols
	rownames(RetValue) <- RRows
	RetValue
}
FLAnova1Way <- function(	Tbl,
							ValueColName,
							CategoryColName,
							WhereClause = "")
	{
		TableName   <- Tbl@TableName;
		WhereClause <- ifelse(nchar(WhereClause) > 1, WhereClause, "1=1");
		
		path        <- "SQL//FLAnova1WayUdt.sql";
		stopifnot(file.exists(path));
		sql         <- readChar(path, nchar = file.info(path)$size);
		sql         <- sprintf(	sql, 
								CategoryColName,
								ValueColName,   
								CategoryColName,   
								ValueColName,
								TableName,
								WhereClause,
								CategoryColName,
								ValueColName);
		sql        <- gsub("[\r\n\t]", " ", sql);
		#print(sql)
		Connection <- Tbl@ODBCConnection
		AnovaRes   <- sqlQuery(Connection, sql, stringsAsFactors = FALSE);
		return( FLMakeAnova(AnovaRes,CategoryColName) );
}
		
								

	