FLMakeAncova <- function(ancovaRes,variable)
{
	DBLytixRows  <- c("between","within")
	RRows        <- c(variable,"Residuals")
	
	DBLytixCols  <- c("df","ss","ms")
	RCols        <- c("Df","Sum Sq","Mean Sq")
	RFStat       <- c("F value","Pr(>F)")
	RCols        <- c(RCols,RFStat)
	DBLytixFStat <- c("f_stat","p_value")

	RetValue     <- data.frame()

	for (i in 1:2 ) {
		temp <- list()
		for (j in 1:3) {
			#str(ancovaRes)
			name <- paste(DBLytixCols[j],DBLytixRows[i],sep="_")
			temp <- c(temp,ancovaRes[1,name])
		}
		temp <- c(temp,ifelse(i < 2,ancovaRes[1,"f_stat"],NA),ifelse(i < 2,ancovaRes[1,"p_value"],NA))
		RetValue <- rbind(RetValue,temp)
	}
	colnames(RetValue) <- RCols
	rownames(RetValue) <- RRows
	RetValue
}
FLAncova <- function(	table,
						response,
						variable,
						control,						
						where_clause = "")
	{
		tableName   <- table@table_name
		whereClause <- ifelse(nchar(where_clause) > 1, where_clause, "1=1")
		
		sqlParameters <- list(	tableName   = tableName,
								variable    = variable,
								control     = control,
								response    = response,							
								whereClause = whereClause)
		connection <- table@odbc_connection
		path       <- "SQL//FLAncovaUdt.sql"
		ancovaRes  <- run_sql(connection, path, sqlParameters)
		return( FLMakeAncova(ancovaRes,variable) )
}