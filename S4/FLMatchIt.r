FLMatchIt <- function( 	x,
						ObsIDColName,
						TreatmentColName,
						PropScoreColName,
						MatchOrderColName) {
		
	DBConnection         <- x@ODBCConnection;
	
	SQLStr        <- "CALL FLMatchIt('";
	SQLParameters <- paste(	x@TableName,
							ObsIDColName,  
							TreatmentColName, 
							PropScoreColName,
							MatchOrderColName,
							toString(1), sep="','")
	SQLStr        <- paste(SQLStr, SQLParameters,"'",",OutTable",")", sep="")
	#print(SQLStr)
		
	# run FLMatchIt
	Res  <- sqlQuery(DBConnection, SQLStr);
	VolatileTableName <- toString(Res[[1,"OutTable"]]);
	
	# Create permanent OutTable
	OutTableName <- GenOutTableName(VolatileTableName);
	SQLStr <- paste("CREATE TABLE ", OutTableName, " AS (SELECT ", ObsIDColName, " FROM ", VolatileTableName, ") WITH DATA", sep = "");
	#print(SQLStr)
	sqlQuery(DBConnection, SQLStr);
	
	RetData = new("FLMatchIt",	ODBCConnection = DBConnection, OutTableName = OutTableName);				
	return(RetData);
}