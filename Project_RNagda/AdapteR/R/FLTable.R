setOldClass("RODBC")

setClass(
	"FLTable", 
	slots = list(	
		odbc_connection = "RODBC", 
		db_name         = "character", 
		table_name      = "character",
		primary_key="character"
	)
)

FLTable <- function(connection, database, table,primary_key=character(0)) {

	# validate_args( 	list(database = database, table = table), 
	# 				list(database = "character", table = "character")
	# )
	sqlQuery(connection, paste("DATABASE", database))
	sqlQuery(connection, "SET ROLE ALL")

	new("FLTable", odbc_connection = connection,db_name = database, table_name = table,primary_key = primary_key)
}

names.FLTable <- function(object){
		connection = object@odbc_connection
		column_database = "dbc"
		sqlQuery(connection, paste("DATABASE", column_database))
		sqlQuery(connection, "SET ROLE ALL")
		sqlstr = paste0("SELECT columnname FROM dbc.columns WHERE tablename='",object@table_name,"' AND databasename='",object@db_name,"';")
		retobj = sqlQuery(connection,sqlstr)
		retobj<-trim(as.vector(retobj$ColumnName))
}