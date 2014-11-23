FLMatchIt <- function( 	table,
						obs_id,
						treatment,
						prop_score,
						match_order) 
						
{		
	connection      <- table@odbc_connection;
	sql        		<- "CALL FLMatchIt('";
	sqlParameters 	<- paste(	table@table_name,
								obs_id,  
								treatment, 
								prop_score,
								match_order,
								toString(1), sep="','")
	sql        		<- paste(sql, sqlParameters,"'",",OutTable",")", sep="")
	#print(sql)
		
	# run FLMatchIt
	res  			<- sqlQuery(connection, sql);
	volatileTable 	<- toString(res[[1,"OutTable"]]);
	
	# Create permanent OutTable
	outTable 		<- gen_out_table_name(volatileTable);
	sql 			<- paste("CREATE TABLE ", outTable, " AS (SELECT ", obs_id, " FROM ", volatileTable, ") WITH DATA", sep = "");
	#print(sql)
	sqlQuery(connection, sql);
	
	retData = new("FLMatchIt",	odbc_connection = connection, out_table_name = outTable);				
	return(retData);
}