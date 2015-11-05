# Contains the support functions

gen_deep_table_name <- function(TableName){
	random_no <- rnorm(1);
	paste(TableName,"Deep",round(as.numeric(Sys.time())),round(random_no*random_no*10000000),sep="_");
}

trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

gen_score_table_name <- function(TableName){
	random_no <- rnorm(1);
	paste(TableName,"Score",round(as.numeric(Sys.time())),round(random_no*random_no*10000000),sep="_");
}

gen_wide_table_name <- function(TableName){
	random_no <- rnorm(1);
	paste(TableName,"Wide",round(as.numeric(Sys.time())),round(random_no*random_no*10000000),sep="_");
}

gen_unique_table_name <- function(TableName){
	random_no <- rnorm(1);
	paste(TableName,"Unique",round(as.numeric(Sys.time())),round(random_no*random_no*10000000),sep="_");
}

FLodbcClose <- function(connection)
{
    	sqlQuery(connection, 
    			 paste0("DROP TABLE ",result_db_name,".",result_matrix_table,"; 
    			 		 DROP TABLE ",result_db_name,".",result_Sparsematrix_table,";
    			 		 DROP TABLE ",result_db_name,".",result_vector_table,";"))
    	odbcClose(connection)
    	flag1 <<- 0
    	flag2 <<- 0
    	flag3 <<- 0
    	max_matrix_id_value <<- 1
    	max_Sparsematrix_id_value <<- 1
    	max_vector_id_value <<- 1
}

FLStartSession <- function(connection)
{
	sqlQuery(connection, 
	 		 paste0("DROP TABLE ",result_db_name,".",result_matrix_table,";
	 		 		 DROP TABLE ",result_db_name,".",result_Sparsematrix_table,";
	 		 		 DROP TABLE ",result_db_name,".",result_vector_table,";
	 		 		 DATABASE ", result_db_name,";
	 		 		 SET ROLE ALL;"))

	sqlQuery(connection,
			 paste0(" CREATE TABLE ",result_db_name,".",result_matrix_table,", FALLBACK ,
				     NO BEFORE JOURNAL,
				     NO AFTER JOURNAL,
				     CHECKSUM = DEFAULT,
				     DEFAULT MERGEBLOCKRATIO
				     (
				      MATRIX_ID INTEGER,
				      ROW_ID INTEGER,
				      COL_ID INTEGER,
				      CELL_VAL FLOAT)
	    			 PRIMARY INDEX ( MATRIX_ID, ROW_ID, COL_ID );"))

	sqlQuery(connection,
			 paste0(" CREATE TABLE ",result_db_name,".",result_Sparsematrix_table,", FALLBACK ,
				     NO BEFORE JOURNAL,
				     NO AFTER JOURNAL,
				     CHECKSUM = DEFAULT,
				     DEFAULT MERGEBLOCKRATIO
				     (
				      MATRIX_ID INTEGER,
				      ROW_ID INTEGER,
				      COL_ID INTEGER,
				      CELL_VAL FLOAT)
	    			 PRIMARY INDEX ( MATRIX_ID, ROW_ID, COL_ID );"))

 	sqlQuery(connection,
			 paste0("CREATE TABLE ",result_db_name,".",result_vector_table," 
			 		 ( VECTOR_ID INT, 
			 		   VECTOR_INDEX INT, 
				 	   VECTOR_VALUE VARCHAR(20) )
			 		   PRIMARY INDEX (VECTOR_ID, VECTOR_INDEX);" ))

 	max_matrix_id_value <<- 1
 	max_Sparsematrix_id_value <<- 1
 	max_vector_id_value <<- 1
 	flag1 <<- 1
 	flag2 <<- 1
 	flag3 <<- 1
 	
 	print("DONE..")
}

flag1Check <- function(connection)
{
	if(flag1==0)
	{
		temp <- sqlQuery(connection,
			 			paste0(" CREATE TABLE ",result_db_name,".",result_matrix_table,", FALLBACK ,
							     NO BEFORE JOURNAL,
							     NO AFTER JOURNAL,
							     CHECKSUM = DEFAULT,
							     DEFAULT MERGEBLOCKRATIO
							     (
							      MATRIX_ID INTEGER,
							      ROW_ID INTEGER,
							      COL_ID INTEGER,
							      CELL_VAL FLOAT)
				    			 PRIMARY INDEX ( MATRIX_ID, ROW_ID, COL_ID );"))

	 	if(temp!="No Data" || length(temp)!=1) 
	 	{
	 		sqlQuery(connection,
					 paste0("DROP TABLE ",result_db_name,".",result_matrix_table,"; 
					 		 CREATE TABLE ",result_db_name,".",result_matrix_table,", FALLBACK ,
						     NO BEFORE JOURNAL,
						     NO AFTER JOURNAL,
						     CHECKSUM = DEFAULT,
						     DEFAULT MERGEBLOCKRATIO
						     (
						      MATRIX_ID INTEGER,
						      ROW_ID INTEGER,
						      COL_ID INTEGER,
						      CELL_VAL FLOAT)
			    			 PRIMARY INDEX ( MATRIX_ID, ROW_ID, COL_ID );"))
	 	}
	 	flag1 <<- 1
	}
}

flag2Check <- function(connection)
{
	if(flag2==0)
 	{
 		temp <- sqlQuery(connection,
						 paste0(" CREATE TABLE ",result_db_name,".",result_Sparsematrix_table,", FALLBACK ,
							     NO BEFORE JOURNAL,
							     NO AFTER JOURNAL,
							     CHECKSUM = DEFAULT,
							     DEFAULT MERGEBLOCKRATIO
							     (
							      MATRIX_ID INTEGER,
							      ROW_ID INTEGER,
							      COL_ID INTEGER,
							      CELL_VAL FLOAT)
				    			 PRIMARY INDEX ( MATRIX_ID, ROW_ID, COL_ID );"))
 		
 		if(temp!="No Data" || length(temp)!=1) 
 		{
 			sqlQuery(connection,
					 paste0("DROP TABLE ",result_db_name,".",result_Sparsematrix_table,";
					 		 CREATE TABLE ",result_db_name,".",result_Sparsematrix_table,", FALLBACK ,
						     NO BEFORE JOURNAL,
						     NO AFTER JOURNAL,
						     CHECKSUM = DEFAULT,
						     DEFAULT MERGEBLOCKRATIO
						     (
						      MATRIX_ID INTEGER,
						      ROW_ID INTEGER,
						      COL_ID INTEGER,
						      CELL_VAL FLOAT)
			    			 PRIMARY INDEX ( MATRIX_ID, ROW_ID, COL_ID );"))
 		}
 		flag2 <<- 1
 	}
}

flag3Check <- function(connection)
{
	if(flag3==0)
	{
	 	temp <- sqlQuery(connection,
	 					 paste0("CREATE TABLE ",result_db_name,".",result_vector_table," 
	 					 		 ( VECTOR_ID INT, 
	 					 		   VECTOR_INDEX INT, 
	 					 		   VECTOR_VALUE VARCHAR(20) )
	 					 		   PRIMARY INDEX (VECTOR_ID, VECTOR_INDEX);" ))
	 	if(temp != TRUE || length(temp)!=1) 
	 	{
	 		sqlQuery(connection,
	 		 		 paste0("DROP TABLE ",result_db_name,".",result_vector_table,";
	 		 		 		 CREATE TABLE ",result_db_name,".",result_vector_table," 
	 		 		 		( VECTOR_ID INT,
	 		 		 		  VECTOR_INDEX INT,
	 		 		 		  VECTOR_VALUE VARCHAR(20) )
	 		 		 		  PRIMARY INDEX (VECTOR_ID, VECTOR_INDEX);" ))
	 	}
	 	flag3 <<- 1
	}
}
