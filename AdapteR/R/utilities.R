# Contains the support functions

getRemoteTableName <- function(database,
                               matrix_table) {
## gk: todo: use options(...) and start  session if required
##    if(is.null(result_matrix_table))
##        FLStartSession()
       
    return(paste0(database,".",matrix_table))
}



sqlError <- function(e){
    print(e)
    sys.call()
}
################################################################################
######  provide methods for JDBC with same signature as ODBC methods
################################################################################
if(!exists("sqlQuery")) sqlQuery <- function(channel,query) UseMethod("sqlQuery")
if(!exists("sqlSendUpdate")) sqlSendUpdate <- function(channel,query) UseMethod("sqlSendUpdate")

options(debugSQL=TRUE)
FLdebugSQL <<- TRUE
sqlSendUpdate.JDBCConnection <- function(channel,query) {
    sapply(query, function(q){
        ##browser()
        if(getOption("debugSQL")) cat(paste0("SENDING SQL: \n",gsub(" +"," ",q),"\n"))
        tryCatch({
            R <- dbSendUpdate(connection,q)
            ##dbCommit(connection)
            return(R)
        },
        error=function(e) sqlError(e))
    })
}

sqlSendUpdate.RODBC <- function(connection,query) {
    odbcSetAutoCommit(connection, autoCommit = FALSE)
    sapply(query, function(q){
        if(FLdebugSQL) cat(paste0("SENDING SQL: \n",gsub(" +"," ",q),"\n"))
        err<-sqlQuery(connection,q,errors=FALSE)
        errmsg<- odbcGetErrMsg(connection)
        if(length(errmsg) == 0 || as.character(errmsg)=="No Data")
        {
        	odbcEndTran(connection, commit = TRUE)
        }
        else
        {
        	odbcEndTran(connection, commit = FALSE)
        	print(errmsg)
        }
        odbcClearError(connection)
	})
    odbcSetAutoCommit(connection, autoCommit = TRUE)
    cat("DONE...")
}


sqlQuery.JDBCConnection <- function(channel,query, ...) {
    if(length(query)==1){
        if(getOption("debugSQL")) cat(paste0("QUERY SQL: \n",query,"\n"))
        tryCatch({
            resd <- dbGetQuery(connection, query, ...)
            return(resd)
        },
        error=function(e) cat(paste0(sqlError(e))))
    }
    lapply(query, function(q){
        if(getOption("debugSQL")) cat(paste0("QUERY SQL: \n",q,"\n"))
        tryCatch({
            resd <- dbGetQuery(connection, q, ...)
            return(resd)
        },
        error=function(e) cat(paste0(sqlError(e))))
    })
}


list_to_where_clause <- function (x) {
	     where_clause <- paste(names(x),x,sep="=\'",collapse="\' AND ");
	     where_clause <- paste(where_clause,"\'",sep="");
       if(nchar(where_clause) > 1) {
         where_clause <- where_clause
       } else {
         where_clause <- "1=1"
       }
	     #where_clause <- ifelse(nchar(where_clause) > 1, where_clause, "1=1");
	     where_clause
     }

# /**
#  * Converts List to class Spec. Used for Data Prep
#  * @param  {list} x e.g. list(Varx="a",Vary="b")
#  * @return {string}   "Varx(x), Vary(y)"
#  */
list_to_class_spec <- function (x) {
	classSpec <- paste(names(x),x,sep="(",collapse="), ")
	classSpec <- paste(classSpec,")",sep="")
	if(nchar(classSpec) > 1) {
	  classSpec <- classSpec
	} else {
	  classSpec <- ""
	}
	#classSpec <- ifelse(nchar(classSpec) > 1, classSpec, "");
	classSpec
}

# /**
#  * Converts List to class Spec. Used for Data Prep
#  * @param  {list} x e.g. list(Varx="a",Vary="b")
#  * @return {string}   "Varx(x), Vary(y)"
#  */
list_to_exclude_clause <- function (x) {
	excludeClause <- paste(x, collapse=", ")
	excludeClause
}

calc_cat_to_dummy <- function(ClassSpec) {
	if (length(ClassSpec) == 0) 
    CatToDummy <- 0
	else 
    CatToDummy <- 1;
	CatToDummy
}

validate_args <- function (arg_list, type_list, class_list = list())
{
	for (name in names(type_list)) {
		if( typeof(arg_list[[name]]) != type_list[[name]])
		{
			stop(paste("Argument Type Mismatch:", name, "should be of type", type_list[[name]]))	
		}	
	}
	for (name in names(class_list)) {
		if( class(arg_list[[name]]) != class_list[[name]])
			stop(paste("Argument Type Mismatch:", name, "should be of class", class_list[[name]]))		
	}
}

is_integer <- function(x) { (x == ceiling(x)||x == floor(x)) }
is_number  <- function(x) { (x == ceiling(x)||x == floor(x))&&(x>=1) }

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
    			 paste0("DROP TABLE ",result_db_name,".",result_matrix_table))
 	  	sqlQuery(connection, 
 	  			 paste0("DROP TABLE ",result_db_name,".",result_Sparsematrix_table))
 	  	sqlQuery(connection, 
 	  			 paste0("DROP TABLE ",result_db_name,".",result_vector_table))
    	odbcClose(connection)
    	flag1 <<- 0
    	flag2 <<- 0
    	flag3 <<- 0
    	max_matrix_id_value <<- 1
    	max_Sparsematrix_id_value <<- 1
    	max_vector_id_value <<- 1
}

gen_table_name <- function(prefix,suffix){
    ifelse(is.null(suffix),
           paste0(prefix),
           paste0(prefix,"_",suffix))
}

FLStartSession <- function(connection,
                           db_name="FL_DEMO",
                           persistent="test",
                           drop=TRUE,
                           tableoptions=", FALLBACK ,
				     NO BEFORE JOURNAL,
				     NO AFTER JOURNAL,
				     CHECKSUM = DEFAULT,
				     DEFAULT MERGEBLOCKRATIO")
{
    result_db_name <<- db_name
##    browser()
    sendqueries <- c(
        paste0("DATABASE ",result_db_name,";"),
        paste0("SET ROLE ALL;"))
    sqlSendUpdate(connection, sendqueries)

    result_Sparsematrix_table <<- gen_table_name("tblMatrixMultiResultSparse",persistent)
    ##max_Sparsematrix_id_value <<- max_Sparsematrix_id_value + 1
 	max_Sparsematrix_id_value <<- 0

    max_matrix_id_value <<- 0
    ##max_matrix_id_value <<- max_matrix_id_value + 1
    result_matrix_table <<- gen_table_name("tblMatrixMultiResult",persistent)

    max_vector_id_value <<- 0
    ##max_vector_id_value <- max_vector_id_value + 1
    result_vector_table <<- gen_table_name("tblVectorResult",persistent)

    if(drop){
        sqlSendUpdate(connection,
                      c(paste0("DROP TABLE ",result_matrix_table,";"),
                        paste0("DROP TABLE ",result_Sparsematrix_table,";"),
                        paste0("DROP TABLE ",result_vector_table,";")))
    }
    
    sendqueries <- c(
        paste0(" CREATE ",ifelse(is.null(persistent),"VOLATILE TABLE ","TABLE "),
               result_matrix_table,
               tableoptions,
				"     (
				      MATRIX_ID INTEGER,
				      ROW_ID INTEGER,
				      COL_ID INTEGER,
				      CELL_VAL FLOAT)
                     PRIMARY INDEX ( MATRIX_ID, ROW_ID, COL_ID );"),
        paste0(" CREATE ",ifelse(is.null(persistent),"VOLATILE TABLE ","TABLE "),
               result_Sparsematrix_table,
               tableoptions,
               "
				     (
				      MATRIX_ID INTEGER,
				      ROW_ID INTEGER,
				      COL_ID INTEGER,
				      CELL_VAL FLOAT)
	    			 PRIMARY INDEX ( MATRIX_ID, ROW_ID, COL_ID );"),
        paste0(" CREATE ",ifelse(is.null(persistent),"VOLATILE TABLE ","TABLE "),
               result_vector_table,
               tableoptions,
               "
			 		 ( VECTOR_ID INT, 
			 		   VECTOR_INDEX INT, 
				 	   VECTOR_VALUE VARCHAR(20) )
			 		   PRIMARY INDEX (VECTOR_ID, VECTOR_INDEX);" ))
    sqlSendUpdate(connection, sendqueries)
 	cat("DONE..\n")
}

flag1Check <- function(connection)
{
    return(TRUE)
	if(flag1==0)
	{
		sqlQuery(connection,paste0(" DATABASE ",result_db_name,"; SET ROLE ALL;"))

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
	 				 paste0("DROP TABLE ",result_db_name,".",result_matrix_table))

	 		sqlQuery(connection,
					 paste0("CREATE TABLE ",result_db_name,".",result_matrix_table,", FALLBACK ,
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
    return(TRUE)
	if(flag2==0)
 	{
 		sqlQuery(connection,paste0(" DATABASE ",result_db_name,"; SET ROLE ALL;"))

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
 					 paste0("DROP TABLE ",result_db_name,".",result_Sparsematrix_table))

 			sqlQuery(connection,
					 paste0("CREATE TABLE ",result_db_name,".",result_Sparsematrix_table,", FALLBACK ,
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
    return(TRUE)
	if(flag3==0)
	{
		sqlQuery(connection,paste0(" DATABASE ",result_db_name,"; SET ROLE ALL;"))

	 	temp <- sqlQuery(connection,
	 					 paste0("CREATE TABLE ",result_db_name,".",result_vector_table," 
	 					 		 ( VECTOR_ID INT, 
	 					 		   VECTOR_INDEX INT, 
	 					 		   VECTOR_VALUE VARCHAR(20) )
	 					 		   PRIMARY INDEX (VECTOR_ID, VECTOR_INDEX);" ))
	 	if(temp != TRUE || length(temp)!=1) 
	 	{
	 		sqlQuery(connection,
	 				 paste0("DROP TABLE ",result_db_name,".",result_vector_table))

	 		sqlQuery(connection,
	 		 		 paste0("CREATE TABLE ",result_db_name,".",result_vector_table," 
	 		 		 		( VECTOR_ID INT,
	 		 		 		  VECTOR_INDEX INT,
	 		 		 		  VECTOR_VALUE VARCHAR(20) )
	 		 		 		  PRIMARY INDEX (VECTOR_ID, VECTOR_INDEX);" ))
	 	}
	 	flag3 <<- 1
	}
}


