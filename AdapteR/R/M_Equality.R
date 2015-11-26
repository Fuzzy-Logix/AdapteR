identical <- function(object1, object2)
{
	UseMethod("identical", object1)
}

identical.default <- base::identical

identical.FLMatrix <- function(object1, object2)
{
	if(is.FLMatrix(object1) && is.FLMatrix(object2))
	{
		sqlstr <- paste("DATABASE ",object1@db_name,";
						 SET ROLE ALL;")
		sqlstr <- paste("SELECT 0
						 FROM ",object1@db_name,".",object1@matrix_table,"a,
						 	  ",object2@db_name,".",object2@matrix_table,"b
						 WHERE a.",object1@matrix_id_colname,"=",object1@matrix_id_value,"
						 AND   b.",object2@matrix_id_colname,"=",object2@matrix_id_value,"
						 AND   a.",object1@row_id_colname,"= b.",object2@row_id_colname,"
						 AND   a.",object1@col_id_colname,"= b.",object2@col_id_colname,"
						 AND   a.",object1@cell_val_colname,"<> b.",object2@cell_val_colname)
		retobj<-sqlQuery(object1@odbc_connection,sqlstr)
		if(nrow(retobj) == 0)
		{
			return(TRUE)

		}
		else if (nrow(retobj) > 0)
		{
			return(FALSE)
		}
	}
	else
	{
		base::identical(object1,object2)
	}

}

"==" <- function(object1, object2)
{
    UseMethod("==", object1)
}

`==.default` <- function(object1,object2)
{
	op <- .Primitive("==")
	op(object1,object2)
}

`==.FLMatrix` <- function(object1,object2)
{
	if(is.FLMatrix(object1) && is.FLMatrix(object2))
	{
		sqlstr <- paste("DATABASE ",object1@db_name,";
						 SET ROLE ALL;")
		sqlstr <- paste("SELECT 0
						 FROM ",object1@db_name,".",object1@matrix_table," a,
						 	  ",object2@db_name,".",object2@matrix_table," b
						 WHERE a.",object1@matrix_id_colname,"=",object1@matrix_id_value," 
						 AND   b.",object2@matrix_id_colname,"=",object2@matrix_id_value," 
						 AND   a.",object1@row_id_colname,"= b.",object2@row_id_colname," 
						 AND   a.",object1@col_id_colname,"= b.",object2@col_id_colname," 
						 AND   a.",object1@cell_val_colname,"<> b.",object2@cell_val_colname)
		retobj<-sqlQuery(object1@odbc_connection,sqlstr)

		if(nrow(retobj) == 0)
		{
			return(TRUE)

		}
		else if (nrow(retobj) > 0)
		{
			return(FALSE)
		}
	}
	else
	{
		base::identical(object1,object2)
	}
}

`==.FLSparseMatrix` <- function(object1,object2)
{
	if(is.FLSparseMatrix(object1) && is.FLSparseMatrix(object2))
	{
		if((nrow(object2) != nrow(object1)) || (ncol(object1) != ncol(object2)))
		return(FALSE)

		sqlstr <- paste("DATABASE ",object1@db_name,";
						 SET ROLE ALL;")
		sqlstr <- paste("SELECT 0
						 FROM ",object1@db_name,".",object1@matrix_table," a,
						 	  ",object2@db_name,".",object2@matrix_table," b
						 WHERE a.",object1@matrix_id_colname,"=",object1@matrix_id_value," 
						 AND   b.",object2@matrix_id_colname,"=",object2@matrix_id_value," 
						 AND   a.",object1@row_id_colname,"= b.",object2@row_id_colname," 
						 AND   a.",object1@col_id_colname,"= b.",object2@col_id_colname," 
						 AND   a.",object1@cell_val_colname,"<> b.",object2@cell_val_colname)
		retobj<-sqlQuery(object1@odbc_connection,sqlstr)

		if(nrow(retobj) == 0)
		{
			return(TRUE)

		}
		else if (nrow(retobj) > 0)
		{
			return(FALSE)
		}
	}
	else
	{
		base::identical(object1,object2)
	}
}

`==.FLVector` <- function(object1,object2)
{
	if(is.FLVector(object1) && is.FLVector(object2))
	{
		if(object2@size != object1@size)
		return(FALSE)

		sqlstr <- paste("DATABASE ",object1@table@db_name,";
						 SET ROLE ALL;")

		if(object1@table@isDeep && object2@table@isDeep)
		{
			sqlstr <- paste("SELECT 0
							 FROM ",object1@table@db_name,".",object1@table@table_name,"a,
							 	  ",object2@table@db_name,".",object2@table@table_name,"b
							 WHERE a.",object1@table@primary_key,"=",object1@vector_id_value,"
							 AND   b.",object2@table@primary_key,"=",object2@vector_id_value,"
							 AND   a.",object1@table@var_id_name,"= b.",object2@table@var_id_name,"
							 AND   a.",object1@col_name," <> b.",object2@col_name)
		}

		if(!object1@table@isDeep && object2@table@isDeep)
		{
			sqlstr <- paste("SELECT 0
							 FROM ",object1@table@db_name,".",object1@table@table_name,"a,
							 	  ",object2@table@db_name,".",object2@table@table_name,"b
							 WHERE b.",object2@table@primary_key,"=",object2@vector_id_value,"
							 AND   a.",object1@table@primary_key,"= b.",object2@table@var_id_name,"
							 AND   a.",object1@col_name," <> b.",object2@col_name)
		}

		if(object1@table@isDeep && !object2@table@isDeep)
		{
			sqlstr <- paste("SELECT 0
							 FROM ",object1@table@db_name,".",object1@table@table_name,"a,
							 	  ",object2@table@db_name,".",object2@table@table_name,"b
							 WHERE a.",object1@table@primary_key,"=",object1@vector_id_value,"
							 AND   b.",object2@table@primary_key,"= a.",object1@table@var_id_name,"
							 AND   a.",object1@col_name," <> b.",object2@col_name)
		}

		if(!object1@table@isDeep && !object2@table@isDeep)
		{
			sqlstr <- paste("SELECT 0
							 FROM ",object1@table@db_name,".",object1@table@table_name,"a,
							 	  ",object2@table@db_name,".",object2@table@table_name,"b
							 WHERE a.",object1@table@primary_key,"= b.",object2@table@primary_key," 
							 AND   a.",object1@col_name,"<> b.",object2@col_name)
		}


		retobj<-sqlQuery(object1@table@odbc_connection,sqlstr)

		if(nrow(retobj) == 0)
		{
			return(TRUE)

		}
		else if (nrow(retobj) > 0)
		{
			return(FALSE)
		}
	}
	else
	{
		return(FALSE)
	}
}
