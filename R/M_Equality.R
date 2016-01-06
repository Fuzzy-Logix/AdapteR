identical <- function(object1, object2)
{
	UseMethod("identical", object1)
}

identical.default <- base::identical

identical.FLMatrix <- function(object1, object2)
{
	if(is.FLMatrix(object1) && is.FLMatrix(object2))
	{
		sqlstr <- paste("SELECT 0
						 FROM ",remoteTable(object1),"a,
						 	  ",remoteTable(object2),"b",
                        constructWhere(c(constraintsSQL(object1,"a"),
                                         constraintsSQL(object2,"b"),
                                         paste0("a.",object1@variables$rowIdColumn,"= b.",object2@variables$rowIdColumn),
                                         paste0("a.",object1@variables$colIdColumn,"= b.",object2@variables$colIdColumn),
                                         paste0("a.",object1@variables$valueColumn,"<> b.",object2@variables$valueColumn))))
		retobj<-sqlQuery(getConnection(object1),sqlstr)
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
		sqlstr <- paste("SELECT 0
						 FROM ",remoteTable(object1),"a,
						 	  ",remoteTable(object2),"b",
                        constructWhere(c(constraintsSQL(object1,"a"),
                                         constraintsSQL(object2,"b"),
                                         paste0("a.",object1@variables$rowIdColumn,"= b.",object2@variables$rowIdColumn),
                                         paste0("a.",object1@variables$colIdColumn,"= b.",object2@variables$colIdColumn),
                                         paste0("a.",object1@variables$valueColumn,"<> b.",object2@variables$valueColumn))))
		retobj<-sqlQuery(getConnection(object1),sqlstr)

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

		sqlstr <- paste("SELECT 0
						 FROM ",remoteTable(object1),"a,
						 	  ",remoteTable(object2),"b",
                        constructWhere(c(constraintsSQL(object1,"a"),
                                         constraintsSQL(object2,"b"),
                                         paste0("a.",object1@variables$rowIdColumn,"= b.",object2@variables$rowIdColumn),
                                         paste0("a.",object1@variables$colIdColumn,"= b.",object2@variables$colIdColumn),
                                         paste0("a.",object1@variables$valueColumn,"<> b.",object2@variables$valueColumn))))

		if(object1@isDeep && object2@isDeep)
		{
			sqlstr <- paste("SELECT 0
							 FROM ",object1@db_name,".",object1@table_name,"a,
							 	  ",object2@db_name,".",object2@table_name,"b
							 WHERE a.",object1@obs_id_colname,"=",object1@vector_id_value,"
							 AND   b.",object2@obs_id_colname,"=",object2@vector_id_value,"
							 AND   a.",object1@var_id_name,"= b.",object2@var_id_name,"
							 AND   a.",object1@col_name," <> b.",object2@col_name)
		}

		if(!object1@isDeep && object2@isDeep)
		{
			sqlstr <- paste("SELECT 0
							 FROM ",object1@db_name,".",object1@table_name,"a,
							 	  ",object2@db_name,".",object2@table_name,"b
							 WHERE b.",object2@obs_id_colname,"=",object2@vector_id_value,"
							 AND   a.",object1@obs_id_colname,"= b.",object2@var_id_name,"
							 AND   a.",object1@col_name," <> b.",object2@col_name)
		}

		if(object1@isDeep && !object2@isDeep)
		{
			sqlstr <- paste("SELECT 0
							 FROM ",object1@db_name,".",object1@table_name,"a,
							 	  ",object2@db_name,".",object2@table_name,"b
							 WHERE a.",object1@obs_id_colname,"=",object1@vector_id_value,"
							 AND   b.",object2@obs_id_colname,"= a.",object1@var_id_name,"
							 AND   a.",object1@col_name," <> b.",object2@col_name)
		}

		if(!object1@isDeep && !object2@isDeep)
		{
			sqlstr <- paste("SELECT 0
							 FROM ",object1@db_name,".",object1@table_name,"a,
							 	  ",object2@db_name,".",object2@table_name,"b
							 WHERE a.",object1@obs_id_colname,"= b.",object2@obs_id_colname," 
							 AND   a.",object1@col_name,"<> b.",object2@col_name)
		}


		retobj<-sqlQuery(getConnection(object1),sqlstr)

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
