#' @include utilities.R
#' @include FLIs.R
#' @include FLCastFunctions.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLSparseMatrix.R
#' @include FLTable.R
#' @include FLDims.R
#' @include FLPrint.R
NULL

#' Addition of in-database objects.
#'
#' \code{+} does the addition of in-database objects.
#'
#' The addition of in-database objects mimics the normal addition of R data types.
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param x can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param y can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{+} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 1)
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix + Rvector
#' @export

"+" <- function(x,y)
{
    UseMethod("+", x)
}

`+.default` <- function(x,y)
{
	op <- .Primitive("+")
	op(x,y)
}

`+.matrix` <- function(x,flmatobj1)
{
	if(is.FLMatrix(flmatobj1))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj1@odbc_connection)
		flmatobj2+flmatobj1
	}
	else if(is.FLSparseMatrix(flmatobj1))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj1@odbc_connection)
		flmatobj1+flmatobj2
	}
	else if(is.FLVector(flmatobj1))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj1@odbc_connection)
		flmatobj2+flmatobj1
	}
	else 
	{
		op <- .Primitive("+")
		op(x,flmatobj1)
	}
}

`+.FLMatrix` <- function(flmatobj1, flmatobj2)
{
	nrow1 <- nrow(flmatobj1)
	ncol1 <- ncol(flmatobj1)
	if(is.FLMatrix(flmatobj2))
	{
		if(nrow(flmatobj1) == nrow(flmatobj2) && ncol(flmatobj1) == ncol(flmatobj2))
		{
			flag1Check(flmatobj1@odbc_connection)
			sqlSendUpdate(flmatobj1@odbc_connection,
					 paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
					 		" SELECT ",max_matrix_id_value," AS MATRIX_ID ,
					 				 a.",flmatobj1@variables$rowId," AS ROW_ID ,
					 				 a.",flmatobj1@variables$colId," AS COL_ID ,
					 				 a.",flmatobj1@variables$value,"+b.",flmatobj2@variables$value," AS CELL_VAL 
					 		  FROM ",remoteTable(flmatobj1)," a,",
					 		  		 remoteTable(flmatobj2)," b 
					 		  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
					 		  AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
					 		  AND a.",flmatobj1@variables$rowId,"=b.",flmatobj2@variables$rowId," 
					 		  AND a.",flmatobj1@variables$colId,"=b.",flmatobj2@variables$colId))
			
			max_matrix_id_value <<- max_matrix_id_value + 1
			FLMatrix( 
				 connection = flmatobj1@odbc_connection, 
				 database = result_db_name, 
				 matrix_table = result_matrix_table, 
				 matrix_id_value = max_matrix_id_value - 1, 
				 matrix_id_colname = "MATRIX_ID", 
				 row_id_colname = "ROW_ID", 
				 col_id_colname = "COL_ID", 
				 cell_val_colname = "CELL_VAL", 
				 nrow = nrow1, 
				 ncol = ncol1)
		}
		else 
		{
			stop("ERROR: Invalid matrix dimensions for addition")
		}
	}
	
	else if(is.vector(flmatobj2))
	{
		flmatobj2 <- as.FLVector(flmatobj2,flmatobj1@odbc_connection)
		flmatobj1+flmatobj2
	}
	
	else if(is.matrix(flmatobj2))
	{
		flmatobj2 <- as.FLMatrix(flmatobj2,flmatobj1@odbc_connection)
		flmatobj1+flmatobj2
	}
	
	else if(class(flmatobj2)=="dgCMatrix")
	{
		flmatobj2 <- as.FLSparseMatrix(flmatobj2,flmatobj1@odbc_connection)
		flmatobj2+flmatobj1
	}
	
	else if(is.FLSparseMatrix(flmatobj2))
	{
		flmatobj2+flmatobj1
	}
	
	else if(is.FLVector(flmatobj2))
	{
		flag1Check(flmatobj1@odbc_connection)

		if(!flmatobj2@isDeep)
		{
			sqlSendUpdate(flmatobj1@odbc_connection,
					 paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
							" WITH Z(MATRIX_ID,ROW_ID,COL_ID,CELL_VAL,ROW_NUM) 
							  AS (SELECT a.",flmatobj1@matrix_id_colname,",
							  			 a.",flmatobj1@variables$rowId,",
							  			 a.",flmatobj1@variables$colId,",
							  			 a.",flmatobj1@variables$value,", 
							  			 ROW_NUMBER() OVER (ORDER BY a.",flmatobj1@variables$colId,",
							  			 							 a.",flmatobj1@variables$rowId,") AS ROW_NUM  
			        			  FROM ",remoteTable(flmatobj1)," a 
			        			  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,") 
					          SELECT ",max_matrix_id_value,",
					          		  Z.ROW_ID,
					          		  Z.COL_ID,
					          		  Z.CELL_VAL+b.",flmatobj2@col_name,
					        " FROM ",remoteTable(flmatobj2)," b,
					        		Z 
					          WHERE Z.ROW_NUM MOD ",length(flmatobj2)," = b.",flmatobj2@obs_id_colname," MOD ",length(flmatobj2)))
		}

		else
		{
			sqlSendUpdate(flmatobj1@odbc_connection,
					 paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
							" WITH Z(MATRIX_ID,ROW_ID,COL_ID,CELL_VAL,ROW_NUM) 
							  AS (SELECT a.",flmatobj1@matrix_id_colname,",
							  			 a.",flmatobj1@variables$rowId,",
							  			 a.",flmatobj1@variables$colId,",
							  			 a.",flmatobj1@variables$value,", 
							  			 ROW_NUMBER() OVER (ORDER BY a.",flmatobj1@variables$colId,",
							  			 							 a.",flmatobj1@variables$rowId,") AS ROW_NUM  
			        			  FROM ",remoteTable(flmatobj1)," a 
			        			  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,") 
	         				  SELECT ",max_matrix_id_value,",
	         				  		 Z.ROW_ID,
	         				  		 Z.COL_ID,
	         				  		 Z.CELL_VAL+b.",flmatobj2@col_name,
	         				" FROM ",remoteTable(flmatobj2)," b,
	         						 Z 
	          				  WHERE Z.ROW_NUM MOD ",length(flmatobj2)," = b.",flmatobj2@var_id_name," MOD ",length(flmatobj2),
	          				" AND b.",flmatobj2@obs_id_colname,"=",flmatobj2@vector_id_value))
		}

		max_matrix_id_value <<- max_matrix_id_value + 1
		FLMatrix( 
			 connection = flmatobj1@odbc_connection, 
			 database = result_db_name, 
			 matrix_table = result_matrix_table, 
			 matrix_id_value = max_matrix_id_value - 1, 
			 matrix_id_colname = "MATRIX_ID", 
			 row_id_colname = "ROW_ID", 
			 col_id_colname = "COL_ID", 
			 cell_val_colname = "CELL_VAL", 
			 nrow = nrow1, 
			 ncol = ncol1)
		
	}
	else cat("ERROR::Operation Currently Not Supported")
}

`+.numeric` <- function(x,obj1)
{	
	if(missing(obj1))
	{
		op <- .Primitive("+")
		return(op(x))
	}

	if(is.FLMatrix(obj1))
	{
		obj2 <- as.FLVector(x,obj1@odbc_connection)
		obj2 + obj1
	}

	else if(class(obj1)=="FLSparseMatrix")
	{
		obj2 <- as.FLVector(x,obj1@odbc_connection)
		obj1+obj2
	}

	else if(class(obj1)=="FLVector")
	{
		obj2 <- as.FLVector(x,obj1@odbc_connection)
		obj1+obj2
	}

	else
	{
		op <- .Primitive("+")
		op(x,obj1)
	}
}

`+.FLSparseMatrix` <- function(flmatobj1, flmatobj2)
{
	nrow1 <- nrow(flmatobj1)
	ncol1 <- ncol(flmatobj1)
	
	if(is.FLMatrix(flmatobj2) || is.FLSparseMatrix(flmatobj2))
	{
		nrow2 <- nrow(flmatobj2)
		ncol2 <- ncol(flmatobj2)

		if(nrow1 == nrow2 && ncol1 == ncol2)
		{
			if(is.FLSparseMatrix(flmatobj2))
			{
				flag2Check(flmatobj1@odbc_connection)
				sqlSendUpdate(flmatobj1@odbc_connection,
						 paste0(" INSERT INTO ",result_db_name,".",result_Sparsematrix_table,
								" SELECT DISTINCT ",max_Sparsematrix_id_value,",
										 a.",flmatobj1@variables$rowId,",
										 a.",flmatobj1@variables$colId,",
										 a.",flmatobj1@variables$value," 
								  FROM ",remoteTable(flmatobj1)," a 
								  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,
			            		" except ",
			            		"SELECT ",max_Sparsematrix_id_value,",
			            				a.",flmatobj1@variables$rowId,",
			            				a.",flmatobj1@variables$colId,",
			            				a.",flmatobj1@variables$value,
			            		" FROM ",remoteTable(flmatobj1)," a, 
			            			   ",remoteTable(flmatobj2)," b 
			            		  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
			            		  AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
			            		  AND a.",flmatobj1@variables$rowId," = b.",flmatobj2@variables$rowId," 
			            		  AND a.",flmatobj1@variables$colId,"=b.",flmatobj2@variables$colId,
			            		" UNION ALL ",
			            		"SELECT DISTINCT ",max_Sparsematrix_id_value,",
			            				b.",flmatobj2@variables$rowId,",
			            				b.",flmatobj2@variables$colId,",
			            				b.",flmatobj2@variables$value,
			            		" FROM ",remoteTable(flmatobj2)," b 
			            		  WHERE b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value,
			            		" except ",
			            		" SELECT ",max_Sparsematrix_id_value,",
			            				  b.",flmatobj2@variables$rowId,",
			            				  b.",flmatobj2@variables$colId,",
			            				  b.",flmatobj2@variables$value,
			            		" FROM ",remoteTable(flmatobj1)," a,",
			            				 remoteTable(flmatobj2)," b 
			            		  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
			            		  AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
			            		  AND a.",flmatobj1@variables$rowId," = b.",flmatobj2@variables$rowId," 
			            		  AND a.",flmatobj1@variables$colId,"=b.",flmatobj2@variables$colId,
			            		" UNION ALL ",
			            		" SELECT ",max_Sparsematrix_id_value,",
			            				 a.",flmatobj1@variables$rowId,",
			            				 a.",flmatobj1@variables$colId,",
			            				 a.",flmatobj1@variables$value,"+b.",flmatobj2@variables$value,
			            		" FROM ",remoteTable(flmatobj1)," a, ",
			            				 remoteTable(flmatobj2)," b 
			            		  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
			            		  AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
			            		  AND a.",flmatobj1@variables$rowId," = b.",flmatobj2@variables$rowId," 
			            		  AND a.",flmatobj1@variables$colId," =b.",flmatobj2@variables$colId))	
				
				max_Sparsematrix_id_value <<- max_Sparsematrix_id_value + 1
				new("FLSparseMatrix", 
					 odbc_connection = flmatobj1@odbc_connection, 
					 database = result_db_name, 
					 matrix_table = result_Sparsematrix_table, 
					 matrix_id_value = max_Sparsematrix_id_value - 1, 
					 matrix_id_colname = "MATRIX_ID", 
					 row_id_colname = "ROW_ID", 
					 col_id_colname = "COL_ID", 
					 cell_val_colname = "CELL_VAL", 
					 nrow = nrow1, 
					 ncol = ncol1, 
					 dimnames= list(c(),c()))
			}
			else
			{
				flag1Check(flmatobj2@odbc_connection)
				sqlSendUpdate(flmatobj2@odbc_connection,
						 paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								" SELECT DISTINCT ",max_matrix_id_value,",
										 b.",flmatobj2@variables$rowId,",
										 b.",flmatobj2@variables$colId,",
										 b.",flmatobj2@variables$value,
								" FROM ",remoteTable(flmatobj2)," b 
								  WHERE b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value,
			            		" except ",
					            "SELECT ",max_matrix_id_value,",
					            		b.",flmatobj2@variables$rowId,",
					            		b.",flmatobj2@variables$colId,",
					            		b.",flmatobj2@variables$value,
					            " FROM ",remoteTable(flmatobj1)," a, ",
					            		 remoteTable(flmatobj2)," b 
					              WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,"
					              AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
					              AND b.",flmatobj2@variables$rowId ," = a.",flmatobj1@variables$rowId," 
					              AND b.",flmatobj2@variables$colId,"=a.",flmatobj1@variables$colId,
					            " UNION ALL ",
								" SELECT  DISTINCT ",max_matrix_id_value,",
										  a.",flmatobj1@variables$rowId,",
										  a.",flmatobj1@variables$colId,",
										  a.",flmatobj1@variables$value,"+b.",flmatobj2@variables$value,
								" FROM ",remoteTable(flmatobj1)," a, ",
										 remoteTable(flmatobj2)," b 
								  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
								  AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
								  AND a.",flmatobj1@variables$rowId," = b.",flmatobj2@variables$rowId," 
								  AND a.",flmatobj1@variables$colId," =b.",flmatobj2@variables$colId))
				max_matrix_id_value <<- max_matrix_id_value + 1
				FLMatrix( 
					 connection = flmatobj2@odbc_connection, 
					 database = result_db_name, 
					 matrix_table = result_matrix_table, 
					 matrix_id_value = max_matrix_id_value - 1, 
					 matrix_id_colname = "MATRIX_ID", 
					 row_id_colname = "ROW_ID", 
					 col_id_colname = "COL_ID", 
					 cell_val_colname = "CELL_VAL", 
					 nrow = nrow1, 
					 ncol = ncol1, 
					 dimnames = list(c(),c()))
			}
		}
		else stop("ERROR: Invalid matrix dimensions for addition")
	}
	else if(is.vector(flmatobj2))
		{
			flmatobj2 <- as.FLVector(flmatobj2,flmatobj1@odbc_connection)
			flmatobj1+flmatobj2
		}
	else if(is.matrix(flmatobj2))
		{
			flmatobj2 <- as.FLMatrix(flmatobj2,flmatobj1@odbc_connection)
			flmatobj1+flmatobj2
		}
	else if(class(flmatobj2)=="dgCMatrix")
		{
			flmatobj2 <- as.FLSparseMatrix(flmatobj2,flmatobj1@odbc_connection)
			flmatobj1+flmatobj2
		}
	else if(is.FLVector(flmatobj2))
		{
			flmatobj2 <- as.FLMatrix(flmatobj2,flmatobj1@odbc_connection,nr=nrow(flmatobj1),nc=ncol(flmatobj1))
			sqlstr <- paste0(" UPDATE ",flmatobj2@db_name,".",flmatobj2@matrix_table,
							 " FROM ( SELECT DISTINCT ",flmatobj2@matrix_id_value," AS mid,
							 				 a.",flmatobj1@variables$rowId," AS rid,
							 				 a.",flmatobj1@variables$colId," AS cid,
							 				 a.",flmatobj1@variables$value,"+b.",flmatobj2@variables$value," AS cval 
							 		  FROM ",remoteTable(flmatobj1)," a, ",
							 		  		 remoteTable(flmatobj2)," b 
							 		  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
							 		  AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
							 		  AND a.",flmatobj1@variables$rowId," = b.",flmatobj2@variables$rowId," 
							 		  AND a.",flmatobj1@variables$colId," =b.",flmatobj2@variables$colId,") c ",
							 " SET ",flmatobj2@variables$value,"= c.cval ",
							 " WHERE ",flmatobj2@matrix_id_colname,"= c.mid 
							   AND ",flmatobj2@variables$rowId,"= c.rid 
							   AND ",flmatobj2@variables$colId,"= c.cid")
			sqlSendUpdate(flmatobj1@odbc_connection,sqlstr)
			return(flmatobj2)
		}
	else stop("Operation Currently Not Supported")
}

`+.FLVector` <- function(pObj1,pObj2)
{
	vNrow1 <- length(pObj1)
	if(is.FLMatrix(pObj2))
	{
		return(pObj2+pObj1)
	}
	else if(is.vector(pObj2))
	{
		pObj2 <- as.FLVector(pObj2,pObj1@odbc_connection)
		pObj2+pObj1
	}
	else if(is.matrix(pObj2))
	{
		pObj2 <- as.FLMatrix(pObj2,pObj1@odbc_connection)
		pObj2+pObj1
	}
	else if(class(pObj2)=="dgCMatrix")
	{
		pObj2 <- as.FLSparseMatrix(pObj2,pObj1@odbc_connection)
		pObj2+pObj1
	}
	else if(is.FLSparseMatrix(pObj2))
	{
		pObj2+pObj1
	}
	else if(is.FLVector(pObj2))
	{
		flag3Check(pObj1@odbc_connection)
		if(length(pObj2) > length(pObj1))
		{
			vTemp <- pObj1
			pObj1 <- pObj2
			pObj2 <- vTemp
		}
		vMinSize <- length(pObj2)

		if(pObj1@isDeep && pObj2@isDeep)
		{
			vSqlStr <-paste0(" INSERT INTO ",result_db_name,".",result_vector_table,
					         " SELECT ",max_vector_id_value,",
					         		  a.",pObj1@var_id_name,", 
					         		  CAST(a.",pObj1@col_name,"+b.",pObj2@col_name," AS NUMBER) ",
					         " FROM ",remoteTable(pObj1)," a,",
					         		  remoteTable(pObj2)," b",
					         " WHERE a.",pObj1@obs_id_colname,"=",pObj1@vector_id_value," 
					           AND b.",pObj2@obs_id_colname,"=",pObj2@vector_id_value,
					         " AND a.",pObj1@var_id_name," MOD ",vMinSize," = b.",pObj2@var_id_name," MOD ",vMinSize)

			sqlSendUpdate(pObj1@odbc_connection,vSqlStr)
		}

		else if(xor(pObj1@isDeep,pObj2@isDeep))
		{
			if(pObj1@isDeep)
			{
				vSqlStr <-paste0(" INSERT INTO ",result_db_name,".",result_vector_table,
						         " SELECT ",max_vector_id_value,",
						         		  a.",pObj1@var_id_name,", 
						         		  CAST(a.",pObj1@col_name,"+b.",pObj2@col_name," AS NUMBER) ",
						         " FROM ",remoteTable(pObj1)," a,",
						         		  remoteTable(pObj2)," b",
						         " WHERE a.",pObj1@obs_id_colname,"=",pObj1@vector_id_value,
						         " AND a.",pObj1@var_id_name," MOD ",vMinSize," = b.",pObj2@obs_id_colname," MOD ",vMinSize)

			    sqlSendUpdate(pObj1@odbc_connection,vSqlStr)
			}
            else
            {
				vSqlStr <-paste0(" INSERT INTO ",result_db_name,".",result_vector_table,
						         " SELECT ",max_vector_id_value,",
						         		  a.",pObj1@obs_id_colname,", 
						         		  CAST(a.",pObj1@col_name,"+b.",pObj2@col_name," AS NUMBER) ",
						         " FROM ",remoteTable(pObj1)," a,",
						         		  remoteTable(pObj2)," b",
						         " WHERE b.",pObj2@obs_id_colname,"=",pObj2@vector_id_value,
						         " AND b.",pObj2@var_id_name," MOD ",vMinSize," = a.",pObj1@obs_id_colname," MOD ",vMinSize)

			    sqlSendUpdate(pObj1@odbc_connection,vSqlStr)
            }
		}

		else if(!pObj1@isDeep && !pObj2@isDeep)
		{
			vSqlStr <-paste0(" INSERT INTO ",result_db_name,".",result_vector_table,
					         " SELECT ",max_vector_id_value,",
					         		  a.",pObj1@obs_id_colname,", 
					         		  CAST(a.",pObj1@col_name,"+b.",pObj2@col_name," AS NUMBER) ",
					         " FROM ",remoteTable(pObj1)," a,",
					         		  remoteTable(pObj2)," b",
					         " WHERE b.",pObj2@var_id_name," MOD ",vMinSize," = a.",pObj1@obs_id_colname," MOD ",vMinSize)

			    sqlSendUpdate(pObj1@odbc_connection,vSqlStr)
		}

			max_vector_id_value <<- max_vector_id_value + 1
			table <- FLTable(connection,
							 result_db_name,
							 result_vector_table,
							 "VECTOR_ID",
							 "VECTOR_INDEX",
							 "VECTOR_VALUE")

			new("FLVector", 
				 table = table, 
				 col_name = table@variables$value, 
				 vector_id_value = max_vector_id_value-1, 
				 size = length(pObj1))
			
	}
	else cat("ERROR::Operation Currently Not Supported")
}


`+.dgCMatrix` <- function(x,flmatobj)
{
	if(is.FLSparseMatrix(flmatobj) || is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLSparseMatrix(x,flmatobj@odbc_connection)
		flmatobj2 + flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLSparseMatrix(x,flmatobj@odbc_connection)
		flmatobj2 + flmatobj
	}
	else
	{
		op <- .Primitive("+")
		op(x,flmatobj)
	}

}
