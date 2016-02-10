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
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj1))
		flmatobj2+flmatobj1
	}
	else if(is.FLSparseMatrix(flmatobj1))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj1))
		flmatobj1+flmatobj2
	}
	else if(is.FLVector(flmatobj1))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj1))
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
			flag1Check(getConnection(flmatobj1))
			sqlSendUpdate(getConnection(flmatobj1),
					 paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
					 		" SELECT ",max_matrix_id_value," AS MATRIX_ID ,
					 				 a.",getVariables(flmatobj1)$rowIdColumn," AS ROW_ID ,
					 				 a.",getVariables(flmatobj1)$colIdColumn," AS COL_ID ,
					 				 a.",getVariables(flmatobj1)$valueColumn,"+b.",getVariables(flmatobj2)$valueColumn," AS CELL_VAL 
					 		  FROM ",remoteTable(flmatobj1)," a,",
					 		  		 remoteTable(flmatobj2)," b 
					 		  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
					 		  AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
					 		  AND a.",getVariables(flmatobj1)$rowIdColumn,"=b.",getVariables(flmatobj2)$rowIdColumn," 
					 		  AND a.",getVariables(flmatobj1)$colIdColumn,"=b.",getVariables(flmatobj2)$colIdColumn))
			
			max_matrix_id_value <<- max_matrix_id_value + 1
			FLMatrix( 
				 connection = getConnection(flmatobj1), 
				 database = getOption("ResultDatabaseFL"), 
				 table_name = getOption("ResultMatrixTableFL"), 
				 matrix_id_value = max_matrix_id_value - 1, 
				 matrix_id_colname = "MATRIX_ID", 
				 row_id_colname = "rowIdColumn", 
				 col_id_colname = "colIdColumn", 
				 cell_val_colname = "valueColumn", 
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
		flmatobj2 <- as.FLVector(flmatobj2,getConnection(flmatobj1))
		flmatobj1+flmatobj2
	}
	
	else if(is.matrix(flmatobj2))
	{
		flmatobj2 <- as.FLMatrix(flmatobj2,getConnection(flmatobj1))
		flmatobj1+flmatobj2
	}
	
	else if(class(flmatobj2)=="dgCMatrix")
	{
		flmatobj2 <- as.FLSparseMatrix(flmatobj2,getConnection(flmatobj1))
		flmatobj2+flmatobj1
	}
	
	else if(is.FLSparseMatrix(flmatobj2))
	{
		flmatobj2+flmatobj1
	}
	
	else if(is.FLVector(flmatobj2))
	{
		flag1Check(getConnection(flmatobj1))

		if(!flmatobj2@isDeep)
		{
			sqlSendUpdate(getConnection(flmatobj1),
					 paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
							" WITH Z(MATRIX_ID,ROW_ID,COL_ID,CELL_VAL,ROW_NUM) 
							  AS (SELECT a.",flmatobj1@matrix_id_colname,",
							  			 a.",getVariables(flmatobj1)$rowIdColumn,",
							  			 a.",getVariables(flmatobj1)$colIdColumn,",
							  			 a.",getVariables(flmatobj1)$valueColumn,", 
							  			 ROW_NUMBER() OVER (ORDER BY a.",getVariables(flmatobj1)$colIdColumn,",
							  			 							 a.",getVariables(flmatobj1)$rowIdColumn,") AS ROW_NUM  
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
			sqlSendUpdate(getConnection(flmatobj1),
					 paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
							" WITH Z(MATRIX_ID,ROW_ID,COL_ID,CELL_VAL,ROW_NUM) 
							  AS (SELECT a.",flmatobj1@matrix_id_colname,",
							  			 a.",getVariables(flmatobj1)$rowIdColumn,",
							  			 a.",getVariables(flmatobj1)$colIdColumn,",
							  			 a.",getVariables(flmatobj1)$valueColumn,", 
							  			 ROW_NUMBER() OVER (ORDER BY a.",getVariables(flmatobj1)$colIdColumn,",
							  			 							 a.",getVariables(flmatobj1)$rowIdColumn,") AS ROW_NUM  
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
			 connection = getConnection(flmatobj1), 
			 database = getOption("ResultDatabaseFL"), 
			 table_name = getOption("ResultMatrixTableFL"), 
			 matrix_id_value = max_matrix_id_value - 1, 
			 matrix_id_colname = "MATRIX_ID", 
			 row_id_colname = "rowIdColumn", 
			 col_id_colname = "colIdColumn", 
			 cell_val_colname = "valueColumn", 
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
		obj2 <- as.FLVector(x,getConnection(obj1))
		obj2 + obj1
	}

	else if(class(obj1)=="FLSparseMatrix")
	{
		obj2 <- as.FLVector(x,getConnection(obj1))
		obj1+obj2
	}

	else if(class(obj1)=="FLVector")
	{
		obj2 <- as.FLVector(x,getConnection(obj1))
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
				flag2Check(getConnection(flmatobj1))
				sqlSendUpdate(getConnection(flmatobj1),
						 paste0(" INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultSparseMatrixTableFL"),
								" SELECT DISTINCT ",max_Sparsematrix_id_value,",
										 a.",getVariables(flmatobj1)$rowIdColumn,",
										 a.",getVariables(flmatobj1)$colIdColumn,",
										 a.",getVariables(flmatobj1)$valueColumn," 
								  FROM ",remoteTable(flmatobj1)," a 
								  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,
			            		" except ",
			            		"SELECT ",max_Sparsematrix_id_value,",
			            				a.",getVariables(flmatobj1)$rowIdColumn,",
			            				a.",getVariables(flmatobj1)$colIdColumn,",
			            				a.",getVariables(flmatobj1)$valueColumn,
			            		" FROM ",remoteTable(flmatobj1)," a, 
			            			   ",remoteTable(flmatobj2)," b 
			            		  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
			            		  AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
			            		  AND a.",getVariables(flmatobj1)$rowIdColumn," = b.",getVariables(flmatobj2)$rowIdColumn," 
			            		  AND a.",getVariables(flmatobj1)$colIdColumn,"=b.",getVariables(flmatobj2)$colIdColumn,
			            		" UNION ALL ",
			            		"SELECT DISTINCT ",max_Sparsematrix_id_value,",
			            				b.",getVariables(flmatobj2)$rowIdColumn,",
			            				b.",getVariables(flmatobj2)$colIdColumn,",
			            				b.",getVariables(flmatobj2)$valueColumn,
			            		" FROM ",remoteTable(flmatobj2)," b 
			            		  WHERE b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value,
			            		" except ",
			            		" SELECT ",max_Sparsematrix_id_value,",
			            				  b.",getVariables(flmatobj2)$rowIdColumn,",
			            				  b.",getVariables(flmatobj2)$colIdColumn,",
			            				  b.",getVariables(flmatobj2)$valueColumn,
			            		" FROM ",remoteTable(flmatobj1)," a,",
			            				 remoteTable(flmatobj2)," b 
			            		  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
			            		  AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
			            		  AND a.",getVariables(flmatobj1)$rowIdColumn," = b.",getVariables(flmatobj2)$rowIdColumn," 
			            		  AND a.",getVariables(flmatobj1)$colIdColumn,"=b.",getVariables(flmatobj2)$colIdColumn,
			            		" UNION ALL ",
			            		" SELECT ",max_Sparsematrix_id_value,",
			            				 a.",getVariables(flmatobj1)$rowIdColumn,",
			            				 a.",getVariables(flmatobj1)$colIdColumn,",
			            				 a.",getVariables(flmatobj1)$valueColumn,"+b.",getVariables(flmatobj2)$valueColumn,
			            		" FROM ",remoteTable(flmatobj1)," a, ",
			            				 remoteTable(flmatobj2)," b 
			            		  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
			            		  AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
			            		  AND a.",getVariables(flmatobj1)$rowIdColumn," = b.",getVariables(flmatobj2)$rowIdColumn," 
			            		  AND a.",getVariables(flmatobj1)$colIdColumn," =b.",getVariables(flmatobj2)$colIdColumn))	
				
				max_Sparsematrix_id_value <<- max_Sparsematrix_id_value + 1
				new("FLSparseMatrix", 
					 connection = getConnection(flmatobj1), 
					 database = getOption("ResultDatabaseFL"), 
					 table_name = getOption("ResultSparseMatrixTableFL"), 
					 matrix_id_value = max_Sparsematrix_id_value - 1, 
					 matrix_id_colname = "MATRIX_ID", 
					 row_id_colname = "rowIdColumn", 
					 col_id_colname = "colIdColumn", 
					 cell_val_colname = "valueColumn", 
					 nrow = nrow1, 
					 ncol = ncol1, 
					 dimnames= list(c(),c()))
			}
			else
			{
				flag1Check(getConnection(flmatobj2))
				sqlSendUpdate(getConnection(flmatobj2),
						 paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
								" SELECT DISTINCT ",max_matrix_id_value,",
										 b.",getVariables(flmatobj2)$rowIdColumn,",
										 b.",getVariables(flmatobj2)$colIdColumn,",
										 b.",getVariables(flmatobj2)$valueColumn,
								" FROM ",remoteTable(flmatobj2)," b 
								  WHERE b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value,
			            		" except ",
					            "SELECT ",max_matrix_id_value,",
					            		b.",getVariables(flmatobj2)$rowIdColumn,",
					            		b.",getVariables(flmatobj2)$colIdColumn,",
					            		b.",getVariables(flmatobj2)$valueColumn,
					            " FROM ",remoteTable(flmatobj1)," a, ",
					            		 remoteTable(flmatobj2)," b 
					              WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,"
					              AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
					              AND b.",getVariables(flmatobj2)$rowIdColumn ," = a.",getVariables(flmatobj1)$rowIdColumn," 
					              AND b.",getVariables(flmatobj2)$colIdColumn,"=a.",getVariables(flmatobj1)$colIdColumn,
					            " UNION ALL ",
								" SELECT  DISTINCT ",max_matrix_id_value,",
										  a.",getVariables(flmatobj1)$rowIdColumn,",
										  a.",getVariables(flmatobj1)$colIdColumn,",
										  a.",getVariables(flmatobj1)$valueColumn,"+b.",getVariables(flmatobj2)$valueColumn,
								" FROM ",remoteTable(flmatobj1)," a, ",
										 remoteTable(flmatobj2)," b 
								  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
								  AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
								  AND a.",getVariables(flmatobj1)$rowIdColumn," = b.",getVariables(flmatobj2)$rowIdColumn," 
								  AND a.",getVariables(flmatobj1)$colIdColumn," =b.",getVariables(flmatobj2)$colIdColumn))
				max_matrix_id_value <<- max_matrix_id_value + 1
				FLMatrix( 
					 connection = getConnection(flmatobj2), 
					 database = getOption("ResultDatabaseFL"), 
					 table_name = getOption("ResultMatrixTableFL"), 
					 matrix_id_value = max_matrix_id_value - 1, 
					 matrix_id_colname = "MATRIX_ID", 
					 row_id_colname = "rowIdColumn", 
					 col_id_colname = "colIdColumn", 
					 cell_val_colname = "valueColumn", 
					 nrow = nrow1, 
					 ncol = ncol1, 
					 dimnames = list(c(),c()))
			}
		}
		else stop("ERROR: Invalid matrix dimensions for addition")
	}
	else if(is.vector(flmatobj2))
		{
			flmatobj2 <- as.FLVector(flmatobj2,getConnection(flmatobj1))
			flmatobj1+flmatobj2
		}
	else if(is.matrix(flmatobj2))
		{
			flmatobj2 <- as.FLMatrix(flmatobj2,getConnection(flmatobj1))
			flmatobj1+flmatobj2
		}
	else if(class(flmatobj2)=="dgCMatrix")
		{
			flmatobj2 <- as.FLSparseMatrix(flmatobj2,getConnection(flmatobj1))
			flmatobj1+flmatobj2
		}
	else if(is.FLVector(flmatobj2))
		{
			flmatobj2 <- as.FLMatrix(flmatobj2,getConnection(flmatobj1),nr=nrow(flmatobj1),nc=ncol(flmatobj1))
			sqlstr <- paste0(" UPDATE ",flmatobj2@database,".",flmatobj2@table_name,
							 " FROM ( SELECT DISTINCT ",flmatobj2@matrix_id_value," AS mid,
							 				 a.",getVariables(flmatobj1)$rowIdColumn," AS rid,
							 				 a.",getVariables(flmatobj1)$colIdColumn," AS cid,
							 				 a.",getVariables(flmatobj1)$valueColumn,"+b.",getVariables(flmatobj2)$valueColumn," AS cval 
							 		  FROM ",remoteTable(flmatobj1)," a, ",
							 		  		 remoteTable(flmatobj2)," b 
							 		  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
							 		  AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
							 		  AND a.",getVariables(flmatobj1)$rowIdColumn," = b.",getVariables(flmatobj2)$rowIdColumn," 
							 		  AND a.",getVariables(flmatobj1)$colIdColumn," =b.",getVariables(flmatobj2)$colIdColumn,") c ",
							 " SET ",getVariables(flmatobj2)$valueColumn,"= c.cval ",
							 " WHERE ",flmatobj2@matrix_id_colname,"= c.mid 
							   AND ",getVariables(flmatobj2)$rowIdColumn,"= c.rid 
							   AND ",getVariables(flmatobj2)$colIdColumn,"= c.cid")
			sqlSendUpdate(getConnection(flmatobj1),sqlstr)
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
		pObj2 <- as.FLVector(pObj2,getConnection(pObj1))
		pObj2+pObj1
	}
	else if(is.matrix(pObj2))
	{
		pObj2 <- as.FLMatrix(pObj2,getConnection(pObj1))
		pObj2+pObj1
	}
	else if(class(pObj2)=="dgCMatrix")
	{
		pObj2 <- as.FLSparseMatrix(pObj2,getConnection(pObj1))
		pObj2+pObj1
	}
	else if(is.FLSparseMatrix(pObj2))
	{
		pObj2+pObj1
	}
	else if(is.FLVector(pObj2))
	{
		flag3Check(getConnection(pObj1))
		if(length(pObj2) > length(pObj1))
		{
			vTemp <- pObj1
			pObj1 <- pObj2
			pObj2 <- vTemp
		}
		vMinSize <- length(pObj2)

		if(pObj1@isDeep && pObj2@isDeep)
		{
			vSqlStr <-paste0(" INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL"),
					         " SELECT ",max_vector_id_value,",
					         		  a.",pObj1@var_id_name,", 
					         		  CAST(a.",pObj1@col_name,"+b.",pObj2@col_name," AS NUMBER) ",
					         " FROM ",remoteTable(pObj1)," a,",
					         		  remoteTable(pObj2)," b",
					         " WHERE a.",pObj1@obs_id_colname,"=",pObj1@vector_id_value," 
					           AND b.",pObj2@obs_id_colname,"=",pObj2@vector_id_value,
					         " AND a.",pObj1@var_id_name," MOD ",vMinSize," = b.",pObj2@var_id_name," MOD ",vMinSize)

			sqlSendUpdate(getConnection(pObj1),vSqlStr)
		}

		else if(xor(pObj1@isDeep,pObj2@isDeep))
		{
			if(pObj1@isDeep)
			{
				vSqlStr <-paste0(" INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL"),
						         " SELECT ",max_vector_id_value,",
						         		  a.",pObj1@var_id_name,", 
						         		  CAST(a.",pObj1@col_name,"+b.",pObj2@col_name," AS NUMBER) ",
						         " FROM ",remoteTable(pObj1)," a,",
						         		  remoteTable(pObj2)," b",
						         " WHERE a.",pObj1@obs_id_colname,"=",pObj1@vector_id_value,
						         " AND a.",pObj1@var_id_name," MOD ",vMinSize," = b.",pObj2@obs_id_colname," MOD ",vMinSize)

			    sqlSendUpdate(getConnection(pObj1),vSqlStr)
			}
            else
            {
				vSqlStr <-paste0(" INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL"),
						         " SELECT ",max_vector_id_value,",
						         		  a.",pObj1@obs_id_colname,", 
						         		  CAST(a.",pObj1@col_name,"+b.",pObj2@col_name," AS NUMBER) ",
						         " FROM ",remoteTable(pObj1)," a,",
						         		  remoteTable(pObj2)," b",
						         " WHERE b.",pObj2@obs_id_colname,"=",pObj2@vector_id_value,
						         " AND b.",pObj2@var_id_name," MOD ",vMinSize," = a.",pObj1@obs_id_colname," MOD ",vMinSize)

			    sqlSendUpdate(getConnection(pObj1),vSqlStr)
            }
		}

		else if(!pObj1@isDeep && !pObj2@isDeep)
		{
			vSqlStr <-paste0(" INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL"),
					         " SELECT ",max_vector_id_value,",
					         		  a.",pObj1@obs_id_colname,", 
					         		  CAST(a.",pObj1@col_name,"+b.",pObj2@col_name," AS NUMBER) ",
					         " FROM ",remoteTable(pObj1)," a,",
					         		  remoteTable(pObj2)," b",
					         " WHERE b.",pObj2@var_id_name," MOD ",vMinSize," = a.",pObj1@obs_id_colname," MOD ",vMinSize)

			    sqlSendUpdate(getConnection(pObj1),vSqlStr)
		}

			max_vector_id_value <<- max_vector_id_value + 1
			table <- FLTable(connection,
							 getOption("ResultDatabaseFL"),
							 getOption("ResultVectorTableFL"),
							 "VECTOR_ID",
							 "VECTOR_INDEX",
							 "VECTOR_VALUE")

			new("FLVector", 
				 table = table, 
				 col_name = table@variables$valueColumn, 
				 vector_id_value = max_vector_id_value-1, 
				 size = length(pObj1))
			
	}
	else cat("ERROR::Operation Currently Not Supported")
}


`+.dgCMatrix` <- function(x,flmatobj)
{
	if(is.FLSparseMatrix(flmatobj) || is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLSparseMatrix(x,getConnection(flmatobj))
		flmatobj2 + flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLSparseMatrix(x,getConnection(flmatobj))
		flmatobj2 + flmatobj
	}
	else
	{
		op <- .Primitive("+")
		op(x,flmatobj)
	}

}
