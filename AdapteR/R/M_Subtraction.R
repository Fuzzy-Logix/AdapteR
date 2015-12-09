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

#' Subtraction of in-database objects.
#'
#' \code{-} does the subtraction of in-database objects.
#'
#' The subtraction of in-database objects mimics the normal subtraction of R data types.
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param x can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param y can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{-} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 1)
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix - Rvector
#' @export

"-" <- function(x,y)
{
    UseMethod("-", x)
}

`-.default` <- function(vec,flmatobj1)
{
	op <- .Primitive("-")
	op(vec,flmatobj1)
}

`-.matrix` <- function(x,flmatobj1)
{
	if(is.FLMatrix(flmatobj1))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj1@odbc_connection)
		flmatobj2-flmatobj1
	}
	else if(is.FLSparseMatrix(flmatobj1))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj1@odbc_connection)
		flmatobj2-flmatobj1
	}
	else if(is.FLVector(flmatobj1))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj1@table@odbc_connection)
		flmatobj2-flmatobj1
	}
	else 
	{
		op <- .Primitive("-")
		op(x,flmatobj1)
	}
}

`-.FLMatrix` <- function(flmatobj1, flmatobj2)
{
	sqlSendUpdate(flmatobj1@odbc_connection,
			 paste("DATABASE", flmatobj1@db_name,";
			 		SET ROLE ALL;"))
	nrow1 <- nrow(flmatobj1)
	ncol1 <- ncol(flmatobj1)
	if(is.FLMatrix(flmatobj2))
	{
		checkSameDims(flmatobj1,flmatobj2)
		flag1Check(flmatobj1@odbc_connection)
		MID <- max_matrix_id_value
		
		sqlstr <- paste0(" INSERT INTO ",
						getRemoteTableName(result_db_name,result_matrix_table),
				 		" SELECT ",max_matrix_id_value," AS MATRIX_ID ,
				 				a.",flmatobj1@row_id_colname," AS ROW_ID ,
				 				a.",flmatobj1@col_id_colname," AS COL_ID ,
				 				a.",flmatobj1@cell_val_colname,"-b.",flmatobj2@cell_val_colname," AS CELL_VAL 
				 		  FROM ",remoteTable(flmatobj1)," a,",
				 		  		,remoteTable(flmatobj1)," b ",
				 		  constructWhere(c(constraintsSQL(flmatobj1,"a"),
					 		  	constraintsSQL(flmatobj2,"b"),
					 		  	paste0("a.",flmatobj1@row_id_colname,"=b.",flmatobj2@row_id_colname),
					 		  	paste0("a.",flmatobj1@col_id_colname,"=b.",flmatobj2@col_id_colname)))
				 		)

		# sqlstr <- paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
		# 		 		" SELECT ",max_matrix_id_value," AS MATRIX_ID ,
		# 		 				a.",flmatobj1@row_id_colname," AS ROW_ID ,
		# 		 				a.",flmatobj1@col_id_colname," AS COL_ID ,
		# 		 				a.",flmatobj1@cell_val_colname,"-b.",flmatobj2@cell_val_colname," AS CELL_VAL 
		# 		 		  FROM ",flmatobj1@db_name,".",flmatobj1@matrix_table," a,",
		# 		 		  		 flmatobj2@db_name,".",flmatobj2@matrix_table," b 
		# 		 		  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
		# 		 		  AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
		# 		 		  AND a.",flmatobj1@row_id_colname,"=b.",flmatobj2@row_id_colname," 
		# 		 		  AND a.",flmatobj1@col_id_colname,"=b.",flmatobj2@col_id_colname))
		
		sqlSendUpdate(flmatobj1@odbc_connection,sqlstr)
		max_matrix_id_value <<- max_matrix_id_value + 1
		FLMatrix( 
			odbc_connection = flmatobj1@odbc_connection, 
			database = result_db_name, 
			matrix_table = result_matrix_table, 
			matrix_id_value = MID, 
			matrix_id_colname = "MATRIX_ID", 
			row_id_colname = "ROW_ID", 
			col_id_colname = "COL_ID", 
			cell_val_colname = "CELL_VAL")
	}
	else if(is.vector(flmatobj2))
	{
		flmatobj2 <- as.FLVector(flmatobj2,flmatobj1@odbc_connection)
		flmatobj1-flmatobj2
	}
	else if(is.matrix(flmatobj2))
	{
		flmatobj2 <- as.FLMatrix(flmatobj2,flmatobj1@odbc_connection)
		flmatobj1-flmatobj2
	}
	else if(class(flmatobj2)=="dgCMatrix")
	{
		flmatobj2 <- as.FLSparseMatrix(flmatobj2,flmatobj1@odbc_connection)
		flmatobj1-flmatobj2
	}
	else if(is.FLSparseMatrix(flmatobj2))
	{
		if(nrow(flmatobj1) == nrow(flmatobj2) && ncol(flmatobj1) == ncol(flmatobj2))
		{
		        sqlQuery(flmatobj2@odbc_connection,
		        		 paste("DATABASE", flmatobj2@db_name,";
		        		 		SET ROLE ALL;"))
				flag1Check(flmatobj2@odbc_connection)
				vSwap <- flmatobj2
				flmatobj2 <- flmatobj1
				flmatobj1 <- vSwap

				sqlQuery(flmatobj2@odbc_connection,
						 paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								" SELECT DISTINCT ",max_matrix_id_value,",
										 b.",flmatobj2@row_id_colname,",
										 b.",flmatobj2@col_id_colname,",
										 b.",flmatobj2@cell_val_colname,
								" FROM ",flmatobj2@db_name,".",flmatobj2@matrix_table," b 
								  WHERE b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value,
					            " except ",
					            " SELECT ",max_matrix_id_value,",
					            		b.",flmatobj2@row_id_colname,",
					            		b.",flmatobj2@col_id_colname,",
					            		b.",flmatobj2@cell_val_colname,
					            " FROM ",flmatobj1@db_name,".",flmatobj1@matrix_table," a, ",
					            		 flmatobj2@db_name,".",flmatobj2@matrix_table," b 
					              WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
					              AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
					              AND b.",flmatobj2@row_id_colname," = a.",flmatobj1@row_id_colname," 
					              AND b.",flmatobj2@col_id_colname,"=a.",flmatobj1@col_id_colname,
					            " UNION ALL ",
								"SELECT DISTINCT ",max_matrix_id_value,",
										a.",flmatobj1@row_id_colname,",
										a.",flmatobj1@col_id_colname,",
										b.",flmatobj2@cell_val_colname,"-a.",
					            		flmatobj1@cell_val_colname,
					            " FROM ",flmatobj1@db_name,".",flmatobj1@matrix_table," a, ",
					            		 flmatobj2@db_name,".",flmatobj2@matrix_table," b 
					              WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
					              AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
					              AND a.",flmatobj1@row_id_colname," = b.",flmatobj2@row_id_colname," 
					              AND a.",flmatobj1@col_id_colname," =b.",flmatobj2@col_id_colname))
				
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
		else stop("incompatible dimensions")
	}
	else if(is.FLVector(flmatobj2))
	{
		flag1Check(flmatobj2@odbc_connection)

		if(!flmatobj2@table@isDeep)
		{
			sqlQuery(flmatobj1@odbc_connection,
					 paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
							" WITH Z(MATRIX_ID,ROW_ID,COL_ID,CELL_VAL,ROW_NUM) 
							  AS (SELECT a.",flmatobj1@matrix_id_colname,",
							  			 a.",flmatobj1@row_id_colname,",
							  			 a.",flmatobj1@col_id_colname,",
							  			 a.",flmatobj1@cell_val_colname,", 
							  			 ROW_NUMBER() OVER (ORDER BY a.",flmatobj1@col_id_colname,",
							  			 							 a.",flmatobj1@row_id_colname,") AS ROW_NUM  
			        		  FROM ",flmatobj1@matrix_table," a 
		     			      WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,") 
					          SELECT ",max_matrix_id_value,",
					          		 Z.ROW_ID,
					          		 Z.COL_ID,
					          		 Z.CELL_VAL-b.",flmatobj2@col_name,
					         "FROM ",flmatobj2@table@db_name,".",flmatobj2@table@table_name," b,
					         		 Z 
					          WHERE Z.ROW_NUM MOD ",flmatobj2@size," = b.",flmatobj2@table@primary_key," MOD ",flmatobj2@size))
		}
		else
		{
			sqlQuery(flmatobj1@odbc_connection,
					 paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
							" WITH Z(MATRIX_ID,ROW_ID,COL_ID,CELL_VAL,ROW_NUM) 
							  AS (SELECT a.",flmatobj1@matrix_id_colname,",
							  			 a.",flmatobj1@row_id_colname,",
							  			 a.",flmatobj1@col_id_colname,",
							  			 a.",flmatobj1@cell_val_colname,", 
							  			 ROW_NUMBER() OVER (ORDER BY a.",flmatobj1@col_id_colname,",
							  			 							 a.",flmatobj1@row_id_colname,") AS ROW_NUM  
							       FROM ",flmatobj1@matrix_table," a 
							       WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,") 
					         SELECT ",max_matrix_id_value,",
					         		Z.ROW_ID,
					         		Z.COL_ID,
					         		Z.CELL_VAL-b.",flmatobj2@col_name,
					         " FROM ",flmatobj2@table@db_name,".",flmatobj2@table@table_name," b,
					         		 Z 
					          WHERE Z.ROW_NUM MOD ",flmatobj2@size," = b.",flmatobj2@table@var_id_name," MOD ",flmatobj2@size,
					          " AND b.",flmatobj2@table@primary_key,"=",flmatobj2@vector_id_value))
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

`-.numeric` <- function(x,obj1)
{	
	if(missing(obj1))
	{
		op <- .Primitive("-")
		return(op(x))
	}
	if(is.FLMatrix(obj1))
	{
		sqlQuery(obj1@odbc_connection,
				 paste("DATABASE", obj1@db_name,";
				 		SET ROLE ALL;"))
		obj2 <- as.FLVector(x,obj1@odbc_connection)
		obj2 - obj1
	}
	else if(class(obj1)=="FLSparseMatrix")
	{
		sqlQuery(obj1@odbc_connection,
				 paste("DATABASE", obj1@db_name,";
				 		SET ROLE ALL;"))
		obj2 <- as.FLVector(x,obj1@odbc_connection)
		obj2-obj1
	}
	else if(class(obj1)=="FLVector")
	{
		sqlQuery(obj1@table@odbc_connection,
				 paste("DATABASE", obj1@table@db_name,";
				 		SET ROLE ALL;"))
		obj2 <- as.FLVector(x,obj1@table@odbc_connection)
		obj2-obj1
	}
	else
	{
		op <- .Primitive("-")
		op(x,obj1)
	}
}

`-.FLSparseMatrix` <- function(flmatobj1, flmatobj2)
{
	sqlQuery(flmatobj1@odbc_connection,
			 paste("DATABASE", flmatobj1@db_name,";
			 		SET ROLE ALL;"))
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
				flag2Check(flmatobj2@odbc_connection)

				sqlQuery(flmatobj1@odbc_connection,
						 paste0(" INSERT INTO ",result_db_name,".",result_Sparsematrix_table,
								" SELECT DISTINCT ",max_Sparsematrix_id_value,",
										 a.",flmatobj1@row_id_colname,",
										 a.",flmatobj1@col_id_colname,",
										 a.",flmatobj1@cell_val_colname,
								" FROM ",flmatobj1@db_name,".",flmatobj1@matrix_table," a 
								  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,
				            " except ",
				            "SELECT ",max_Sparsematrix_id_value,",
				            		a.",flmatobj1@row_id_colname,",
				            		a.",flmatobj1@col_id_colname,",
				            		a.",flmatobj1@cell_val_colname,
				            " FROM ",flmatobj1@db_name,".",flmatobj1@matrix_table," a, ",
				            		 flmatobj2@db_name,".",flmatobj2@matrix_table," b 
				              WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
				              AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
				              AND a.",flmatobj1@row_id_colname," = b.",flmatobj2@row_id_colname," 
				              AND a.",flmatobj1@col_id_colname,"=b.",flmatobj2@col_id_colname,
				            " UNION ALL ",
				            "SELECT DISTINCT ",max_Sparsematrix_id_value,",
				            		b.",flmatobj2@row_id_colname,",
				            		b.",flmatobj2@col_id_colname,",
				            		b.",flmatobj2@cell_val_colname,
				            " FROM ",flmatobj2@db_name,".",flmatobj2@matrix_table," b 
				              WHERE b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value,
				            " except ",
				            "SELECT ",max_Sparsematrix_id_value,",
				            		b.",flmatobj2@row_id_colname,",
				            		b.",flmatobj2@col_id_colname,",
				            		b.",flmatobj2@cell_val_colname,
				            " FROM ",flmatobj1@db_name,".",flmatobj1@matrix_table," a, ",
				            		 flmatobj2@db_name,".",flmatobj2@matrix_table," b 
				              WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
				              AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
				              AND a.",flmatobj1@row_id_colname," = b.",flmatobj2@row_id_colname," 
				              AND a.",flmatobj1@col_id_colname,"=b.",flmatobj2@col_id_colname,
				            " UNION ALL ",
				            " SELECT ",max_Sparsematrix_id_value,",
				            		a.",flmatobj1@row_id_colname,",
				            		a.",flmatobj1@col_id_colname,",
				            		a.",flmatobj1@cell_val_colname,"-b.",
				            		flmatobj2@cell_val_colname,
				            " FROM ",flmatobj1@db_name,".",flmatobj1@matrix_table," a, ",
				            		 flmatobj2@db_name,".",flmatobj2@matrix_table," b 
				              WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
				              AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
				              AND a.",flmatobj1@row_id_colname," = b.",flmatobj2@row_id_colname," 
				              AND a.",flmatobj1@col_id_colname," =b.",flmatobj2@col_id_colname))	
				
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
				sqlQuery(flmatobj2@odbc_connection, paste("DATABASE", flmatobj2@db_name))
				
				flag1Check(flmatobj2@odbc_connection)

				sqlQuery(flmatobj2@odbc_connection,
						 paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								" SELECT DISTINCT ",max_matrix_id_value,",
										 b.",flmatobj2@row_id_colname,",
										 b.",flmatobj2@col_id_colname,",
										 b.",flmatobj2@cell_val_colname,"*(-1) 
								  FROM ",flmatobj2@db_name,".",flmatobj2@matrix_table," b 
								  WHERE b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value,
					            " except ",
					            "SELECT ",max_matrix_id_value,",
					            		b.",flmatobj2@row_id_colname,",
					            		b.",flmatobj2@col_id_colname,",
					            		b.",flmatobj2@cell_val_colname,"*(-1) 
					             FROM ",flmatobj1@db_name,".",flmatobj1@matrix_table," a, ",
					             		flmatobj2@db_name,".",flmatobj2@matrix_table," b 
					             WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
					             AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
					             AND b.",flmatobj2@row_id_colname," = a.",flmatobj1@row_id_colname," 
					             AND b.",flmatobj2@col_id_colname,"=a.",flmatobj1@col_id_colname,
					            " union all ",
								"SELECT DISTINCT ",max_matrix_id_value,",
										a.",flmatobj1@row_id_colname,",
										a.",flmatobj1@col_id_colname,",
										a.",flmatobj1@cell_val_colname,"-b.",
					            		flmatobj2@cell_val_colname,
					            " FROM ",flmatobj1@db_name,".",flmatobj1@matrix_table," a, ",
					            		 flmatobj2@db_name,".",flmatobj2@matrix_table," b 
					              WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
					              AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
					              AND a.",flmatobj1@row_id_colname," = b.",flmatobj2@row_id_colname," 
					              AND a.",flmatobj1@col_id_colname," =b.",flmatobj2@col_id_colname))
				
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
		else stop("ERROR: Invalid dimensions")
	}
	else if(is.vector(flmatobj2))
		{
			flmatobj2 <- as.FLMatrix(matrix(flmatobj2,nrow1,ncol1),flmatobj1@odbc_connection)
			flmatobj1-flmatobj2
		}
	else if(is.matrix(flmatobj2))
		{
			flmatobj2 <- as.FLMatrix(flmatobj2,flmatobj1@odbc_connection)
			flmatobj1-flmatobj2
		}
	else if(class(flmatobj2)=="dgCMatrix")
		{
			flmatobj2 <- as.FLSparseMatrix(flmatobj2,flmatobj1@odbc_connection)
			flmatobj1-flmatobj2
		}
	else if(is.FLVector(flmatobj2))
		{
			flmatobj2 <- as.FLMatrix(flmatobj2,flmatobj1@odbc_connection,nr=nrow(flmatobj1),nc=ncol(flmatobj1))

			sqlstr <-paste0(" UPDATE ",flmatobj2@db_name,".",flmatobj2@matrix_table,
					        " FROM ( SELECT DISTINCT ",flmatobj2@matrix_id_value," AS mid,
					        				a.",flmatobj2@row_id_colname," AS rid,
					        				a.",flmatobj2@col_id_colname," AS cid,
					        				a.",flmatobj2@cell_val_colname,"*(-1) AS cval 
					        		 FROM ",flmatobj2@db_name,".",flmatobj2@matrix_table," a 
					        		 WHERE a.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value,") c ",
					        " SET ",flmatobj2@cell_val_colname,"= c.cval ",
					        " WHERE ",flmatobj2@matrix_id_colname,"= c.mid 
					          AND ",flmatobj2@row_id_colname,"= c.rid 
					          AND ",flmatobj2@col_id_colname,"= c.cid")
			sqlQuery(flmatobj1@odbc_connection,sqlstr)

			sqlstr <-paste0(" UPDATE ",flmatobj2@db_name,".",flmatobj2@matrix_table,
							" FROM ( SELECT DISTINCT ",flmatobj2@matrix_id_value," AS mid,
											a.",flmatobj1@row_id_colname," AS rid,
											a.",flmatobj1@col_id_colname," AS cid,
											a.",flmatobj1@cell_val_colname,"+b.",
						            			flmatobj2@cell_val_colname," AS cval 
						             FROM ",flmatobj1@db_name,".",flmatobj1@matrix_table," a, ",
						             		flmatobj2@db_name,".",flmatobj2@matrix_table," b 
						             WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
						             AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
						             AND a.",flmatobj1@row_id_colname," = b.",flmatobj2@row_id_colname," 
						             AND a.",flmatobj1@col_id_colname," =b.",flmatobj2@col_id_colname,") c ",
							" SET ",flmatobj2@cell_val_colname,"= c.cval ",
							" WHERE ",flmatobj2@matrix_id_colname,"= c.mid 
							  AND ",flmatobj2@row_id_colname,"= c.rid 
							  AND ",flmatobj2@col_id_colname,"= c.cid")
			sqlQuery(flmatobj1@odbc_connection,sqlstr)

			return(flmatobj2)
		}
	else stop("Operation Currently Not Supported")
}

`-.FLVector` <- function(pObj1,pObj2)
{
	vNrow1 <- pObj1@size
	sqlQuery(pObj1@table@odbc_connection,
			 paste("DATABASE", pObj1@table@db_name,";
			 		SET ROLE ALL;"))
	if(is.FLMatrix(pObj2))
	{
		flag1Check(pObj1@table@odbc_connection)
		flmatobj1 <- pObj2
		flmatobj2 <- pObj1
		if(!flmatobj2@table@isDeep)
		{
			sqlQuery(flmatobj1@odbc_connection,
					 paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
							" WITH Z(MATRIX_ID,ROW_ID,COL_ID,CELL_VAL,ROW_NUM) 
							  AS (SELECT a.",flmatobj1@matrix_id_colname,",
							  			 a.",flmatobj1@row_id_colname,",
							  			 a.",flmatobj1@col_id_colname,",
							  			 a.",flmatobj1@cell_val_colname,", 
							  			 ROW_NUMBER() OVER (ORDER BY a.",flmatobj1@col_id_colname,",
							  			 							 a.",flmatobj1@row_id_colname,") AS ROW_NUM  
						          FROM ",flmatobj1@matrix_table," a 
						          WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,") 
					         SELECT ",max_matrix_id_value,",
					         		Z.ROW_ID,
					         		Z.COL_ID,
					         		b.",flmatobj2@col_name,"-Z.CELL_VAL",
					         " FROM ",flmatobj2@table@db_name,".",flmatobj2@table@table_name," b,
					         		 Z 
					          WHERE Z.ROW_NUM MOD ",flmatobj2@size," = b.",flmatobj2@table@primary_key," MOD ",flmatobj2@size))
		}
		else
		{
			sqlQuery(flmatobj1@odbc_connection,
					 paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
							" WITH Z(MATRIX_ID,ROW_ID,COL_ID,CELL_VAL,ROW_NUM) 
							  AS (SELECT a.",flmatobj1@matrix_id_colname,",
							  			 a.",flmatobj1@row_id_colname,",
							  			 a.",flmatobj1@col_id_colname,",
							  			 a.",flmatobj1@cell_val_colname,", 
							  			 ROW_NUMBER() OVER (ORDER BY a.",flmatobj1@col_id_colname,",
							  			 							 a.",flmatobj1@row_id_colname,") AS ROW_NUM  
						          FROM ",flmatobj1@matrix_table," a 
						          WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,") 
				         	SELECT ",max_matrix_id_value,",
				         			Z.ROW_ID,
				         			Z.COL_ID,
				         			b.",flmatobj2@col_name,"-Z.CELL_VAL",
				         	" FROM ",flmatobj2@table@db_name,".",flmatobj2@table@table_name," b,
				         			Z 
				          	WHERE Z.ROW_NUM MOD ",flmatobj2@size," = b.",flmatobj2@table@var_id_name," MOD ",flmatobj2@size,
				          	" AND b.",flmatobj2@table@primary_key,"=",flmatobj2@vector_id_value))
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
			nrow = nrow(flmatobj1), 
			ncol = ncol(flmatobj1))
	}
	else if(is.vector(pObj2))
	{
		pObj2 <- as.FLVector(pObj2,pObj1@table@odbc_connection)
		pObj1-pObj2
	}
	else if(is.matrix(pObj2))
	{
		pObj2 <- as.FLMatrix(pObj2,pObj1@table@odbc_connection)
		pObj1-pObj2
	}
	else if(class(pObj2)=="dgCMatrix")
	{
		pObj2 <- as.FLSparseMatrix(pObj2,pObj1@table@odbc_connection)
		pObj1-pObj2
	}
	else if(is.FLSparseMatrix(pObj2))
	{
		flmatobj1 <- pObj2
		flmatobj2 <- as.FLMatrix(pObj1,flmatobj1@odbc_connection,nr=nrow(flmatobj1),nc=ncol(flmatobj1))
			sqlstr <-paste0(" UPDATE ",flmatobj2@db_name,".",flmatobj2@matrix_table,
					        " FROM ( SELECT DISTINCT ",flmatobj2@matrix_id_value," AS mid,
					        				a.",flmatobj1@row_id_colname," AS rid,
					        				a.",flmatobj1@col_id_colname," AS cid,
					        				b.",flmatobj2@cell_val_colname,"-a.",flmatobj1@cell_val_colname," AS cval 
					        		 FROM ",flmatobj1@db_name,".",flmatobj1@matrix_table," a, ",
					        		 		flmatobj2@db_name,".",flmatobj2@matrix_table," b 
					        		 WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
					        		 AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
					        		 AND a.",flmatobj1@row_id_colname," = b.",flmatobj2@row_id_colname," 
					        		 AND a.",flmatobj1@col_id_colname," =b.",flmatobj2@col_id_colname,") c ",
							" SET ",flmatobj2@cell_val_colname,"= c.cval ",
							" WHERE ",flmatobj2@matrix_id_colname,"= c.mid 
							  AND ",flmatobj2@row_id_colname,"= c.rid 
							  AND ",flmatobj2@col_id_colname,"= c.cid")
			sqlQuery(flmatobj1@odbc_connection,sqlstr)
			return(flmatobj2)
	}
	else if(is.FLVector(pObj2))
	{
		flag3Check(pObj2@odbc_connection)
		if(pObj2@size > pObj1@size)
		{
			if(pObj2@table@isDeep)
			vPrimaryKey <- paste0(",b.",pObj2@table@var_id_name)
			else
			vPrimaryKey <- paste0(",b.",pObj2@table@primary_key)
			vMinSize <- pObj1@size
		}
		else
		{
			if(pObj1@table@isDeep)
			vPrimaryKey <- paste0(",a.",pObj1@table@var_id_name)
			else
			vPrimaryKey <- paste0(",a.",pObj1@table@primary_key)
			vMinSize <- pObj2@size
		}
		

		if(pObj1@table@isDeep && pObj2@table@isDeep)
		{
			vSqlStr <-paste0(" INSERT INTO ",result_db_name,".",result_vector_table,
					         " SELECT ",max_vector_id_value,
					         			vPrimaryKey,", 
					         			CAST(a.",pObj1@col_name,"-b.",pObj2@col_name," AS NUMBER) ",
					         " FROM ",pObj1@table@db_name,".",pObj1@table@table_name," a,",
					         		  pObj2@table@db_name,".",pObj2@table@table_name," b",
					         " WHERE a.",pObj1@table@primary_key,"=",pObj1@vector_id_value," 
					           AND b.",pObj2@table@primary_key,"=",pObj2@vector_id_value,
					         " AND a.",pObj1@table@var_id_name," MOD ",vMinSize," = b.",pObj2@table@var_id_name," MOD ",vMinSize)

			sqlQuery(pObj1@table@odbc_connection,vSqlStr)
		}

		else if(xor(pObj1@table@isDeep,pObj2@table@isDeep))
		{
			if(pObj1@table@isDeep)
			{
				vSqlStr <-paste0(" INSERT INTO ",result_db_name,".",result_vector_table,
						         " SELECT ",max_vector_id_value,
						         			vPrimaryKey,", 
						         			CAST(a.",pObj1@col_name,"-b.",pObj2@col_name," AS NUMBER) ",
						         " FROM ",pObj1@table@db_name,".",pObj1@table@table_name," a,",
						         		  pObj2@table@db_name,".",pObj2@table@table_name," b",
						         " WHERE a.",pObj1@table@primary_key,"=",pObj1@vector_id_value,
						         " AND a.",pObj1@table@var_id_name," MOD ",vMinSize," = b.",pObj2@table@primary_key," MOD ",vMinSize)

			    sqlQuery(pObj1@table@odbc_connection,vSqlStr)
			}
            else
            {
				vSqlStr <-paste0(" INSERT INTO ",result_db_name,".",result_vector_table,
						         " SELECT ",max_vector_id_value,
						         			vPrimaryKey,", 
						         			CAST(a.",pObj1@col_name,"-b.",pObj2@col_name," AS NUMBER) ",
						         " FROM ",pObj1@table@db_name,".",pObj1@table@table_name," a,",pObj2@table@db_name,".",pObj2@table@table_name," b",
						         " WHERE b.",pObj2@table@primary_key,"=",pObj2@vector_id_value,
						         " AND b.",pObj2@table@var_id_name," MOD ",vMinSize," = a.",pObj1@table@primary_key," MOD ",vMinSize)

			    sqlQuery(pObj1@table@odbc_connection,vSqlStr)
            }
		}

		else if(!pObj1@table@isDeep && !pObj2@table@isDeep)
		{
			vSqlStr <-paste0(" INSERT INTO ",result_db_name,".",result_vector_table,
					         " SELECT ",max_vector_id_value,
					         			vPrimaryKey,", 
					         			CAST(a.",pObj1@col_name,"-b.",pObj2@col_name," AS NUMBER) ",
					         " FROM ",pObj1@table@db_name,".",pObj1@table@table_name," a,",
					         		  pObj2@table@db_name,".",pObj2@table@table_name," b",
					         " WHERE b.",pObj2@table@var_id_name," MOD ",vMinSize," = a.",pObj1@table@primary_key," MOD ",vMinSize)

			    sqlQuery(pObj1@table@odbc_connection,vSqlStr)
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
				col_name = table@num_val_name, 
				vector_id_value = max_vector_id_value-1, 
				size = pObj1@size)
	}
	else cat("ERROR::Operation Currently Not Supported")
}


`-.dgCMatrix` <- function(x,flmatobj)
{
	if(is.FLSparseMatrix(flmatobj) || is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLSparseMatrix(x,flmatobj@odbc_connection)
		flmatobj2 - flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLSparseMatrix(x,flmatobj@table@odbc_connection)
		flmatobj2 - flmatobj
	}
	else
	{
		op <- .Primitive("-")
		op(x,flmatobj)
	}

}
