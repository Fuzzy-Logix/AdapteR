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

#' Element-wise Division of in-database objects.
#'
#' \code{/} does the Element-wise Division of in-database objects.
#'
#' The Element-wise Division of in-database objects mimics the \code{/} of R data types.
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param x can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param y can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{/} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @section Constraints: division by 0 gives inf in R, but is not supported for 
#' in-database objects
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 1)
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix / Rvector
#' @export

"/" <- function(x,y)
{
    UseMethod("/", x)
}

`/.default` <- function(vec,flmatobj1)
{
	op <- .Primitive("/")
	op(vec,flmatobj1)
}

`/.matrix` <- function(x,flmatobj1)
{
	if(is.FLMatrix(flmatobj1))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj1@odbc_connection)
		flmatobj2/flmatobj1
	}
	else if(is.FLSparseMatrix(flmatobj1))
	{
		stop("division by zero not supported currently")
	}
	else if(is.FLVector(flmatobj1))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj1@table@odbc_connection)
		flmatobj2/flmatobj1
	}
	else 
	{
		op <- .Primitive("/")
		op(x,flmatobj1)
	}
}

`/.FLMatrix` <- function(flmatobj1, flmatobj2)
{
	nrow1 <- nrow(flmatobj1)
	ncol1 <- ncol(flmatobj1)
	if(is.FLMatrix(flmatobj2))
	{
		if(nrow(flmatobj1) == nrow(flmatobj2) && ncol(flmatobj1) == ncol(flmatobj2))
		{
			flag1Check(flmatobj1@odbc_connection)
			t<-sqlSendUpdate(flmatobj1@odbc_connection,
						 paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						 		" SELECT ",max_matrix_id_value," AS MATRIX_ID ,
						 				 a.",flmatobj1@row_id_colname," AS ROW_ID ,
						 				 a.",flmatobj1@col_id_colname," AS COL_ID ,
						 				 a.",flmatobj1@cell_val_colname,"/b.",flmatobj2@cell_val_colname," AS CELL_VAL 
						 		  FROM ",remoteTable(flmatobj1)," a,",
						 		  		 remoteTable(flmatobj2)," b 
						 		  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
						 		  AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
						 		  AND a.",flmatobj1@row_id_colname,"=b.",flmatobj2@row_id_colname," 
						 		  AND a.",flmatobj1@col_id_colname,"=b.",flmatobj2@col_id_colname))
			if(length(t)!=0) { stop("division by zero not supported currently") }
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
		else stop("ERROR: Invalid matrix dimensions")
	}
	else if(is.vector(flmatobj2))
	{
		flmatobj2 <- as.FLVector(flmatobj2,flmatobj1@odbc_connection)
		flmatobj1/flmatobj2
	}
	else if(is.matrix(flmatobj2))
	{
		flmatobj2 <- as.FLMatrix(flmatobj2,flmatobj1@odbc_connection)
		flmatobj1/flmatobj2
	}
	else if(class(flmatobj2)=="dgCMatrix")
	{
		stop("division by zero not supported currently")
	}
	else if(is.FLSparseMatrix(flmatobj2))
	{
		stop("division by zero not supported currently")
	}
	else if(is.FLVector(flmatobj2))
	{
		flag1Check(flmatobj1@odbc_connection)
		if(!flmatobj2@table@isDeep)
		{
			t<-sqlSendUpdate(flmatobj1@odbc_connection,
						 paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								" WITH Z(MATRIX_ID,ROW_ID,COL_ID,CELL_VAL,ROW_NUM) 
								  AS (SELECT a.",flmatobj1@matrix_id_colname,",
								  			 a.",flmatobj1@row_id_colname,",
								  			 a.",flmatobj1@col_id_colname,",
								  			 a.",flmatobj1@cell_val_colname,", 
								  			 ROW_NUMBER() OVER (ORDER BY a.",flmatobj1@col_id_colname,",
								  			 							 a.",flmatobj1@row_id_colname,") AS ROW_NUM  
							         FROM ",remoteTable(flmatobj1)," a 
							         WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,") 
						         SELECT ",max_matrix_id_value,",
						         		Z.ROW_ID,
						         		Z.COL_ID,
						         		Z.CELL_VAL/b.",
						         		flmatobj2@col_name,
						        "FROM ",remoteTable(flmatobj2@table)," b,
						        		Z 
						        WHERE Z.ROW_NUM MOD ",flmatobj2@size," = b.",flmatobj2@table@primary_key," MOD ",flmatobj2@size))
		}		
		else
		{
			t<-sqlSendUpdate(flmatobj1@odbc_connection,
						 paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								" WITH Z(MATRIX_ID,ROW_ID,COL_ID,CELL_VAL,ROW_NUM) 
								AS (SELECT a.",flmatobj1@matrix_id_colname,",
										   a.",flmatobj1@row_id_colname,",
										   a.",flmatobj1@col_id_colname,",
										   a.",flmatobj1@cell_val_colname,", 
										   ROW_NUMBER() OVER (ORDER BY a.",flmatobj1@col_id_colname,",
										   							   a.",flmatobj1@row_id_colname,") AS ROW_NUM  
			        				FROM ",remoteTable(flmatobj1)," a 
			        				WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,") 
	         					SELECT ",max_matrix_id_value,",
	         							Z.ROW_ID,
	         							Z.COL_ID,
	         							Z.CELL_VAL/b.",
	         							flmatobj2@col_name,
	         					" FROM ",remoteTable(flmatobj2@table)," b,
	         							Z 
          						WHERE Z.ROW_NUM MOD ",flmatobj2@size," = b.",flmatobj2@table@var_id_name," MOD ",flmatobj2@size,
	          					" AND b.",flmatobj2@table@primary_key,"=",flmatobj2@vector_id_value))
		}
		if(length(t)!=0) { stop("division by zero not supported currently") }
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

`/.numeric` <- function(x,obj1)
{	if(is.FLMatrix(obj1))
	{
		obj2 <- as.FLVector(x,obj1@odbc_connection)
		obj2 / obj1
	}
	else if(class(obj1)=="FLSparseMatrix")
	{
		stop("division by zero not supported currently")
	}
	else if(class(obj1)=="FLVector")
	{
		obj2 <- as.FLVector(x,obj1@table@odbc_connection)
		obj2/obj1
	}
	else
	{
		op <- .Primitive("/")
		op(x,obj1)
	}
}

`/.FLSparseMatrix` <- function(flmatobj1, flmatobj2)
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
				t<-sqlSendUpdate(flmatobj1@odbc_connection,
							paste0(" INSERT INTO ",result_db_name,".",result_Sparsematrix_table,
			            		   " SELECT ",max_Sparsematrix_id_value,",
			            		   			a.",flmatobj1@row_id_colname,",
			            		   			a.",flmatobj1@col_id_colname,",
			            		   			a.",flmatobj1@cell_val_colname,"/b.",
									            flmatobj2@cell_val_colname," 
									  FROM ",remoteTable(flmatobj1)," a, ",
									  		 remoteTable(flmatobj2)," b 
									  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
									  AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
									  AND a.",flmatobj1@row_id_colname," = b.",flmatobj2@row_id_colname," 
									  AND a.",flmatobj1@col_id_colname," =b.",flmatobj2@col_id_colname))
				if(length(t)!=0) { stop("division by zero not supported currently") }
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
				t<-sqlSendUpdate(flmatobj2@odbc_connection,
							paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								   " SELECT DISTINCT ",max_matrix_id_value,",
								   					 b.",flmatobj2@row_id_colname,",
								   					 b.",flmatobj2@col_id_colname," ,
								   					 0 ",
									" FROM ",remoteTable(flmatobj2)," b 
									  WHERE b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value,
						            " except ",
			            			" SELECT ",max_matrix_id_value,",
			            					 b.",flmatobj2@row_id_colname,",
			            					 b.",flmatobj2@col_id_colname," ,
			            					 0 
			            			  FROM ",remoteTable(flmatobj1)," a, ",
			            			  		 remoteTable(flmatobj2)," b 
			            			  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
			            			  AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
			            			  AND b.",flmatobj2@row_id_colname," = a.",flmatobj1@row_id_colname," 
			            			  AND b.",flmatobj2@col_id_colname,"=a.",flmatobj1@col_id_colname,
						            " UNION ALL ",
									" SELECT DISTINCT ",max_matrix_id_value,",
											 a.",flmatobj1@row_id_colname,",
											 a.",flmatobj1@col_id_colname,",
											 a.",flmatobj1@cell_val_colname,"/b.",
		    						         flmatobj2@cell_val_colname," 
		    						  FROM ",remoteTable(flmatobj1)," a, ",
		    						  		 remoteTable(flmatobj2)," b 
		    						  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
		    						  AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
		    						  AND a.",flmatobj1@row_id_colname," = b.",flmatobj2@row_id_colname," 
		    						  AND a.",flmatobj1@col_id_colname," =b.",flmatobj2@col_id_colname))
				
				if(length(t)!=0) { stop("division by zero not supported currently") }
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
		else stop("ERROR: Invalid matrix dimensions")
	}
	else if(is.vector(flmatobj2))
		{
			flmatobj2 <- as.FLVector(flmatobj2,flmatobj1@odbc_connection)
			flmatobj1/flmatobj2
		}
	else if(is.matrix(flmatobj2))
		{
			flmatobj2 <- as.FLMatrix(flmatobj2,flmatobj1@odbc_connection)
			flmatobj1/flmatobj2
		}
	else if(class(flmatobj2)=="dgCMatrix")
		{
			stop("division by zero not supported currently")
		}
	else if(is.FLVector(flmatobj2))
		{
			flag2Check(flmatobj1@odbc_connection)
			if(flmatobj2@table@isDeep)
			{
				t<-sqlSendUpdate(flmatobj1@odbc_connection,
							paste0(" INSERT INTO ",result_db_name,".",result_Sparsematrix_table,
			            		   " SELECT ",max_Sparsematrix_id_value,",
			            		   			a.",flmatobj1@row_id_colname,",
			            		   			a.",flmatobj1@col_id_colname,",
			            		   			a.",flmatobj1@cell_val_colname,"/b.",
			            					flmatobj2@col_name,
			            			" FROM ",remoteTable(flmatobj1)," a, ",
			            					 remoteTable(flmatobj2@table)," b 
			            			  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
			            			  AND b.",flmatobj2@table@primary_key,"=",flmatobj2@vector_id_value," 
			            			  AND (((a.",flmatobj1@col_id_colname,"-1)*",nrow(flmatobj1),")+",
			            			  			 flmatobj1@row_id_colname,") MOD ",flmatobj2@size," = b.",flmatobj2@table@var_id_name," MOD ",flmatobj2@size))
			}
			else
			{
				t<-sqlSendUpdate(flmatobj1@odbc_connection, 
							paste0(" INSERT INTO ",result_db_name,".",result_Sparsematrix_table,
		            			   " SELECT ",max_Sparsematrix_id_value,",
		            			   			a.",flmatobj1@row_id_colname,",
		            			   			a.",flmatobj1@col_id_colname,",
		            			   			a.",flmatobj1@cell_val_colname,"/b.",
		            						flmatobj2@col_name,
		            				" FROM ",remoteTable(flmatobj1)," a, ",
		            						 remoteTable(flmatobj2@table)," b 
		            				  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
		            				  AND (((a.",flmatobj1@col_id_colname,"-1)*",nrow(flmatobj1),")+",
		            						flmatobj1@row_id_colname,") MOD ",flmatobj2@size," = b.",flmatobj2@table@primary_key," MOD ",flmatobj2@size))
			}
			
			if(length(t)!=0) { stop("division by zero not supported currently") }
			max_Sparsematrix_id_value <<- max_Sparsematrix_id_value + 1
			new("FLSparseMatrix", 
				connection = flmatobj1@odbc_connection, 
				database = result_db_name, 
				matrix_table = result_Sparsematrix_table, 
				matrix_id_value = max_Sparsematrix_id_value - 1, 
				matrix_id_colname = "MATRIX_ID", 
				row_id_colname = "ROW_ID", 
				col_id_colname = "COL_ID", 
				cell_val_colname = "CELL_VAL", 
				nrow = nrow(flmatobj1), 
				ncol = ncol(flmatobj1), 
				dimnames= list(c(),c()))
		}
	else stop("Operation Currently Not Supported")
}

`/.FLVector` <- function(pObj1,pObj2)
{
	vNrow1 <- pObj1@size
	if(is.FLMatrix(pObj2))
	{
		flag1Check(flmatobj1@odbc_connection)
		flmatobj2 <- pObj1
		flmatobj1 <- pObj2
		if(!flmatobj2@table@isDeep)
		{
			t<-sqlSendUpdate(flmatobj1@odbc_connection, 
						paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
							   " WITH Z(MATRIX_ID,ROW_ID,COL_ID,CELL_VAL,ROW_NUM) 
							   	 AS (SELECT a.",flmatobj1@matrix_id_colname,",
							   			  a.",flmatobj1@row_id_colname,",
							   			  a.",flmatobj1@col_id_colname,",
							   			  a.",flmatobj1@cell_val_colname,", 
							   			  ROW_NUMBER() OVER (ORDER BY a.",flmatobj1@col_id_colname,",
							   			  							  a.",flmatobj1@row_id_colname,") AS ROW_NUM  
			        			 	 FROM ",remoteTable(flmatobj1)," a 
			        			 	 WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,") 
						         SELECT ",max_matrix_id_value,",
						         		 Z.ROW_ID,
						         		 Z.COL_ID,
						         		 b.",flmatobj2@col_name,"/Z.CELL_VAL",
						         "FROM ",remoteTable(flmatobj2@table)," b,
						         		 Z 
						          WHERE Z.ROW_NUM MOD ",flmatobj2@size," = b.",flmatobj2@table@primary_key," MOD ",flmatobj2@size))
		}
		else
		{
			t<-sqlSendUpdate(flmatobj1@odbc_connection, 
						paste0( " INSERT INTO ",result_db_name,".",result_matrix_table,
								" WITH Z(MATRIX_ID,ROW_ID,COL_ID,CELL_VAL,ROW_NUM) 
								  AS (SELECT a.",flmatobj1@matrix_id_colname,",
								  			 a.",flmatobj1@row_id_colname,",
								  			 a.",flmatobj1@col_id_colname,",
								  			 a.",flmatobj1@cell_val_colname,", 
								  			 ROW_NUMBER() OVER (ORDER BY a.",flmatobj1@col_id_colname,",
								  			 							 a.",flmatobj1@row_id_colname,") AS ROW_NUM  
			        				  FROM ",remoteTable(flmatobj1)," a
			        				  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,") 
						         SELECT ",max_matrix_id_value,",
						         		Z.ROW_ID,
						         		Z.COL_ID,
						         		b.",flmatobj2@col_name,"/Z.CELL_VAL",
						         " FROM ",remoteTable(flmatobj2@table)," b,
						         		 Z 
						          WHERE Z.ROW_NUM MOD ",flmatobj2@size," = b.",flmatobj2@table@var_id_name," MOD ",flmatobj2@size,
						          " AND b.",flmatobj2@table@primary_key,"=",flmatobj2@vector_id_value))
		}
		if(length(t)!=0) { stop("division by zero not supported currently") }
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
		pObj1/pObj2
	}
	else if(is.matrix(pObj2))
	{
		pObj2 <- as.FLMatrix(pObj2,pObj1@table@odbc_connection)
		pObj1/pObj2
	}
	else if(class(pObj2)=="dgCMatrix")
	{
		stop("division by zero not supported currently")
	}
	else if(is.FLSparseMatrix(pObj2))
	{
		stop("division by zero not supported currently")
	}
	else if(is.FLVector(pObj2))
	{
		flag3Check(pObj1@table@odbc_connection)
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
					         " SELECT ",max_vector_id_value,vPrimaryKey,", 
					         			CAST(a.",pObj1@col_name,"/b.",pObj2@col_name," AS NUMBER) ",
					         " FROM ",remoteTable(pObj1@table)," a,",
					         		  remoteTable(pObj2@table)," b",
					         " WHERE a.",pObj1@table@primary_key,"=",pObj1@vector_id_value," 
					           AND b.",pObj2@table@primary_key,"=",pObj2@vector_id_value,
					         " AND a.",pObj1@table@var_id_name," MOD ",vMinSize," = b.",pObj2@table@var_id_name," MOD ",vMinSize)

			t<-sqlSendUpdate(pObj1@table@odbc_connection,vSqlStr)
		}

		else if(xor(pObj1@table@isDeep,pObj2@table@isDeep))
		{
			if(pObj1@table@isDeep)
			{
				vSqlStr <-paste0(" INSERT INTO ",result_db_name,".",result_vector_table,
						         " SELECT ",max_vector_id_value,vPrimaryKey,", 
						         			CAST(a.",pObj1@col_name,"/b.",pObj2@col_name," AS NUMBER) ",
						         " FROM ",remoteTable(pObj1@table)," a,",
						         		  remoteTable(pObj2@table)," b",
						         " WHERE a.",pObj1@table@primary_key,"=",pObj1@vector_id_value,
						         " AND a.",pObj1@table@var_id_name," MOD ",vMinSize," = b.",pObj2@table@primary_key," MOD ",vMinSize)

			    t<-sqlSendUpdate(pObj1@table@odbc_connection,vSqlStr)
			}
            else
            {
				vSqlStr <-paste0(" INSERT INTO ",result_db_name,".",result_vector_table,
						         " SELECT ",max_vector_id_value,vPrimaryKey,", 
						         			CAST(a.",pObj1@col_name,"/b.",pObj2@col_name," AS NUMBER) ",
						         " FROM ",remoteTable(pObj1@table)," a,",
						         		  remoteTable(pObj2@table)," b",
						         " WHERE b.",pObj2@table@primary_key,"=",pObj2@vector_id_value,
						         " AND b.",pObj2@table@var_id_name," MOD ",vMinSize," = a.",pObj1@table@primary_key," MOD ",vMinSize)

			    t<-sqlSendUpdate(pObj1@table@odbc_connection,vSqlStr)
            }
		}

		else if(!pObj1@table@isDeep && !pObj2@table@isDeep)
		{
			vSqlStr <-paste0(" INSERT INTO ",result_db_name,".",result_vector_table,
					         " SELECT ",max_vector_id_value,vPrimaryKey,", 
					         			CAST(a.",pObj1@col_name,"/b.",pObj2@col_name," AS NUMBER) ",
					         " FROM ",remoteTable(pObj1@table)," a,",
					         		  remoteTable(pObj2@table)," b",
					         " WHERE b.",pObj2@table@var_id_name," MOD ",vMinSize," = a.",pObj1@table@primary_key," MOD ",vMinSize)

			    t<-sqlSendUpdate(pObj1@table@odbc_connection,vSqlStr)
		}

			if(length(t)!=0) { stop("division by zero not supported currently") }
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


`/.dgCMatrix` <- function(x,flmatobj)
{
	if(is.FLSparseMatrix(flmatobj) || is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLSparseMatrix(x,flmatobj@odbc_connection)
		flmatobj2 / flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLSparseMatrix(x,flmatobj@table@odbc_connection)
		flmatobj2 / flmatobj
	}
	else
	{
		op <- .Primitive("/")
		op(x,flmatobj)
	}

}
