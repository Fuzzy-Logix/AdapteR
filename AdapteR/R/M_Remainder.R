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

#' remainder of division on in-database objects.
#'
#' \code{\%\%} calculates the remainder of in-database object division.
#'
#' The remainder of in-database objects mimics the normal remainder of R data types.
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param x can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param y can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{\%\%} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @section Constraints: division by 0 gives inf in R, but is not supported for 
#' in-database objects
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 1)
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix %% Rvector
#' @export

"%%" <- function(x,y)
{
    UseMethod("%%", x)
}

`%%.default` <- function(vec,flmatobj1)
{
	op <- .Primitive("%%")
	op(vec,flmatobj1)
}

`%%.matrix` <- function(x,flmatobj1)
{
	if(is.FLMatrix(flmatobj1))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj1))
		flmatobj2%%flmatobj1
	}
	else if(is.FLSparseMatrix(flmatobj1))
	{
		stop("division by zero not supported currently")
	}
	else if(is.FLVector(flmatobj1))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj1))
		flmatobj2%%flmatobj1
	}
	else 
	{
		op <- .Primitive("%%")
		op(x,flmatobj1)
	}
}

`%%.FLMatrix` <- function(flmatobj1, flmatobj2)
{
	sqlQuery(getConnection(flmatobj1),
			 paste("DATABASE", flmatobj1@db_name,";
			 		SET ROLE ALL;"))
	nrow1 <- nrow(flmatobj1)
	ncol1 <- ncol(flmatobj1)
	if(is.FLMatrix(flmatobj2))
	{
		if(nrow(flmatobj1) == nrow(flmatobj2) && ncol(flmatobj1) == ncol(flmatobj2))
		{
			flag1Check(getConnection(flmatobj1))
			t<-sqlQuery(getConnection(flmatobj1),
					    paste0(" INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultMatrixTableFL"),
					    	   " SELECT ",max_matrix_id_value," AS MATRIX_ID ,
					    	   			a.",getVariables(flmatobj1)$rowIdColumn," AS ROW_ID ,
					    	   			a.",getVariables(flmatobj1)$colIdColumn," AS COL_ID ,
					    	   			a.",getVariables(flmatobj1)$valueColumn," MOD b.",getVariables(flmatobj2)$valueColumn," AS CELL_VAL 
					    	   	 FROM ",flmatobj1@db_name,".",flmatobj1@matrix_table," a,",
					    	   	 		flmatobj2@db_name,".",flmatobj2@matrix_table," b 
					    	   	 WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
					    	   	 AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
					    	   	 AND a.",getVariables(flmatobj1)$rowIdColumn,"=b.",getVariables(flmatobj2)$rowIdColumn," 
					    	   	 AND a.",getVariables(flmatobj1)$colIdColumn,"=b.",getVariables(flmatobj2)$colIdColumn))
			
			if(length(t)!=0) { stop("division by zero not supported currently") }
			max_matrix_id_value <<- max_matrix_id_value + 1
			FLMatrix( 
				connection = getConnection(flmatobj1), 
				database = getOption("ResultDatabaseFL"), 
				matrix_table = getOption("ResultMatrixTableFL"), 
				matrix_id_value = max_matrix_id_value - 1, 
				matrix_id_colname = "MATRIX_ID", 
				row_id_colname = "rowIdColumn", 
				col_id_colname = "colIdColumn", 
				cell_val_colname = "valueColumn", 
				nrow = nrow1, 
				ncol = ncol1)
		}
		else stop("ERROR: Invalid matrix dimensions")
	}
	else if(is.vector(flmatobj2))
	{
		flmatobj2 <- as.FLVector(flmatobj2,getConnection(flmatobj1))
		flmatobj1%%flmatobj2
	}
	else if(is.matrix(flmatobj2))
	{
		flmatobj2 <- as.FLMatrix(flmatobj2,getConnection(flmatobj1))
		flmatobj1%%flmatobj2
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
			flag1Check(getConnection(flmatobj1))

			if(!flmatobj2@isDeep)

					t<-sqlQuery(getConnection(flmatobj1),
							    paste0(" INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultMatrixTableFL"),
									   " WITH Z(MATRIX_ID,ROW_ID,COL_ID,CELL_VAL,ROW_NUM) 
									     AS (SELECT a.",flmatobj1@matrix_id_colname,",
									   			  a.",getVariables(flmatobj1)$rowIdColumn,",
									   			  a.",getVariables(flmatobj1)$colIdColumn,",
									   			  a.",getVariables(flmatobj1)$valueColumn,", 
									   			  ROW_NUMBER() OVER (ORDER BY a.",getVariables(flmatobj1)$colIdColumn,",
									   			  							  a.",getVariables(flmatobj1)$rowIdColumn,") AS ROW_NUM  
					        		     FROM ",flmatobj1@matrix_table," a 
					        			 WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,") 
								         SELECT ",max_matrix_id_value,",
								         		  Z.ROW_ID,
								         		  Z.COL_ID,
								         		  Z.CELL_VAL MOD b.",
								         		  flmatobj2@col_name,
								         " FROM ",flmatobj2@db_name,".",flmatobj2@table_name," b,
								         		  Z 
								          WHERE Z.ROW_NUM MOD ",length(flmatobj2)," = b.",flmatobj2@obs_id_colname," MOD ",length(flmatobj2)))

			else
			{
				t<-sqlQuery(getConnection(flmatobj1),
							paste0(" INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultMatrixTableFL"),
								   " WITH Z(MATRIX_ID,ROW_ID,COL_ID,CELL_VAL,ROW_NUM) 
								     AS (SELECT a.",flmatobj1@matrix_id_colname,",
								     			a.",getVariables(flmatobj1)$rowIdColumn,",
								     			a.",getVariables(flmatobj1)$colIdColumn,",
								     			a.",getVariables(flmatobj1)$valueColumn,", 
								     			ROW_NUMBER() OVER (ORDER BY a.",getVariables(flmatobj1)$colIdColumn,",
								     										a.",getVariables(flmatobj1)$rowIdColumn,") AS ROW_NUM  
							        FROM ",flmatobj1@matrix_table," a 
							        WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,") 
							        SELECT ",max_matrix_id_value,",Z.ROW_ID,Z.COL_ID,Z.CELL_VAL MOD b.",flmatobj2@col_name,
							        " FROM ",flmatobj2@db_name,".",flmatobj2@table_name," b,Z 
							        WHERE Z.ROW_NUM MOD ",length(flmatobj2)," = b.",flmatobj2@var_id_name," MOD ",length(flmatobj2),
							        " AND b.",flmatobj2@obs_id_colname,"=",flmatobj2@vector_id_value))
			}
			if(length(t)!=0) { stop("division by zero not supported currently") }
			max_matrix_id_value <<- max_matrix_id_value + 1
			FLMatrix( 
				connection = getConnection(flmatobj1), 
				database = getOption("ResultDatabaseFL"), 
				matrix_table = getOption("ResultMatrixTableFL"), 
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

`%%.numeric` <- function(x,obj1)
{	if(is.FLMatrix(obj1))
	{
		sqlQuery(getConnection(obj1),
				 paste("DATABASE", obj1@db_name,";
				 		SET ROLE ALL;"))
		obj2 <- as.FLVector(x,getConnection(obj1))
		obj2 %% obj1
	}
	else if(class(obj1)=="FLSparseMatrix")
	{
		stop("division by zero not supported currently")
	}
	else if(class(obj1)=="FLVector")
	{
		sqlQuery(getConnection(obj1),
				 paste("DATABASE", obj1@db_name,";
				 		SET ROLE ALL;"))
		obj2 <- as.FLVector(x,getConnection(obj1))
		obj2%%obj1
	}
	else
	{
		op <- .Primitive("%%")
		op(x,obj1)
	}
}

`%%.FLSparseMatrix` <- function(flmatobj1, flmatobj2)
{
	sqlQuery(getConnection(flmatobj1),
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
				flag2Check(getConnection(flmatobj1))

				t<-sqlQuery(getConnection(flmatobj1),
							paste0(" INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultSparseMatrixTableFL"),
			            		   " SELECT ",max_Sparsematrix_id_value,",
			            		   			a.",getVariables(flmatobj1)$rowIdColumn,",
			            		   			a.",getVariables(flmatobj1)$colIdColumn,",
			            		   			a.",getVariables(flmatobj1)$valueColumn," MOD b.",getVariables(flmatobj2)$valueColumn,
			            		   " FROM ",flmatobj1@db_name,".",flmatobj1@matrix_table," a, ",
			            		   			flmatobj2@db_name,".",flmatobj2@matrix_table," b 
			            		   	 WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
			            		   	 AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
			            		   	 AND a.",getVariables(flmatobj1)$rowIdColumn," = b.",getVariables(flmatobj2)$rowIdColumn," 
			            		   	 AND a.",getVariables(flmatobj1)$colIdColumn," =b.",getVariables(flmatobj2)$colIdColumn))
				
				if(length(t)!=0) { stop("division by zero not supported currently") }
				max_Sparsematrix_id_value <<- max_Sparsematrix_id_value + 1
				new("FLSparseMatrix", 
					odbc_connection = getConnection(flmatobj1), 
					database = getOption("ResultDatabaseFL"), 
					matrix_table = getOption("ResultSparseMatrixTableFL"), 
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
				sqlQuery(getConnection(flmatobj2),
						 paste("DATABASE", flmatobj2@db_name,";
						 		SET ROLE ALL;"))
				
				flag1Check(getConnection(flmatobj2))

				t<-sqlQuery(getConnection(flmatobj2),
							paste0( " INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultMatrixTableFL"),
									" SELECT DISTINCT ",max_matrix_id_value,",
											 b.",getVariables(flmatobj2)$rowIdColumn,",
											 b.",getVariables(flmatobj2)$colIdColumn," ,
											 0 ",
									" FROM ",flmatobj2@db_name,".",flmatobj2@matrix_table," b 
									  WHERE b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value,
						            " except ",
						            "SELECT ",max_matrix_id_value,",
						            		b.",getVariables(flmatobj2)$rowIdColumn,",
						            		b.",getVariables(flmatobj2)$colIdColumn," ,
						            		0 
						             FROM ",flmatobj1@db_name,".",flmatobj1@matrix_table," a, ",
						             		flmatobj2@db_name,".",flmatobj2@matrix_table," b 
						             WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
						             AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
						             AND b.",getVariables(flmatobj2)$rowIdColumn," = a.",getVariables(flmatobj1)$rowIdColumn," and b.",getVariables(flmatobj2)$colIdColumn,"=a.",getVariables(flmatobj1)$colIdColumn,
						            " UNION ALL ",
									"SELECT DISTINCT ",max_matrix_id_value,",
											a.",getVariables(flmatobj1)$rowIdColumn,",
											a.",getVariables(flmatobj1)$colIdColumn,",
											a.",getVariables(flmatobj1)$valueColumn," MOD b.",getVariables(flmatobj2)$valueColumn,
									" FROM ",flmatobj1@db_name,".",flmatobj1@matrix_table," a, ",
											 flmatobj2@db_name,".",flmatobj2@matrix_table," b 
									  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
									  AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
									  AND a.",getVariables(flmatobj1)$rowIdColumn," = b.",getVariables(flmatobj2)$rowIdColumn," 
									  AND a.",getVariables(flmatobj1)$colIdColumn," =b.",getVariables(flmatobj2)$colIdColumn))
				
				if(length(t)!=0) { stop("division by zero not supported currently") }
				max_matrix_id_value <<- max_matrix_id_value + 1
				FLMatrix( 
					connection = getConnection(flmatobj2), 
					database = getOption("ResultDatabaseFL"), 
					matrix_table = getOption("ResultMatrixTableFL"), 
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
		else stop("ERROR: Invalid matrix dimensions")
	}
	else if(is.vector(flmatobj2))
		{
			flmatobj2 <- as.FLVector(flmatobj2,getConnection(flmatobj1))
			flmatobj1%%flmatobj2
		}
	else if(is.matrix(flmatobj2))
		{
			flmatobj2 <- as.FLMatrix(flmatobj2,getConnection(flmatobj1))
			flmatobj1%%flmatobj2
		}
	else if(class(flmatobj2)=="dgCMatrix")
		{
			stop("division by zero not supported currently")
		}
	else if(is.FLVector(flmatobj2))
		{
			flag2Check(getConnection(flmatobj1))
			if(flmatobj2@isDeep)
			{
				t<-sqlQuery(getConnection(flmatobj1),
							paste0(" INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultSparseMatrixTableFL"),
		            			   " SELECT ",max_Sparsematrix_id_value,",
		            			   			a.",getVariables(flmatobj1)$rowIdColumn,",
		            			   			a.",getVariables(flmatobj1)$colIdColumn,",
		            			   			a.",getVariables(flmatobj1)$valueColumn," MOD b.",flmatobj2@col_name,
		            			   	" FROM ",flmatobj1@db_name,".",flmatobj1@matrix_table," a, ",
		            			   			 flmatobj2@db_name,".",flmatobj2@table_name," b 
		            			   	  WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
		            			   	  AND b.",flmatobj2@obs_id_colname,"=",flmatobj2@vector_id_value," 
		            			   	  AND (((a.",getVariables(flmatobj1)$colIdColumn,"-1)*",nrow(flmatobj1),")+",
		            							 getVariables(flmatobj1)$rowIdColumn,") MOD ",length(flmatobj2)," = b.",
												 flmatobj2@var_id_name," MOD ",length(flmatobj2)))
			}
			else
			{
				t<-sqlQuery(getConnection(flmatobj1),
							paste0(" INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultSparseMatrixTableFL"),
		            			   " SELECT ",max_Sparsematrix_id_value,",
		            			   			a.",getVariables(flmatobj1)$rowIdColumn,",
		            			   			a.",getVariables(flmatobj1)$colIdColumn,",
		            			   			a.",getVariables(flmatobj1)$valueColumn," MOD b.",flmatobj2@col_name,
		            			   " FROM ",flmatobj1@db_name,".",flmatobj1@matrix_table," a, ",
		            			   			flmatobj2@db_name,".",flmatobj2@table_name," b 
		            			   	 WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
		            			   	 AND (((a.",getVariables(flmatobj1)$colIdColumn,"-1)*",nrow(flmatobj1),")+",
		            							getVariables(flmatobj1)$rowIdColumn,") MOD ",
												length(flmatobj2)," = b.",flmatobj2@obs_id_colname," MOD ",length(flmatobj2)))
			}
			if(length(t)!=0) { stop("division by zero not supported currently") }
			max_Sparsematrix_id_value <<- max_Sparsematrix_id_value + 1
			new("FLSparseMatrix", 
				odbc_connection = getConnection(flmatobj1), 
				database = getOption("ResultDatabaseFL"), 
				matrix_table = getOption("ResultSparseMatrixTableFL"), 
				matrix_id_value = max_Sparsematrix_id_value - 1, 
				matrix_id_colname = "MATRIX_ID", 
				row_id_colname = "rowIdColumn", 
				col_id_colname = "colIdColumn", 
				cell_val_colname = "valueColumn", 
				nrow = nrow(flmatobj1), 
				ncol = ncol(flmatobj1), 
				dimnames= list(c(),c()))
		}
	else stop("Operation Currently Not Supported")
}

`%%.FLVector` <- function(pObj1,pObj2)
{
	vNrow1 <- length(pObj1)
	sqlQuery(getConnection(pObj1),
			 paste("DATABASE", pObj1@db_name,";
			 		SET ROLE ALL;"))
	if(is.FLMatrix(pObj2))
	{
		flag1Check(getConnection(flmatobj1))
		flmatobj2 <- pObj1
		flmatobj1 <- pObj2

		if(!flmatobj2@isDeep)
		{
			t<-sqlQuery(getConnection(flmatobj1),
						paste0(" INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultMatrixTableFL"),
							   " WITH Z(MATRIX_ID,ROW_ID,COL_ID,CELL_VAL,ROW_NUM) 
							   	 AS (SELECT a.",flmatobj1@matrix_id_colname,",
							   	 			a.",getVariables(flmatobj1)$rowIdColumn,",
							   	 			a.",getVariables(flmatobj1)$colIdColumn,",
							   	 			a.",getVariables(flmatobj1)$valueColumn,", 
							   	 			ROW_NUMBER() OVER (ORDER BY a.",getVariables(flmatobj1)$colIdColumn,",
							   	 										a.",getVariables(flmatobj1)$rowIdColumn,") AS ROW_NUM  
			        			 FROM ",flmatobj1@matrix_table," a 
			        			 WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,") 
						         SELECT ",max_matrix_id_value,",
						         		Z.ROW_ID,
						         		Z.COL_ID,
						         		b.",flmatobj2@col_name," MOD Z.CELL_VAL",
						         " FROM ",flmatobj2@db_name,".",flmatobj2@table_name," b,
						         		 Z 
						          WHERE Z.ROW_NUM MOD ",length(flmatobj2)," = b.",flmatobj2@obs_id_colname," MOD ",length(flmatobj2)))
		}
		else
		{
			t<-sqlQuery(getConnection(flmatobj1),
						paste0(" INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultMatrixTableFL"),
							   " WITH Z(MATRIX_ID,ROW_ID,COL_ID,CELL_VAL,ROW_NUM) 
							   	 AS (SELECT a.",flmatobj1@matrix_id_colname,",
							   	 			a.",getVariables(flmatobj1)$rowIdColumn,",
							   	 			a.",getVariables(flmatobj1)$colIdColumn,",
							   	 			a.",getVariables(flmatobj1)$valueColumn,", 
							   	 			ROW_NUMBER() OVER (ORDER BY a.",getVariables(flmatobj1)$colIdColumn,",
							   	 										a.",getVariables(flmatobj1)$rowIdColumn,") AS ROW_NUM  
								     FROM ",flmatobj1@matrix_table," a 
								     WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,") 
						         SELECT ",max_matrix_id_value,",
						         		Z.ROW_ID,
						         		Z.COL_ID,
						         		b.",flmatobj2@col_name," MOD Z.CELL_VAL",
						         " FROM ",flmatobj2@db_name,".",flmatobj2@table_name," b,
						         		  Z 
						          WHERE Z.ROW_NUM MOD ",length(flmatobj2)," = b.",flmatobj2@var_id_name," MOD ",length(flmatobj2),
						          " AND b.",flmatobj2@obs_id_colname,"=",flmatobj2@vector_id_value))
		}
			if(length(t)!=0) { stop("division by zero not supported currently") }
			max_matrix_id_value <<- max_matrix_id_value + 1
			FLMatrix( 
				connection = getConnection(flmatobj1), 
				database = getOption("ResultDatabaseFL"), 
				matrix_table = getOption("ResultMatrixTableFL"), 
				matrix_id_value = max_matrix_id_value - 1, 
				matrix_id_colname = "MATRIX_ID", 
				row_id_colname = "rowIdColumn", 
				col_id_colname = "colIdColumn", 
				cell_val_colname = "valueColumn", 
				nrow = nrow(flmatobj1), 
				ncol = ncol(flmatobj1))
	}
	else if(is.vector(pObj2))
	{
		pObj2 <- as.FLVector(pObj2,getConnection(pObj1))
		pObj1%%pObj2
	}
	else if(is.matrix(pObj2))
	{
		pObj2 <- as.FLMatrix(pObj2,getConnection(pObj1))
		pObj1%%pObj2
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
		flag3Check(getConnection(pObj1))
		if(length(pObj2) > length(pObj1))
		{
			if(pObj2@isDeep)
			vPrimaryKey <- paste0(",b.",pObj2@var_id_name)
			else
			vPrimaryKey <- paste0(",b.",pObj2@obs_id_colname)
			vMinSize <- length(pObj1)
		}
		else
		{
			if(pObj1@isDeep)
			vPrimaryKey <- paste0(",a.",pObj1@var_id_name)
			else
			vPrimaryKey <- paste0(",a.",pObj1@obs_id_colname)
			vMinSize <- length(pObj2)
		}
		

		if(pObj1@isDeep && pObj2@isDeep)
		{
			vSqlStr <-paste0(" INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL"),
					         " SELECT ",max_vector_id_value,
					         			vPrimaryKey,", 
					         			CAST(a.",pObj1@col_name," MOD b.",pObj2@col_name," AS NUMBER) ",
					         " FROM ",pObj1@db_name,".",pObj1@table_name," a,",
					         		  pObj2@db_name,".",pObj2@table_name," b",
					         " WHERE a.",pObj1@obs_id_colname,"=",pObj1@vector_id_value," 
					           AND b.",pObj2@obs_id_colname,"=",pObj2@vector_id_value,
					         " AND a.",pObj1@var_id_name," MOD ",vMinSize," = b.",pObj2@var_id_name," MOD ",vMinSize)

			t<-sqlQuery(getConnection(pObj1),vSqlStr)
		}

		else if(xor(pObj1@isDeep,pObj2@isDeep))
		{
			if(pObj1@isDeep)
			{
				vSqlStr <-paste0(" INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL"),
						         " SELECT ",max_vector_id_value,
						         			vPrimaryKey,", 
						         			CAST(a.",pObj1@col_name," MOD b.",pObj2@col_name," AS NUMBER) ",
						         " FROM ",pObj1@db_name,".",pObj1@table_name," a,",
						         		  pObj2@db_name,".",pObj2@table_name," b",
						         " WHERE a.",pObj1@obs_id_colname,"=",pObj1@vector_id_value,
						         " AND a.",pObj1@var_id_name," MOD ",vMinSize," = b.",pObj2@obs_id_colname," MOD ",vMinSize)

			    t<-sqlQuery(getConnection(pObj1),vSqlStr)
			}
            else
            {
				vSqlStr <-paste0(" INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL"),
						         " SELECT ",max_vector_id_value,
						         			vPrimaryKey,", 
						         			CAST(a.",pObj1@col_name," MOD b.",pObj2@col_name," AS NUMBER) ",
						         " FROM ",pObj1@db_name,".",pObj1@table_name," a,",
						         		  pObj2@db_name,".",pObj2@table_name," b",
						         " WHERE b.",pObj2@obs_id_colname,"=",pObj2@vector_id_value,
			         			 " AND b.",pObj2@var_id_name," MOD ",vMinSize," = a.",pObj1@obs_id_colname," MOD ",vMinSize)

			    t<-sqlQuery(getConnection(pObj1),vSqlStr)
            }
		}

		else if(!pObj1@isDeep && !pObj2@isDeep)
		{
			vSqlStr <-paste0(" INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL"),
					         " SELECT ",max_vector_id_value,
					         			vPrimaryKey,", 
					         			CAST(a.",pObj1@col_name," MOD b.",pObj2@col_name," AS NUMBER) ",
					         " FROM ",pObj1@db_name,".",pObj1@table_name," a,",
					         		  pObj2@db_name,".",pObj2@table_name," b",
					         " WHERE b.",pObj2@var_id_name," MOD ",vMinSize," = a.",pObj1@obs_id_colname," MOD ",vMinSize)

			    t<-sqlQuery(getConnection(pObj1),vSqlStr)
		}

			if(length(t)!=0) { stop("division by zero not supported currently") }
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


`%%.dgCMatrix` <- function(x,flmatobj)
{
	if(is.FLSparseMatrix(flmatobj) || is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLSparseMatrix(x,getConnection(flmatobj))
		flmatobj2 %% flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLSparseMatrix(x,getConnection(flmatobj))
		flmatobj2 %% flmatobj
	}
	else
	{
		op <- .Primitive("%%")
		op(x,flmatobj)
	}

}
