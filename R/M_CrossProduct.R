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

#' Cross-Product of in-database objects.
#'
#' \code{\%*\%} does the Cross-Product of in-database objects.
#'
#' The Cross-Product of in-database objects mimics the \code{\%*\%} of R data types.
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param x can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param y can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{\%*\%} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix %*% Rvector
#' @export

"%*%" <- function(x,y)
{
    UseMethod("%*%", x)
}

`%*%.default` <- function(vec,flmatobj1)
{
	op <- .Primitive("%*%")
	op(vec,flmatobj1)
}

`%*%.matrix` <- function(x,flmatobj1)
{
	if(is.FLMatrix(flmatobj1))
	{
		if(ncol(x)!=nrow(flmatobj1)) {stop("non-conformable dimensions")}

		flmatobj2 <- as.FLMatrix(x,flmatobj1@odbc_connection)
		flmatobj2%*%flmatobj1
	}
	else if(is.FLSparseMatrix(flmatobj1))
	{
		if(ncol(x)!=nrow(flmatobj1)) {stop("non-conformable dimensions")}

		flmatobj2 <- as.FLMatrix(x,flmatobj1@odbc_connection)
		flmatobj2%*%flmatobj1
	}
	else if(is.FLVector(flmatobj1))
	{
		if(flmatobj1@size==ncol(x))
		{
			flmatobj2 <- as.FLMatrix(x,flmatobj1@table@odbc_connection)
			flmatobj2%*%flmatobj1
		}
		else stop("non-conformable dimensions")
	}
	else 
	{
		op <- .Primitive("%*%")
		op(x,flmatobj1)
	}
}

`%*%.FLMatrix` <- function(flmatobj1, flmatobj2)
{
	nrow1 <- nrow(flmatobj1)
	ncol1 <- ncol(flmatobj1)
	if(is.FLMatrix(flmatobj2))
	{
		if(ncol(flmatobj1) == nrow(flmatobj2))
		{
			flag1Check(flmatobj1@odbc_connection)
			sqlSendUpdate(flmatobj1@odbc_connection,
					 paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
							" SELECT ",max_matrix_id_value," AS MATRIX_ID ,
									 a.",flmatobj1@row_id_colname," AS ROW_ID ,
									 b.",flmatobj2@col_id_colname," AS COL_ID , 
									 SUM(a.",flmatobj1@cell_val_colname,"*b.",flmatobj2@cell_val_colname,") AS CELL_VAL 
									 FROM ",remoteTable(flmatobj1)," a,",
									 		remoteTable(flmatobj2)," b 
									 WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
									 AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
									 AND a.",flmatobj1@col_id_colname,"=b.",flmatobj2@row_id_colname," 
									 GROUP BY 1,2,3"))
			
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
				 ncol = ncol(flmatobj2))
		}
		else stop("non-conformable dimensions")
	}
	else if(is.vector(flmatobj2))
	{
		if(length(flmatobj2)!=ncol(flmatobj1)) { stop("non-conformable dimensions") }
		flmatobj2 <- as.FLVector(flmatobj2,flmatobj1@odbc_connection)
		flmatobj1%*%flmatobj2
	}
	else if(is.matrix(flmatobj2))
	{
		if(ncol(flmatobj1) != nrow(flmatobj2)) { stop("non-conformable dimensions") }
		flmatobj2 <- as.FLMatrix(flmatobj2,flmatobj1@odbc_connection)
		flmatobj1 %*% flmatobj2
	}
	else if(class(flmatobj2)=="dgCMatrix")
	{
		if(nrow(flmatobj2)!=ncol(flmatobj1)) { stop("non-conformable dimensions") }
		flmatobj2 <- as.FLSparseMatrix(flmatobj2,flmatobj1@odbc_connection)
		flmatobj1 %*% flmatobj2
	}
	else if(is.FLSparseMatrix(flmatobj2))
	{
		if(ncol(flmatobj1) == nrow(flmatobj2))
		{

			vTempFlv <- as.FLVector(0,flmatobj1@odbc_connection)
			vTempFlm <- as.FLMatrix(vTempFlv,flmatobj1@odbc_connection,nr=nrow(flmatobj1),nc=ncol(flmatobj2))

			vSqlStr <- paste0(" UPDATE ",vTempFlm@db_name,".",vTempFlm@matrix_table,
							  " FROM ( SELECT ",vTempFlm@matrix_id_value," AS mid ,
							  				  a.",flmatobj1@row_id_colname," AS rid ,
							  				  b.",flmatobj2@col_id_colname," AS cid , 
							  				  SUM(a.",flmatobj1@cell_val_colname,"*b.",flmatobj2@cell_val_colname,") AS cval 
							  		   FROM ",remoteTable(flmatobj1)," a,",
							  		   		  remoteTable(flmatobj2)," b 
							  		   WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
							  		   AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value,
				    				 " AND a.",flmatobj1@col_id_colname,"=b.",flmatobj2@row_id_colname," 
				    				   GROUP BY 1,2,3",") c ",
							   " SET ",vTempFlm@cell_val_colname,"= c.cval ",
							   " WHERE ",vTempFlm@matrix_id_colname,"= c.mid 
							     AND ",vTempFlm@row_id_colname,"= c.rid 
							     AND ",vTempFlm@col_id_colname,"= c.cid")

			sqlSendUpdate(flmatobj1@odbc_connection, vSqlStr)
			return(vTempFlm)
		}
		else stop("non-conformable dimensions")
	}
	else if(is.FLVector(flmatobj2))
	{
		if(flmatobj2@size==ncol(flmatobj1))
		{
			flag1Check(flmatobj2@odbc_connection)

			if(!flmatobj2@table@isDeep)
			{
				sqlSendUpdate(flmatobj1@odbc_connection,
						 paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								" SELECT ",max_matrix_id_value," AS MATRIX_ID ,
										 a.",flmatobj1@row_id_colname," AS ROW_ID , 
										 CAST(((b.",flmatobj2@table@primary_key,"-0.5)/",ncol(flmatobj1),")+1 as INT) AS COL_ID , 
										 SUM(a.",flmatobj1@cell_val_colname,"*b.",flmatobj2@col_name,") AS CELL_VAL 
								   FROM ",remoteTable(flmatobj1)," a,",
								   		  remoteTable(flmatobj2@table)," b 
								   WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
								   AND a.",flmatobj1@col_id_colname,"=b.",flmatobj2@table@primary_key,"-(CAST(((b.",flmatobj2@table@primary_key,"-0.5)/",ncol(flmatobj1),") as INT) *",ncol(flmatobj1),") 
								   GROUP BY 1,2,3"))
			}

			else
			{
				sqlSendUpdate(flmatobj1@odbc_connection,
						 paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								" SELECT ",max_matrix_id_value," AS MATRIX_ID ,
										 a.",flmatobj1@row_id_colname," AS ROW_ID , 
										 CAST(((b.",flmatobj2@table@var_id_name,"-0.5)/",ncol(flmatobj1),")+1 as INT) AS COL_ID , 
										 SUM(a.",flmatobj1@cell_val_colname,"*b.",flmatobj2@col_name,") AS CELL_VAL 
								   FROM ",remoteTable(flmatobj1)," a,",
								   		  remoteTable(flmatobj2@table)," b 
								   WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
								   AND b.",flmatobj2@table@primary_key,"=",flmatobj2@vector_id_value," 
								   AND a.",flmatobj1@col_id_colname,"=b.",flmatobj2@table@var_id_name,
								 "-(CAST(((b.",flmatobj2@table@var_id_name,"-0.5)/",ncol(flmatobj1),") as INT) *",ncol(flmatobj1),") 
								   GROUP BY 1,2,3"))
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
				 nrow = nrow(flmatobj1), ncol = 1)
			}
		}
		else stop("non-conformable dimensions")
	}
	else cat("ERROR::Operation Currently Not Supported")
}

`%*%.numeric` <- function(x,obj1)
{	
	if(missing(obj1))
	{
		op <- .Primitive("%*%")
		return(op(x))
	}
	if(is.FLMatrix(obj1))
	{
		if(nrow(obj1) != length(x)) stop("non-conformable dimensions")

		obj2 <- as.FLVector(x,obj1@odbc_connection)
		obj2 %*% obj1
	}
	else if(class(obj1)=="FLSparseMatrix")
	{
		if(nrow(obj1) != length(x)) stop("non-conformable dimensions")
		obj2 <- as.FLVector(x,obj1@odbc_connection)
		obj2 %*% obj1
	}
	else if(class(obj1)=="FLVector")
	{
		if(obj1@size != length(x)) stop("non-conformable dimensions")
		obj2 <- as.FLVector(x,obj1@table@odbc_connection)
		obj2 %*% obj1
	}
	else
	{
		op <- .Primitive("%*%")
		op(x,obj1)
	}
}

`%*%.FLSparseMatrix` <- function(flmatobj1, flmatobj2)
{
	nrow1 <- nrow(flmatobj1)
	ncol1 <- ncol(flmatobj1)
	
	if(is.FLMatrix(flmatobj2) || is.FLSparseMatrix(flmatobj2))
	{
		nrow2 <- nrow(flmatobj2)
		ncol2 <- ncol(flmatobj2)

		if(ncol1 == nrow2)
		{
			if(is.FLSparseMatrix(flmatobj2))
			{
				flag2Check(flmatobj1@odbc_connection)

				vSqlStr<-paste0(" INSERT INTO ",result_db_name,".",result_Sparsematrix_table,
								" SELECT ",max_Sparsematrix_id_value," AS MATRIX_ID ,
										 a.",flmatobj1@row_id_colname," AS ROW_ID ,
										 b.",flmatobj2@col_id_colname," AS COL_ID , 
										 SUM(a.",flmatobj1@cell_val_colname,"*b.",flmatobj2@cell_val_colname,") AS CELL_VAL 
										 FROM ",remoteTable(flmatobj1)," a,",
										 		remoteTable(flmatobj2)," b 
										 WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
										 AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
										 AND a.",flmatobj1@col_id_colname,"=b.",flmatobj2@row_id_colname," 
										 GROUP BY 1,2,3")
								
				sqlSendUpdate(flmatobj1@odbc_connection, vSqlStr)
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
					 ncol = ncol2, 
					 dimnames= list(c(),c()))
			}
			else
			{
				
				vTempFlv <- as.FLVector(0,flmatobj1@odbc_connection)
				vTempFlm <- as.FLMatrix(vTempFlv,flmatobj1@odbc_connection,nr=ncol(flmatobj1),nc=nrow(flmatobj2))

				vSqlStr<-paste0(" UPDATE ",vTempFlm@db_name,".",vTempFlm@matrix_table,
								" FROM ( SELECT ",vTempFlm@matrix_id_value," AS mid ,
												a.",flmatobj1@row_id_colname," AS rid ,
												b.",flmatobj2@col_id_colname," AS cid , 
												SUM(a.",flmatobj1@cell_val_colname,"*b.",flmatobj2@cell_val_colname,") AS cval 
										 FROM ",remoteTable(flmatobj1)," a,",
										 		remoteTable(flmatobj2)," b 
										 WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
										 AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value,
									    "AND a.",flmatobj1@col_id_colname,"=b.",flmatobj2@row_id_colname," 
									    GROUP BY 1,2,3",") c ",
							    " SET ",vTempFlm@cell_val_colname,"= c.cval ",
							    " WHERE ",vTempFlm@matrix_id_colname,"= c.mid 
							      AND ",vTempFlm@row_id_colname,"= c.rid 
							      AND ",vTempFlm@col_id_colname,"= c.cid")

				sqlSendUpdate(flmatobj1@odbc_connection, vSqlStr)
				return(vTempFlm)
			}
		}
		else stop("non-conformable dimensions")
	}
	else if(is.vector(flmatobj2))
		{
			if(length(flmatobj2) != ncol(flmatobj1)) stop("non-conformable dimensions")

			flmatobj2 <- as.FLVector(flmatobj2,flmatobj1@odbc_connection)
			flmatobj1 %*% flmatobj2
		}
	else if(is.matrix(flmatobj2))
		{
			if(nrow(flmatobj2) != ncol(flmatobj1)) stop("non-conformable dimensions")
			flmatobj2 <- as.FLMatrix(flmatobj2,flmatobj1@odbc_connection)
			flmatobj1 %*% flmatobj2
		}
	else if(class(flmatobj2)=="dgCMatrix")
		{
			if(nrow(flmatobj2) != ncol(flmatobj1)) stop("non-conformable dimensions")
			flmatobj2 <- as.FLSparseMatrix(flmatobj2,flmatobj1@odbc_connection)
			flmatobj1 %*% flmatobj2
		}
	else if(is.FLVector(flmatobj2))
		{
			if(flmatobj2@size != ncol(flmatobj1)) stop("non-conformable dimensions")

			vTempFlv <- as.FLVector(0,flmatobj1@odbc_connection)
			vTempFlm <- as.FLMatrix(vTempFlv,flmatobj1@odbc_connection,nr=ncol(flmatobj1),nc=1)

			if(!flmatobj2@table@isDeep)
			{
			    vSqlStr<-paste0(" UPDATE ",vTempFlm@db_name,".",vTempFlm@matrix_table,
								" FROM ( SELECT ",vTempFlm@matrix_id_value," AS mid ,
												a.",flmatobj1@row_id_colname," AS rid , 
												CAST(((b.",flmatobj2@table@primary_key,"-0.5)/",ncol(flmatobj1),")+1 as INT) AS cid , 
												SUM(a.",flmatobj1@cell_val_colname,"*b.",flmatobj2@col_name,") AS cval 
										 FROM ",remoteTable(flmatobj1)," a,",
										 		remoteTable(flmatobj2@table)," b 
										 WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
										 AND a.",flmatobj1@col_id_colname,"=b.",flmatobj2@table@primary_key,
									    "-(CAST(((b.",flmatobj2@table@primary_key,"-0.5)/",ncol(flmatobj1),") as INT) *",ncol(flmatobj1),") 
									    GROUP BY 1,2,3",") c ",
							    " SET ",vTempFlm@cell_val_colname,"= c.cval ",
							    " WHERE ",vTempFlm@matrix_id_colname,"= c.mid 
							      AND ",vTempFlm@row_id_colname,"= c.rid 
							      AND ",vTempFlm@col_id_colname,"= c.cid")
			}
			else
			{
				vSqlStr<-paste0(" UPDATE ",vTempFlm@db_name,".",vTempFlm@matrix_table,
								" FROM ( SELECT ",vTempFlm@matrix_id_value," AS mid ,
												a.",flmatobj1@row_id_colname," AS rid , 
												CAST(((b.",flmatobj2@table@var_id_name,"-0.5)/",ncol(flmatobj1),")+1 as INT) AS cid , 
												SUM(a.",flmatobj1@cell_val_colname,"*b.",flmatobj2@col_name,") AS cval 
										 FROM ",remoteTable(flmatobj1)," a,",
													   remoteTable(flmatobj2@table)," b 
										 WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
										 AND b.",flmatobj2@table@primary_key,"=",flmatobj2@vector_id_value,
									   " AND a.",flmatobj1@col_id_colname,"=b.",flmatobj2@table@var_id_name,
									   " -(CAST(((b.",flmatobj2@table@var_id_name,"-0.5)/",ncol(flmatobj1),") as INT) *",ncol(flmatobj1),") 
										 GROUP BY 1,2,3",") c ",
							    " SET ",vTempFlm@cell_val_colname,"= c.cval ",
							    " WHERE ",vTempFlm@matrix_id_colname,"= c.mid 
							      AND ",vTempFlm@row_id_colname,"= c.rid 
							      AND ",vTempFlm@col_id_colname,"= c.cid")	
			}
			
			sqlSendUpdate(flmatobj1@odbc_connection,vSqlStr)
			return(vTempFlm)
		}
	else stop("Operation Currently Not Supported")
}

`%*%.FLVector` <- function(pObj1,pObj2)
{
	vNrow1 <- pObj1@size
	if(is.FLMatrix(pObj2))
	{
		if(pObj1@size != nrow(pObj2)) stop("non-conformable dimensions")
		flag1Check(flmatobj1@odbc_connection)
		flmatobj1 <- pObj2
		flmatobj2 <- pObj1
			if(!flmatobj2@table@isDeep)
			{
			    vSqlStr<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								" SELECT ",max_matrix_id_value," AS MATRIX_ID ,
										 b.",flmatobj2@table@primary_key,
										"-(CAST(((b.",flmatobj2@table@primary_key,"-0.5)/1) as INT) * 1) AS ROW_ID , 
										a.",flmatobj1@col_id_colname," AS COL_ID , 
										SUM(a.",flmatobj1@cell_val_colname,"*b.",flmatobj2@col_name,") AS CELL_VAL 
							       FROM ",remoteTable(flmatobj1)," a,",
										   remoteTable(flmatobj2@table)," b 
								   WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
								   AND a.",flmatobj1@row_id_colname,"= CAST(((b.",flmatobj2@table@primary_key,"-0.5)/1)+1 as INT) 
								   GROUP BY 1,2,3")
			}

			else
			{
				vSqlStr<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								" SELECT ",max_matrix_id_value," AS MATRIX_ID ,
										 b.",flmatobj2@table@var_id_name,
										"-(CAST(((b.",flmatobj2@table@var_id_name,"-0.5)/1) as INT) * 1) AS ROW_ID , 
										a.",flmatobj1@col_id_colname," AS COL_ID , 
										SUM(a.",flmatobj1@cell_val_colname,"*b.",flmatobj2@col_name,") AS CELL_VAL 
								   FROM ",remoteTable(flmatobj1)," a,",
										   remoteTable(flmatobj2@table)," b 
								   WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
								   AND b.",flmatobj2@table@primary_key,"=",flmatobj2@vector_id_value,
								  "AND a.",flmatobj1@row_id_colname,"= CAST(((b.",flmatobj2@table@var_id_name,"-0.5)/1)+1 as INT) 
								   GROUP BY 1,2,3")
			}
			sqlSendUpdate(flmatobj1@odbc_connection, vSqlStr)
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
				 nrow = 1, 
				 ncol = ncol(flmatobj1))
	}
	else if(is.vector(pObj2))
	{
		pObj2 <- as.FLVector(pObj2,pObj1@table@odbc_connection)
		pObj1 %*% pObj2
	}
	else if(is.matrix(pObj2))
	{
		pObj2 <- as.FLMatrix(pObj2,pObj1@table@odbc_connection)
		pObj1 %*% pObj2
	}
	else if(class(pObj2)=="dgCMatrix")
	{
		pObj2 <- as.FLSparseMatrix(pObj2,pObj1@table@odbc_connection)
		pObj1 %*% pObj2
	}
	else if(is.FLSparseMatrix(pObj2))
	{
		flmatobj1 <- pObj2
		flmatobj2 <- pObj1

		if(flmatobj2@size != nrow(flmatobj1)) stop("non-conformable dimensions")

			vTempFlv <- as.FLVector(0,flmatobj1@odbc_connection)
			vTempFlm <- as.FLMatrix(vTempFlv,flmatobj1@odbc_connection,nr=1,nc=nrow(flmatobj1))

			if(!flmatobj2@table@isDeep)
			{
			    sqlSendUpdate(flmatobj1@odbc_connection,
			    		 paste0(" UPDATE ",vTempFlm@db_name,".",vTempFlm@matrix_table,
								" FROM ( SELECT ",vTempFlm@matrix_id_value," AS mid ,
												b.",flmatobj2@table@primary_key,
											    "-(CAST(((b.",flmatobj2@table@primary_key,"-0.5)/1) as INT) * 1) AS rid , 
											    a.",flmatobj1@col_id_colname," AS cid , 
											    SUM(a.",flmatobj1@cell_val_colname,"*b.",flmatobj2@col_name,") AS cval 
										 FROM ",remoteTable(flmatobj1)," a,",
										 		remoteTable(flmatobj2@table)," b 
										 WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
										 AND a.",flmatobj1@row_id_colname,"= CAST(((b.",flmatobj2@table@primary_key,"-0.5)/1)+1 as INT) 
										 GROUP BY 1,2,3",") c ",
							    " SET ",vTempFlm@cell_val_colname,"= c.cval ",
							    " WHERE ",vTempFlm@matrix_id_colname,"= c.mid 
							      AND ",vTempFlm@row_id_colname,"= c.rid 
							      AND ",vTempFlm@col_id_colname,"= c.cid"))

				return(vTempFlm)
			}
			else
			{
				sqlSendUpdate(flmatobj1@odbc_connection,
						 paste0(" UPDATE ",vTempFlm@db_name,".",vTempFlm@matrix_table,
								" FROM ( SELECT ",vTempFlm@matrix_id_value," AS mid ,
												b.",flmatobj2@table@var_id_name,
												"-(CAST(((b.",flmatobj2@table@var_id_name,"-0.5)/1) as INT) * 1) AS rid , 
												a.",flmatobj1@col_id_colname," AS cid , 
												SUM(a.",flmatobj1@cell_val_colname,"*b.",flmatobj2@col_name,") AS cval 
												FROM ",remoteTable(flmatobj1)," a,",
													   remoteTable(flmatobj2@table)," b 
												WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
												AND b.",flmatobj2@table@primary_key,"=",flmatobj2@vector_id_value,
											  " AND a.",flmatobj1@row_id_colname,"= CAST(((b.",flmatobj2@table@var_id_name,"-0.5)/1)+1 as INT) 
											    GROUP BY 1,2,3",") c ",
							    " SET ",vTempFlm@cell_val_colname,"= c.cval ",
							    " WHERE ",vTempFlm@matrix_id_colname,"= c.mid 
							      AND ",vTempFlm@row_id_colname,"= c.rid 
							      AND ",vTempFlm@col_id_colname,"= c.cid"))
				return(vTempFlm)
			}
	}
	else if(is.FLVector(pObj2))
	{
		if(pObj2@size != pObj1@size) stop(" non-conformable dimensions ")
		flag1Check(pObj1@table@odbc_connection)

		flmatobj1 <- pObj1
		flmatobj2 <- pObj2
		
		if(pObj1@table@isDeep && pObj2@table@isDeep)
		{
			vSqlStr<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
							" SELECT ",max_matrix_id_value," AS MATRIX_ID ,
									 a.",flmatobj1@table@var_id_name,
									"-(CAST(((a.",flmatobj1@table@var_id_name,"-0.5)/1) as INT) *1) AS ROW_ID , 
									 CAST(((b.",flmatobj2@table@var_id_name,"-0.5)/",flmatobj2@size,")+1 as INT) AS COL_ID , 
									 SUM(a.",flmatobj1@col_name," * b.",flmatobj2@col_name,") AS CELL_VAL 
									 FROM ",remoteTable(flmatobj1@table)," a,",
									 		remoteTable(flmatobj2@table)," b 
									 WHERE a.",flmatobj1@table@primary_key,"=",flmatobj1@vector_id_value," 
									 AND b.",flmatobj2@table@primary_key,"=",flmatobj2@vector_id_value,
									"AND CAST(((a.",flmatobj1@table@var_id_name,"-0.5)/1)+1 as INT)","=b.",flmatobj2@table@var_id_name,
									"-(CAST(((b.",flmatobj2@table@var_id_name,"-0.5)/",flmatobj2@size,") as INT) *",flmatobj2@size,") 
									GROUP BY 1,2,3")

			sqlSendUpdate(pObj1@table@odbc_connection,vSqlStr)
		}

		else if(xor(pObj1@table@isDeep,pObj2@table@isDeep))
		{
			if(pObj1@table@isDeep)
			{
				vSqlStr<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								" SELECT ",max_matrix_id_value," AS MATRIX_ID ,
										 a.",flmatobj1@table@var_id_name,"-(CAST(((a.",flmatobj1@table@var_id_name,"-0.5)/1) as INT) *1) AS ROW_ID , 
										 CAST(((b.",flmatobj2@table@primary_key,"-0.5)/",flmatobj2@size,")+1 as INT) AS COL_ID , 
										 SUM(a.",flmatobj1@col_name," * b.",flmatobj2@col_name,") AS CELL_VAL 
										 FROM ",remoteTable(flmatobj1@table)," a,",
										 		remoteTable(flmatobj2@table)," b 
										 WHERE a.",flmatobj1@table@primary_key,"=",flmatobj1@vector_id_value,
										"AND CAST(((a.",flmatobj1@table@var_id_name,"-0.5)/1)+1 as INT)","=b.",flmatobj2@table@primary_key,
											"-(CAST(((b.",flmatobj2@table@primary_key,"-0.5)/",flmatobj2@size,") as INT) *",flmatobj2@size,") 
										 GROUP BY 1,2,3")
			    sqlSendUpdate(pObj1@table@odbc_connection,vSqlStr)
			}
            else
            {
				vSqlStr<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								" SELECT ",max_matrix_id_value," AS MATRIX_ID ,
								  		 a.",flmatobj1@table@primary_key,"-(CAST(((a.",flmatobj1@table@primary_key,"-0.5)/1) as INT) *1) AS ROW_ID , 
								  		 CAST(((b.",flmatobj2@table@var_id_name,"-0.5)/",flmatobj2@size,")+1 as INT) AS COL_ID , 
								  		 SUM(a.",flmatobj1@col_name," * b.",flmatobj2@col_name,") AS CELL_VAL 
								  FROM ",remoteTable(flmatobj1@table)," a,",
								   		  remoteTable(flmatobj2@table)," b 
								  WHERE b.",flmatobj2@table@primary_key,"=",flmatobj2@vector_id_value,
								" AND CAST(((a.",flmatobj1@table@primary_key,"-0.5)/1)+1 as INT)","=b.",flmatobj2@table@var_id_name,
									"-(CAST(((b.",flmatobj2@table@var_id_name,"-0.5)/",flmatobj2@size,") as INT) *",flmatobj2@size,") 
								  GROUP BY 1,2,3")
			    sqlSendUpdate(pObj1@table@odbc_connection,vSqlStr)
            }
		}

		else if(!pObj1@table@isDeep && !pObj2@table@isDeep)
		{
			vSqlStr<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
							" SELECT ",max_matrix_id_value," AS MATRIX_ID ,
									 a.",flmatobj1@table@var_id_name,
									"-(CAST(((a.",flmatobj1@table@var_id_name,"-0.5)/1) as INT) *1) AS ROW_ID , 
									 CAST(((b.",flmatobj2@table@var_id_name,"-0.5)/",flmatobj2@size,")+1 as INT) AS COL_ID , 
									 SUM(a.",flmatobj1@col_name," * b.",flmatobj2@col_name,") AS CELL_VAL 
									 FROM ",remoteTable(flmatobj1@table)," a,",
									 		remoteTable(flmatobj2@table)," b 
									 WHERE a.",flmatobj1@table@primary_key,"=",flmatobj1@vector_id_value," 
									 AND b.",flmatobj2@table@primary_key,"=",flmatobj2@vector_id_value,
									"AND CAST(((a.",flmatobj1@table@var_id_name,"-0.5)/1)+1 as INT)","=b.",flmatobj2@table@var_id_name,
										"-(CAST(((b.",flmatobj2@table@var_id_name,"-0.5)/",flmatobj2@size,") as INT) *",flmatobj2@size,") 
									 GROUP BY 1,2,3")

			sqlSendUpdate(pObj1@table@odbc_connection,vSqlStr)
		}

		max_matrix_id_value <<- max_matrix_id_value + 1
		FLMatrix(
			 connection = flmatobj1@table@odbc_connection, 
			 database = result_db_name, 
			 matrix_table = result_matrix_table, 
			 matrix_id_value = max_matrix_id_value - 1, 
			 matrix_id_colname = "MATRIX_ID", 
			 row_id_colname = "ROW_ID", 
			 col_id_colname = "COL_ID", 
			 cell_val_colname = "CELL_VAL", 
			 nrow = 1, 
			 ncol = 1)
			
	}
	else cat("ERROR::Operation Currently Not Supported")
}

`%*%.dgCMatrix` <- function(x,flmatobj)
{
	if(is.FLSparseMatrix(flmatobj) || is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLSparseMatrix(x,flmatobj@odbc_connection)
		flmatobj2 %*% flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLSparseMatrix(x,flmatobj@table@odbc_connection)
		flmatobj2 %*% flmatobj
	}
	else
	{
		op <- .Primitive("%*%")
		op(x,flmatobj)
	}

}
