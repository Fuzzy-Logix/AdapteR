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

#' Integer Division of in-database objects.
#'
#' \code{\%/\%} does the Element-wise Integer Division of in-database objects.
#'
#' The Element-wise Integer Division of in-database objects mimics the \code{\%/\%} of R data types.
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param x can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param y can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{\%/\%} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @section Constraints: division by 0 gives inf in R, but is not supported for 
#' in-database objects
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 1)
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix %/% Rvector
#' @export

"%/%" <- function(x,y)
{
    UseMethod("%/%", x)
}

`%/%.default` <- function(vec,flmatobj1)
{
	op <- .Primitive("%/%")
	op(vec,flmatobj1)
}

`%/%.matrix` <- function(x,flmatobj1)
{
	if(is.FLMatrix(flmatobj1))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj1@odbc_connection)
		flmatobj2%/%flmatobj1
	}
	# else if(is.FLSparseMatrix(flmatobj1))
	# {
	# 	stop("division by zero not supported currently")
	# }
	else if(is.FLVector(flmatobj1))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj1@odbc_connection)
		flmatobj2%/%flmatobj1
	}
	else 
	{
		op <- .Primitive("%/%")
		op(x,flmatobj1)
	}
}

# `%/%.FLMatrix` <- function(flmatobj1, flmatobj2)
# {
# 	sqlQuery(flmatobj1@odbc_connection,
# 			 paste("DATABASE", flmatobj1@db_name,";
# 			 		SET ROLE ALL;"))
# 	nrow1 <- nrow(flmatobj1)
# 	ncol1 <- ncol(flmatobj1)
# 	if(is.FLMatrix(flmatobj2))
# 	{
# 		if(nrow(flmatobj1) == nrow(flmatobj2) && ncol(flmatobj1) == ncol(flmatobj2))
# 		{
# 			flag1Check(flmatobj1@odbc_connection)
# 			t<-sqlQuery(flmatobj1@odbc_connection,
# 					    paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
# 					    	   " SELECT ",max_matrix_id_value," AS MATRIX_ID ,
# 					    	   			a.",flmatobj1@row_id_colname," AS ROW_ID ,
# 					    	   			a.",flmatobj1@col_id_colname," AS COL_ID ,
# 					    	   			CAST(a.",flmatobj1@cell_val_colname,"/b.",flmatobj2@cell_val_colname," AS INT) AS CELL_VAL 
# 					    	   	 FROM ",flmatobj1@db_name,".",flmatobj1@matrix_table," a,",
# 					    	   	 		flmatobj2@db_name,".",flmatobj2@matrix_table," b 
# 					    	   	 WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
# 					    	   	 AND b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," 
# 					    	   	 AND a.",flmatobj1@row_id_colname,"=b.",flmatobj2@row_id_colname," 
# 					    	   	 AND a.",flmatobj1@col_id_colname,"=b.",flmatobj2@col_id_colname))
			
# 			if(length(t)!=0) { stop("division by zero not supported currently") }
# 			max_matrix_id_value <<- max_matrix_id_value + 1
# 			FLMatrix( 
# 				connection = flmatobj1@odbc_connection, 
# 				database = result_db_name, 
# 				matrix_table = result_matrix_table, 
# 				matrix_id_value = max_matrix_id_value - 1, 
# 				matrix_id_colname = "MATRIX_ID", 
# 				row_id_colname = "ROW_ID", 
# 				col_id_colname = "COL_ID", 
# 				cell_val_colname = "CELL_VAL", 
# 				nrow = nrow1, 
# 				ncol = ncol1)
# 		}
# 		else stop("ERROR: Invalid matrix dimensions")
# 	}
# 	else if(is.vector(flmatobj2))
# 	{
# 		flmatobj2 <- as.FLVector(flmatobj2,flmatobj1@odbc_connection)
# 		flmatobj1%/%flmatobj2
# 	}
# 	else if(is.matrix(flmatobj2))
# 	{
# 		flmatobj2 <- as.FLMatrix(flmatobj2,flmatobj1@odbc_connection)
# 		flmatobj1%/%flmatobj2
# 	}
# 	else if(class(flmatobj2)=="dgCMatrix")
# 	{
# 		stop("division by zero not supported currently")
# 	}
# 	else if(is.FLSparseMatrix(flmatobj2))
# 	{
# 		stop("division by zero not supported currently")
# 	}
# 	else if(is.FLVector(flmatobj2))
# 	{
# 		flag1Check(flmatobj1@odbc_connection)

# 		if(!flmatobj2@isDeep)
# 		{
# 				t<-sqlQuery(flmatobj1@odbc_connection,
# 						    paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
# 								   " WITH Z(MATRIX_ID,ROW_ID,COL_ID,CELL_VAL,ROW_NUM) 
# 								   	 AS (SELECT a.",flmatobj1@matrix_id_colname,",
# 								   	 			a.",flmatobj1@row_id_colname,",
# 								   	 			a.",flmatobj1@col_id_colname,",
# 								   	 			a.",flmatobj1@cell_val_colname,", 
# 								   	 			ROW_NUMBER() OVER (ORDER BY a.",flmatobj1@col_id_colname,",
# 								   	 										a.",flmatobj1@row_id_colname,") AS ROW_NUM  
# 				        				 FROM ",flmatobj1@matrix_table," a 
# 				        				 WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,") 
# 		         					 SELECT ",max_matrix_id_value,",
# 		         					 		 Z.ROW_ID,
# 		         					 		 Z.COL_ID,
# 		         					 		 CAST(Z.CELL_VAL/b.",flmatobj2@col_name," AS INT) 
# 		         					 FROM ",flmatobj2@db_name,".",flmatobj2@table_name," b,
# 		         					 		Z 
# 		          					 WHERE Z.ROW_NUM MOD ",length(flmatobj2)," = b.",flmatobj2@obs_id_colname," MOD ",length(flmatobj2)))
# 		}
# 		else
# 		{
# 			t<-sqlQuery(flmatobj1@odbc_connection,
# 					    paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
# 							   " WITH Z(MATRIX_ID,ROW_ID,COL_ID,CELL_VAL,ROW_NUM) 
# 							   	 AS (SELECT a.",flmatobj1@matrix_id_colname,",
# 							   	 			a.",flmatobj1@row_id_colname,",
# 							   	 			a.",flmatobj1@col_id_colname,",
# 							   	 			a.",flmatobj1@cell_val_colname,", 
# 							   	 			ROW_NUMBER() OVER (ORDER BY a.",flmatobj1@col_id_colname,",
# 							   	 										a.",flmatobj1@row_id_colname,") AS ROW_NUM  
# 			        				 FROM ",flmatobj1@matrix_table," a 
# 			        				 WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,") 
# 						         SELECT ",max_matrix_id_value,",
# 						         		Z.ROW_ID,
# 						         		Z.COL_ID,
# 						         		CAST(Z.CELL_VAL/b.",flmatobj2@col_name," AS INT) 
# 						         FROM ",flmatobj2@db_name,".",flmatobj2@table_name," b,
# 						         	   Z 
# 						         WHERE Z.ROW_NUM MOD ",length(flmatobj2)," = b.",flmatobj2@var_id_name," MOD ",length(flmatobj2),
# 	          					"AND b.",flmatobj2@obs_id_colname,"=",flmatobj2@vector_id_value))
# 		}
# 		if(length(t)!=0) { stop("division by zero not supported currently") }
# 		max_matrix_id_value <<- max_matrix_id_value + 1
# 		FLMatrix( 
# 			connection = flmatobj1@odbc_connection, 
# 			database = result_db_name, 
# 			matrix_table = result_matrix_table, 
# 			matrix_id_value = max_matrix_id_value - 1, 
# 			matrix_id_colname = "MATRIX_ID", 
# 			row_id_colname = "ROW_ID", 
# 			col_id_colname = "COL_ID", 
# 			cell_val_colname = "CELL_VAL", 
# 			nrow = nrow1, 
# 			ncol = ncol1)
# 	}
# 	else cat("ERROR::Operation Currently Not Supported")
# }

`%/%.numeric` <- function(x,obj1)
{	if(is.FLMatrix(obj1))
	{
		obj2 <- as.FLVector(x,obj1@odbc_connection)
		obj2 %/% obj1
	}
	# else if(class(obj1)=="FLSparseMatrix")
	# {
	# 	stop("division by zero not supported currently")
	# }
	else if(class(obj1)=="FLVector")
	{
		obj2 <- as.FLVector(x,obj1@odbc_connection)
		obj2 %/% obj1
	}
	else
	{
		op <- .Primitive("%/%")
		op(x,obj1)
	}
}

`%/%.FLMatrix` <- function(flmatobj1, flmatobj2)
{
	# nrow1 <- nrow(flmatobj1)
	# ncol1 <- ncol(flmatobj1)
	
	if(is.FLMatrix(flmatobj2))
	{
		###Phani-- division by 0 is ignored and 0 is returned.
		checkSameDims(flmatobj1,flmatobj2)

		flag1Check(flmatobj1@odbc_connection)

				
		sqlstr <-   paste0(" INSERT INTO ",
							getRemoteTableName(result_db_name,result_matrix_table),
	            		   " SELECT ",max_matrix_id_value,",
	            		   			a.",flmatobj1@row_id_colname,",
	            		   			a.",flmatobj1@col_id_colname,",
	            		   			CAST(a.",flmatobj1@cell_val_colname,"/b.",flmatobj2@cell_val_colname," AS INT) 
	            		    FROM ",remoteTable(flmatobj1)," a, ",
	            		           remoteTable(flmatobj2)," b ",
	            			constructWhere(c(constraintsSQL(flmatobj1,"a"),
			 		  		constraintsSQL(flmatobj2,"b"),
			 		  		paste0("a.",flmatobj1@row_id_colname,"=b.",flmatobj2@row_id_colname),
			 		  		paste0("a.",flmatobj1@col_id_colname,"=b.",flmatobj2@col_id_colname),
			 		  		paste0("b.",flmatobj2@cell_val_colname,"!=0"))))
		
		t<-sqlQuery(flmatobj1@odbc_connection,sqlstr)
		
		if(length(t)!=0) { stop("division by zero encountered") }

		MID <- max_matrix_id_value	
		max_matrix_id_value <<- max_matrix_id_value + 1
		return(FLMatrix( 
		       connection = flmatobj1@odbc_connection, 
		       database = result_db_name, 
		       matrix_table = result_matrix_table, 
			   matrix_id_value = MID,
			   matrix_id_colname = "MATRIX_ID", 
			   row_id_colname = "ROW_ID", 
			   col_id_colname = "COL_ID", 
			   cell_val_colname = "CELL_VAL",
			   ))
	}
	else if(is.vector(flmatobj2))
		{
			flmatobj2 <- as.FLMatrix(matrix(flmatobj2,nrow(flmatobj1),ncol(flmatobj1)),flmatobj1@odbc_connection)
			flmatobj1 %/% flmatobj2
		}
	else if(is.matrix(flmatobj2))
		{
			flmatobj2 <- as.FLMatrix(flmatobj2,flmatobj1@odbc_connection)
			flmatobj1 %/% flmatobj2
		}
	else if(class(flmatobj2)=="dgCMatrix"||class(flmatobj2)=="dgeMatrix"
		||class(flmatobj2)=="dsCMatrix"||class(flmatobj2)=="dgTMatrix")
		{
			flmatobj2 <- as.FLMatrix(flmatobj2,flmatobj1@odbc_connection)
			flmatobj1 %/% flmatobj2
		}
	else if(is.FLVector(flmatobj2))
		{
			flmatobj2 <- as.FLMatrix(flmatobj2,flmatobj1@odbc_connection,
							sparse=TRUE,rows=nrow(flmatobj1),cols=ncol(flmatobj1))


			# sqlstr <-paste0(" UPDATE ",
			# 				remoteTable(flmatobj2),
			# 				" FROM ( SELECT DISTINCT ",flmatobj2@matrix_id_value," AS mid,
			# 								a.",flmatobj1@row_id_colname," AS rid,
			# 								a.",flmatobj1@col_id_colname," AS cid,
			# 								CAST(a.",flmatobj1@cell_val_colname,"/b.",
			# 			            			flmatobj2@cell_val_colname," AS INT) AS cval 
			# 			             FROM ",remoteTable(flmatobj1)," a, ",
			# 			             		remoteTable(flmatobj2)," b ",
			# 			            constructWhere(c(constraintsSQL(flmatobj1,"a"),
			# 		 		  		constraintsSQL(flmatobj2,"b"),
			# 		 		  		paste0("a.",flmatobj1@row_id_colname,"=b.",flmatobj2@row_id_colname),
			# 		 		  		paste0("a.",flmatobj1@col_id_colname,"=b.",flmatobj2@col_id_colname))),
			# 			            ") c ",
			# 				" SET ",flmatobj2@cell_val_colname,"= c.cval ",
			# 				constructWhere(c(paste0(flmatobj2@matrix_id_colname,"= c.mid "),
			# 		        	paste0(flmatobj2@row_id_colname,"= c.rid "),
			# 		        	paste0(flmatobj2@col_id_colname,"= c.cid "))))

			# t <- sqlQuery(flmatobj1@odbc_connection,sqlstr)

			# if(length(t)!=0) { stop("division by zero not supported currently") }
				
			return(flmatobj1 %/% flmatobj2)
		}
	else stop("Operation Currently Not Supported")
}

`%/%.FLVector` <- function(pObj1,pObj2)
{
	if(is.FLMatrix(pObj2))
	{
		flmatobj1 <- pObj2
		flmatobj2 <- as.FLMatrix(pObj1,flmatobj1@odbc_connection,
			sparse=TRUE,rows=nrow(flmatobj1),cols=ncol(flmatobj1))

		# sqlstr <-paste0(" UPDATE ",
		# 				remoteTable(flmatobj2),
		# 		        " FROM ( SELECT DISTINCT ",flmatobj2@matrix_id_value," AS mid,
		# 		        				a.",flmatobj1@row_id_colname," AS rid,
		# 		        				a.",flmatobj1@col_id_colname," AS cid,
		# 		        				b.",flmatobj2@cell_val_colname,"/a.",flmatobj1@cell_val_colname," AS cval 
		# 		        		 FROM ",remoteTable(flmatobj1)," a, ",
		# 		        		 		remoteTable(flmatobj2)," b ",
		# 		        		 constructWhere(c(constraintsSQL(flmatobj1,"a"),
		# 			 		  		constraintsSQL(flmatobj2,"b"),
		# 			 		  		paste0("a.",flmatobj1@row_id_colname,"=b.",flmatobj2@row_id_colname),
		# 			 		  		paste0("a.",flmatobj1@col_id_colname,"=b.",flmatobj2@col_id_colname))),
		# 		        		 ") c ",
		# 				" SET ",flmatobj2@cell_val_colname,"= c.cval ",
		# 				constructWhere(c(paste0(flmatobj2@matrix_id_colname,"= c.mid "),
		# 			        	paste0(flmatobj2@row_id_colname,"= c.rid "),
		# 			        	paste0(flmatobj2@col_id_colname,"= c.cid "))))

		# sqlQuery(flmatobj1@odbc_connection,sqlstr)
		return(flmatobj2 %/% flmatobj1)
	}
	# 	if(!flmatobj2@isDeep)
	# 	{
	# 		t<-sqlQuery(flmatobj1@odbc_connection,
	# 					paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
	# 						   " WITH Z(MATRIX_ID,ROW_ID,COL_ID,CELL_VAL,ROW_NUM) 
	# 						     AS (SELECT a.",flmatobj1@matrix_id_colname,",
	# 						     			a.",flmatobj1@row_id_colname,",
	# 						     			a.",flmatobj1@col_id_colname,",
	# 						     			a.",flmatobj1@cell_val_colname,", 
	# 						     			ROW_NUMBER() OVER (ORDER BY a.",flmatobj1@col_id_colname,",
	# 						     										a.",flmatobj1@row_id_colname,") AS ROW_NUM  
	# 			        			 FROM ",flmatobj1@matrix_table," a 
	# 			        			 WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,") 
	#          					 SELECT ",max_matrix_id_value,",
	#          					 		Z.ROW_ID,
	#          					 		Z.COL_ID,
	#          					 		CAST(b.",flmatobj2@col_name,"/Z.CELL_VAL AS INT)",
	#          					 "FROM ",flmatobj2@db_name,".",flmatobj2@table_name," b,
	#          					 		Z 
	#           					 WHERE Z.ROW_NUM MOD ",length(flmatobj2)," = b.",flmatobj2@obs_id_colname," MOD ",length(flmatobj2)))
	# 	}
	# 	else
	# 	{
	# 		t<-sqlQuery(flmatobj1@odbc_connection,
	# 					paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
	# 						   " WITH Z(MATRIX_ID,ROW_ID,COL_ID,CELL_VAL,ROW_NUM) 
	# 						     AS (SELECT a.",flmatobj1@matrix_id_colname,",
	# 						     			a.",flmatobj1@row_id_colname,",
	# 						     			a.",flmatobj1@col_id_colname,",
	# 						     			a.",flmatobj1@cell_val_colname,", 
	# 						     			ROW_NUMBER() OVER (ORDER BY a.",flmatobj1@col_id_colname,",
	# 						     										a.",flmatobj1@row_id_colname,") AS ROW_NUM  
	# 						        FROM ",flmatobj1@matrix_table," a 
	# 						        WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,") 
	# 					         SELECT ",max_matrix_id_value,",
	# 					         		Z.ROW_ID,
	# 					         		Z.COL_ID,
	# 					         		CAST(b.",flmatobj2@col_name,"/Z.CELL_VAL AS INT)",
	# 					         " FROM ",flmatobj2@db_name,".",flmatobj2@table_name," b,
	# 					         		 Z 
	# 					          WHERE Z.ROW_NUM MOD ",length(flmatobj2)," = b.",flmatobj2@var_id_name," MOD ",length(flmatobj2),
	# 					          " AND b.",flmatobj2@obs_id_colname,"=",flmatobj2@vector_id_value))
	# 	}
	# 	if(length(t)!=0) { stop("division by zero not supported currently") }
	# 	max_matrix_id_value <<- max_matrix_id_value + 1
	# 	FLMatrix( 
	# 		connection = flmatobj1@odbc_connection, 
	# 		database = result_db_name, 
	# 		matrix_table = result_matrix_table, 
	# 		matrix_id_value = max_matrix_id_value - 1, 
	# 		matrix_id_colname = "MATRIX_ID", 
	# 		row_id_colname = "ROW_ID", 
	# 		col_id_colname = "COL_ID", 
	# 		cell_val_colname = "CELL_VAL", 
	# 		nrow = nrow(flmatobj1), 
	# 		ncol = ncol(flmatobj1))
	# }
	else if(is.vector(pObj2))
	{
		pObj2 <- as.FLVector(pObj2,pObj1@odbc_connection)
		pObj1 %/% pObj2
	}
	else if(is.matrix(pObj2))
	{
		pObj2 <- as.FLMatrix(pObj2,pObj1@odbc_connection)
		pObj1 %/% pObj2
	}
	else if(class(pObj2)=="dgCMatrix"||class(pObj2)=="dgeMatrix"
		||class(pObj2)=="dsCMatrix"||class(pObj2)=="dgTMatrix")
	{
		pObj2 <- as.FLMatrix(pObj2,pObj1@odbc_connection)
		pObj1-pObj2
	}
	else if(is.FLVector(pObj2))
	{
		connection <- pObj2@odbc_connection
		flag3Check(connection)

		if(nrow(pObj1)==1 && nrow(pObj2)==1)
		{
			if(ncol(pObj2)>ncol(pObj1))
			max_length <- ncol(pObj2)
			else max_length <- ncol(pObj1)

			sqlstr <- paste0("INSERT INTO ",
							getRemoteTableName(result_db_name,result_vector_table),
							" SELECT ",max_vector_id_value,
									",",1:max_length,
									",CAST(a.",pObj1@dimnames[[2]],
									"/b.",pObj2@dimnames[[2]]," AS INT) 
							 FROM ",remoteTable(pObj1)," a,",
							   remoteTable(pObj2)," b ",
							constructWhere(c(constraintsSQL(pObj1,localName="a"),
								constraintsSQL(pObj2,localName="b"))),collapse=";")

			sqlSendUpdate(connection,sqlstr)
			max_vector_id_value <<- max_vector_id_value + 1

			table <- FLTable(connection,
				             result_db_name,
				             result_vector_table,
				             "VECTOR_INDEX",
				             whereconditions=paste0(result_db_name,".",result_vector_table,".","VECTOR_ID = ",max_vector_id_value-1)
				             )

			return(table[,"VECTOR_VALUE"])
		}
		if(ncol(pObj1)==1 && ncol(pObj2)==1)
		{

			sqlstr <- paste0("INSERT INTO ",
							getRemoteTableName(result_db_name,result_vector_table),
							" SELECT ",max_vector_id_value,
									",a.",pObj1@obs_id_colname,
									",CAST(a.",pObj1@dimnames[[2]],
									"/b.",pObj2@dimnames[[2]]," AS INT) 
							 FROM ",remoteTable(pObj1)," a,",
							   remoteTable(pObj2)," b ",
							constructWhere(c(constraintsSQL(pObj1,localName="a"),
							constraintsSQL(pObj2,localName="b"),paste0(
							" a.",pObj1@obs_id_colname,"=b.",
							pObj2@obs_id_colname))))

			retobj<- sqlSendUpdate(connection,sqlstr)
			max_vector_id_value <<- max_vector_id_value + 1

			table <- FLTable(connection,
				             result_db_name,
				             result_vector_table,
				             "VECTOR_INDEX",
				             whereconditions=paste0(result_db_name,".",result_vector_table,".","VECTOR_ID = ",max_vector_id_value-1)
				             )

			return(table[,"VECTOR_VALUE"])
		}

		if(ncol(pObj1)==1 && nrow(pObj2)==1)
		{
			if(ncol(pObj2)>nrow(pObj1))
			max_length <- ncol(pObj2)
			else max_length <- nrow(pObj1)

			sqlstr <- paste0("INSERT INTO ",
							getRemoteTableName(result_db_name,result_vector_table),
							" SELECT ",max_vector_id_value,
									",",1:max_length,
									",CAST(a.",pObj1@dimnames[[2]],
									"/b.",pObj2@dimnames[[2]]," AS INT) 
							 FROM ",remoteTable(pObj1)," a,",
							   remoteTable(pObj2)," b ",
							constructWhere(c(constraintsSQL(pObj1,localName="a"),
								constraintsSQL(pObj2,localName="b"))),
							" AND  a.",pObj1@obs_id_colname," IN('",pObj1@dimnames[[1]],"')",collapse=";")

			retobj<- sqlSendUpdate(connection,sqlstr)
			max_vector_id_value <<- max_vector_id_value + 1

			table <- FLTable(connection,
				             result_db_name,
				             result_vector_table,
				             "VECTOR_INDEX",
				             whereconditions=paste0(result_db_name,".",result_vector_table,".","VECTOR_ID = ",max_vector_id_value-1)
				             )

			return(table[,"VECTOR_VALUE"])
		}

		if(nrow(pObj1)==1 && ncol(pObj2)==1)
		{
			if(nrow(pObj2)>ncol(pObj1))
			max_length <- nrow(pObj2)
			else max_length <- ncol(pObj1)

			sqlstr <- paste0("INSERT INTO ",
							getRemoteTableName(result_db_name,result_vector_table),
							" SELECT ",max_vector_id_value,
									",",1:max_length,
									",CAST(a.",pObj1@dimnames[[2]],
									"/b.",pObj2@dimnames[[2]]," AS INT) 
							 FROM ",remoteTable(pObj1)," a,",
							   remoteTable(pObj2)," b ",
							constructWhere(c(constraintsSQL(pObj1,localName="a"),
								constraintsSQL(pObj2,localName="b"))),
							" AND  b.",pObj2@obs_id_colname," IN('",pObj2@dimnames[[1]],"')",collapse=";")

			retobj<- sqlSendUpdate(connection,sqlstr)
			max_vector_id_value <<- max_vector_id_value + 1

			table <- FLTable(connection,
				             result_db_name,
				             result_vector_table,
				             "VECTOR_INDEX",
				             whereconditions=paste0(result_db_name,".",result_vector_table,".","VECTOR_ID = ",max_vector_id_value-1)
				             )

			return(table[,"VECTOR_VALUE"])
		}
	# 	flag3Check(pObj1@odbc_connection)
	# 	if(length(pObj2) > length(pObj1))
	# 	{
	# 		if(pObj2@isDeep)
	# 		vPrimaryKey <- paste0(",b.",pObj2@var_id_name)
	# 		else
	# 		vPrimaryKey <- paste0(",b.",pObj2@obs_id_colname)
	# 		vMinSize <- length(pObj1)
	# 	}
	# 	else
	# 	{
	# 		if(pObj1@isDeep)
	# 		vPrimaryKey <- paste0(",a.",pObj1@var_id_name)
	# 		else
	# 		vPrimaryKey <- paste0(",a.",pObj1@obs_id_colname)
	# 		vMinSize <- length(pObj2)
	# 	}
		

	# 	if(pObj1@isDeep && pObj2@isDeep)
	# 	{
	# 		vSqlStr <- paste0(" INSERT INTO ",result_db_name,".",result_vector_table,
	# 				          " SELECT ",max_vector_id_value,
	# 				          			vPrimaryKey,", 
	# 				          			CAST(a.",pObj1@col_name,"/b.",pObj2@col_name," AS INT) ",
	# 				          " FROM ",pObj1@db_name,".",pObj1@table_name," a,",
	# 				          		   pObj2@db_name,".",pObj2@table_name," b",
	# 				          " WHERE a.",pObj1@obs_id_colname,"=",pObj1@vector_id_value," 
	# 				            AND b.",pObj2@obs_id_colname,"=",pObj2@vector_id_value,
	# 				          " AND a.",pObj1@var_id_name," MOD ",vMinSize," = b.",pObj2@var_id_name," MOD ",vMinSize)

	# 		t<-sqlQuery(pObj1@odbc_connection,vSqlStr)
	# 	}

	# 	else if(xor(pObj1@isDeep,pObj2@isDeep))
	# 	{
	# 		if(pObj1@isDeep)
	# 		{
	# 			vSqlStr <- paste0(" INSERT INTO ",result_db_name,".",result_vector_table,
	# 		         			  " SELECT ",max_vector_id_value,
	# 		         			  			 vPrimaryKey,", 
	# 		         			  			 CAST(a.",pObj1@col_name,"/b.",pObj2@col_name," AS INT) ",
	# 		         			  " FROM ",pObj1@db_name,".",pObj1@table_name," a,",
	# 		         			  		   pObj2@db_name,".",pObj2@table_name," b",
	# 					         " WHERE a.",pObj1@obs_id_colname,"=",pObj1@vector_id_value,
	# 					         " AND a.",pObj1@var_id_name," MOD ",vMinSize," = b.",pObj2@obs_id_colname," MOD ",vMinSize)

	# 		    t<-sqlQuery(pObj1@odbc_connection,vSqlStr)
	# 		}
 #            else
 #            {
	# 			vSqlStr <- paste0(" INSERT INTO ",result_db_name,".",result_vector_table,
	# 		         			  " SELECT ",max_vector_id_value,
	# 		         			  			 vPrimaryKey,", 
	# 		         			  			 CAST(a.",pObj1@col_name,"/b.",pObj2@col_name," AS INT) ",
	# 		         			  " FROM ",pObj1@db_name,".",pObj1@table_name," a,",
	# 		         			  		   pObj2@db_name,".",pObj2@table_name," b",
	# 					          " WHERE b.",pObj2@obs_id_colname,"=",pObj2@vector_id_value,
	# 					          " AND b.",pObj2@var_id_name," MOD ",vMinSize," = a.",pObj1@obs_id_colname," MOD ",vMinSize)

	# 		    t<-sqlQuery(pObj1@odbc_connection,vSqlStr)
 #            }
	# 	}

	# 	else if(!pObj1@isDeep && !pObj2@isDeep)
	# 	{
	# 		vSqlStr <- paste0(" INSERT INTO ",result_db_name,".",result_vector_table,
	# 		         		  " SELECT ",max_vector_id_value,
	# 		         		  			 vPrimaryKey,", 
	# 		         		  			 CAST(a.",pObj1@col_name,"/b.",pObj2@col_name," AS INT) ",
	# 				         " FROM ",pObj1@db_name,".",pObj1@table_name," a,",
	# 				         		  pObj2@db_name,".",pObj2@table_name," b",
	# 				         " WHERE b.",pObj2@var_id_name," MOD ",vMinSize," = a.",pObj1@obs_id_colname," MOD ",vMinSize)

	# 		    t<-sqlQuery(pObj1@odbc_connection,vSqlStr)
	# 	}

	# 		if(length(t)!=0) { stop("division by zero not supported currently") }
	# 		max_vector_id_value <<- max_vector_id_value + 1
	# 		table <- FLTable(connection,
	# 						 result_db_name,
	# 						 result_vector_table,
	# 						 "VECTOR_ID",
	# 						 "VECTOR_INDEX",
	# 						 "VECTOR_VALUE")

	# 		new("FLVector", 
	# 			table = table, 
	# 			col_name = table@cell_val_colname, 
	# 			vector_id_value = max_vector_id_value-1, 
	# 			size = length(pObj1))
			
	}
	else cat("ERROR::Operation Currently Not Supported")
}


`%/%.dgCMatrix` <- function(x,flmatobj)
{
	if(is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj@odbc_connection)
		flmatobj2 %/% flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj@odbc_connection)
		flmatobj2 %/% flmatobj
	}
	else
	{
		op <- .Primitive("%/%")
		op(x,flmatobj)
	}

}

`%/%.dgeMatrix` <- function(x,flmatobj)
{
	if(is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj@odbc_connection)
		flmatobj2 %/% flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj@odbc_connection)
		flmatobj2 %/% flmatobj
	}
	else
	{
		op <- .Primitive("%/%")
		op(x,flmatobj)
	}

}

`%/%.dgTMatrix` <- function(x,flmatobj)
{
	if(is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj@odbc_connection)
		flmatobj2 %/% flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj@odbc_connection)
		flmatobj2 %/% flmatobj
	}
	else
	{
		op <- .Primitive("%/%")
		op(x,flmatobj)
	}

}

`%/%.dsCMatrix` <- function(x,flmatobj)
{
	if(is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj@odbc_connection)
		flmatobj2 %/% flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj@odbc_connection)
		flmatobj2 %/% flmatobj
	}
	else
	{
		op <- .Primitive("%/%")
		op(x,flmatobj)
	}

}
