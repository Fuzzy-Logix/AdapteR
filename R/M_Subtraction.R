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
	# else if(is.FLSparseMatrix(flmatobj1))
	# {
	# 	flmatobj2 <- as.FLMatrix(x,flmatobj1@odbc_connection)
	# 	flmatobj2-flmatobj1
	# }
	else if(is.FLVector(flmatobj1))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj1@odbc_connection)
		flmatobj2-flmatobj1
	}
	else 
	{
		op <- .Primitive("-")
		op(x,flmatobj1)
	}
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

		obj2 <- as.FLVector(x,obj1@odbc_connection)
		obj2 - obj1
	}
	# else if(class(obj1)=="FLSparseMatrix")
	# {
	# 	obj2 <- as.FLVector(x,obj1@odbc_connection)
	# 	obj2-obj1
	# }
	else if(class(obj1)=="FLVector")
	{
		obj2 <- as.FLVector(x,obj1@odbc_connection)
		obj2-obj1
	}
	else
	{
		op <- .Primitive("-")
		op(x,obj1)
	}
}

`-.FLMatrix` <- function(flmatobj1, flmatobj2)
{
	nrow1 <- nrow(flmatobj1)
	ncol1 <- ncol(flmatobj1)
	if(is.FLMatrix(flmatobj2))
	{
		checkSameDims(flmatobj1,flmatobj2)
		flag1Check(flmatobj1@odbc_connection)
        ## gk: todo:refactor
		sqlstr <-paste0(" INSERT INTO ",
				 		getRemoteTableName(result_db_name,result_matrix_table),
						" SELECT DISTINCT ",max_matrix_id_value,",
								 a.",flmatobj1@variables$rowId,",
								 a.",flmatobj1@variables$colId,",
								 a.",flmatobj1@variables$value,
						" FROM ",remoteTable(flmatobj1)," a ",
						constructWhere(constraintsSQL(flmatobj1,"a")),
			            " except ",
			            "SELECT ",max_matrix_id_value,",
			            		a.",flmatobj1@variables$rowId,",
			            		a.",flmatobj1@variables$colId,",
			            		a.",flmatobj1@variables$value,
			            " FROM ",remoteTable(flmatobj1)," a, ",
			            		 remoteTable(flmatobj2)," b ",
			            constructWhere(c(constraintsSQL(flmatobj1,"a"),
					 		  	constraintsSQL(flmatobj2,"b"),
					 		  	paste0("a.",flmatobj1@variables$rowId,"=b.",flmatobj2@variables$rowId),
					 		  	paste0("a.",flmatobj1@variables$colId,"=b.",flmatobj2@variables$colId))),
			            " UNION ALL ",
			            "SELECT DISTINCT ",max_matrix_id_value,",
			            		b.",flmatobj2@variables$rowId,",
			            		b.",flmatobj2@variables$colId,",
			            		b.",flmatobj2@variables$value,"*(-1) 
			             FROM ",remoteTable(flmatobj2)," b ",
			            constructWhere(constraintsSQL(flmatobj2,"b")),
			            " except ",
			            "SELECT ",max_matrix_id_value,",
			            		b.",flmatobj2@variables$rowId,",
			            		b.",flmatobj2@variables$colId,",
			            		b.",flmatobj2@variables$value,"*(-1) 
			             FROM ",remoteTable(flmatobj1)," a, ",
			            		 remoteTable(flmatobj2)," b ",
			            constructWhere(c(constraintsSQL(flmatobj1,"a"),
					 		  	constraintsSQL(flmatobj2,"b"),
					 		  	paste0("a.",flmatobj1@variables$rowId,"=b.",flmatobj2@variables$rowId),
					 		  	paste0("a.",flmatobj1@variables$colId,"=b.",flmatobj2@variables$colId))),
			            " UNION ALL ",
			            " SELECT ",max_matrix_id_value,",
			            		a.",flmatobj1@variables$rowId,",
			            		a.",flmatobj1@variables$colId,",
			            		a.",flmatobj1@variables$value,"-b.",
			            		flmatobj2@variables$value,
			            " FROM ",remoteTable(flmatobj1)," a, ",
			            		 remoteTable(flmatobj2)," b ",
			            constructWhere(c(constraintsSQL(flmatobj1,"a"),
					 		  	constraintsSQL(flmatobj2,"b"),
					 		  	paste0("a.",flmatobj1@variables$rowId,"=b.",flmatobj2@variables$rowId),
					 		  	paste0("a.",flmatobj1@variables$colId,"=b.",flmatobj2@variables$colId))))

		sqlSendUpdate(flmatobj1@odbc_connection,sqlstr)
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
			flmatobj1-flmatobj2
		}
	else if(is.matrix(flmatobj2))
		{
			flmatobj2 <- as.FLMatrix(flmatobj2,flmatobj1@odbc_connection)
			flmatobj1-flmatobj2
		}
	else if(class(flmatobj2)=="dgCMatrix"||class(flmatobj2)=="dgeMatrix"
		||class(flmatobj2)=="dsCMatrix"||class(flmatobj2)=="dgTMatrix")
		{
			flmatobj2 <- as.FLMatrix(flmatobj2,flmatobj1@odbc_connection)
			flmatobj1-flmatobj2
		}
	else if(is.FLVector(flmatobj2))
		{
			flmatobj2 <- as.FLMatrix(flmatobj2,flmatobj1@odbc_connection,
							sparse=TRUE,rows=nrow(flmatobj1),cols=ncol(flmatobj1))

			# sqlstr0 <-paste0(" UPDATE ",
			# 				remoteTable(flmatobj2),
			# 		        " FROM ( SELECT DISTINCT ",flmatobj2@matrix_id_value," AS mid,
			# 		        				a.",flmatobj2@variables$rowId," AS rid,
			# 		        				a.",flmatobj2@variables$colId," AS cid,
			# 		        				a.",flmatobj2@variables$value,"*(-1) AS cval 
			# 		        		 FROM ",remoteTable(flmatobj2)," a ",
			# 		        		 constructWhere(constraintsSQL(flmatobj2,"a")),") c ",
			# 		        " SET ",flmatobj2@variables$value,"= c.cval ",
			# 		        constructWhere(c(paste0(flmatobj2@matrix_id_colname,"= c.mid "),
			# 		        	paste0(flmatobj2@variables$rowId,"= c.rid "),
			# 		        	paste0(flmatobj2@variables$colId,"= c.cid "))))
			# sqlstr1 <-paste0(" UPDATE ",
			# 				remoteTable(flmatobj2),
			# 				" FROM ( SELECT DISTINCT ",flmatobj2@matrix_id_value," AS mid,
			# 								a.",flmatobj1@variables$rowId," AS rid,
			# 								a.",flmatobj1@variables$colId," AS cid,
			# 								a.",flmatobj1@variables$value,"+b.",
			# 			            			flmatobj2@variables$value," AS cval 
			# 			             FROM ",remoteTable(flmatobj1)," a, ",
			# 			             		remoteTable(flmatobj2)," b ",
			# 			            constructWhere(c(constraintsSQL(flmatobj1,"a"),
			# 		 		  		constraintsSQL(flmatobj2,"b"),
			# 		 		  		paste0("a.",flmatobj1@variables$rowId,"=b.",flmatobj2@variables$rowId),
			# 		 		  		paste0("a.",flmatobj1@variables$colId,"=b.",flmatobj2@variables$colId))),
			# 			            ") c ",
			# 				" SET ",flmatobj2@variables$value,"= c.cval ",
			# 				constructWhere(c(paste0(flmatobj2@matrix_id_colname,"= c.mid "),
			# 		        	paste0(flmatobj2@variables$rowId,"= c.rid "),
			# 		        	paste0(flmatobj2@variables$colId,"= c.cid "))))

			# sqlstr <- paste(sqlstr0,sqlstr1,sep=";")
			# sqlSendUpdate(flmatobj1@odbc_connection,sqlstr)

			return(flmatobj1-flmatobj2)
		}
	else stop("Operation Currently Not Supported")
}

`-.FLVector` <- function(pObj1,pObj2)
{
	if(is.vector(pObj2))
	{
		pObj2 <- as.FLVector(pObj2,pObj1@odbc_connection)
		pObj1-pObj2
	}
	else if(is.matrix(pObj2))
	{
		pObj2 <- as.FLMatrix(pObj2,pObj1@odbc_connection)
		pObj1-pObj2
	}
	else if(class(pObj2)=="dgCMatrix"||class(pObj2)=="dgeMatrix"
		||class(pObj2)=="dsCMatrix"||class(pObj2)=="dgTMatrix")
	{
		pObj2 <- as.FLMatrix(pObj2,pObj1@odbc_connection)
		pObj1-pObj2
	}
	else if(is.FLMatrix(pObj2))
	{
		flmatobj1 <- pObj2
		flmatobj2 <- as.FLMatrix(pObj1,flmatobj1@odbc_connection,
			sparse=TRUE,rows=nrow(flmatobj1),cols=ncol(flmatobj1))

		# sqlstr <-paste0(" UPDATE ",
		# 				remoteTable(flmatobj2),
		# 		        " FROM ( SELECT DISTINCT ",flmatobj2@matrix_id_value," AS mid,
		# 		        				a.",flmatobj1@variables$rowId," AS rid,
		# 		        				a.",flmatobj1@variables$colId," AS cid,
		# 		        				b.",flmatobj2@variables$value,"-a.",flmatobj1@variables$value," AS cval 
		# 		        		 FROM ",remoteTable(flmatobj1)," a, ",
		# 		        		 		remoteTable(flmatobj2)," b ",
		# 		        		 constructWhere(c(constraintsSQL(flmatobj1,"a"),
		# 			 		  		constraintsSQL(flmatobj2,"b"),
		# 			 		  		paste0("a.",flmatobj1@variables$rowId,"=b.",flmatobj2@variables$rowId),
		# 			 		  		paste0("a.",flmatobj1@variables$colId,"=b.",flmatobj2@variables$colId))),
		# 		        		 ") c ",
		# 				" SET ",flmatobj2@variables$value,"= c.cval ",
		# 				constructWhere(c(paste0(flmatobj2@matrix_id_colname,"= c.mid "),
		# 			        	paste0(flmatobj2@variables$rowId,"= c.rid "),
		# 			        	paste0(flmatobj2@variables$colId,"= c.cid "))))

		# sqlQuery(flmatobj1@odbc_connection,sqlstr)
		return(flmatobj2-flmatobj1)
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
									",a.",pObj1@dimnames[[2]],
									"-b.",pObj2@dimnames[[2]],
							" FROM ",remoteTable(pObj1)," a,",
							   remoteTable(pObj2)," b ",
							constructWhere(c(constraintsSQL(pObj1,localName="a"),
								constraintsSQL(pObj2,localName="b"))),collapse=";")

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

		if(ncol(pObj1)==1 && ncol(pObj2)==1)
		{
			## Phani-- Subtraction is done based on matching of
			## primary keys in both vectors.If one vector is short, it is
            ## not repeated as is done in R.
            ##
            ## gk: we can defer this for now, we need modulo.
            ### Phani-- modulo approach I used in earlier version,
            ### But here primary key can be non-numeric
			sqlstr <- paste0("INSERT INTO ",
							getRemoteTableName(result_db_name,result_vector_table),
							" SELECT ",max_vector_id_value,
									",a.",pObj1@variables$obs_id_colname,
									",a.",pObj1@dimnames[[2]],
									"-b.",pObj2@dimnames[[2]],
							" FROM ",remoteTable(pObj1)," a,",
							   remoteTable(pObj2)," b ",
							constructWhere(c(constraintsSQL(pObj1,localName="a"),
							constraintsSQL(pObj2,localName="b"),paste0(
							" a.",pObj1@variables$obs_id_colname,"=b.",
							pObj2@variables$obs_id_colname))))

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
									",a.",pObj1@dimnames[[2]],
									"-b.",pObj2@dimnames[[2]],
							" FROM ",remoteTable(pObj1)," a,",
							   remoteTable(pObj2)," b ",
							constructWhere(c(constraintsSQL(pObj1,localName="a"),
								constraintsSQL(pObj2,localName="b"))),
							" AND  a.",pObj1@variables$obs_id_colname," IN('",pObj1@dimnames[[1]],"')",collapse=";")

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
									",a.",pObj1@dimnames[[2]],
									"-b.",pObj2@dimnames[[2]],
							" FROM ",remoteTable(pObj1)," a,",
							   remoteTable(pObj2)," b ",
							constructWhere(c(constraintsSQL(pObj1,localName="a"),
								constraintsSQL(pObj2,localName="b"))),
							" AND  b.",pObj2@variables$obs_id_colname," IN('",pObj2@dimnames[[1]],"')",collapse=";")

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
		
	}
	else cat("ERROR::Operation Currently Not Supported")
}


`-.dgCMatrix` <- function(x,flmatobj)
{
	if(is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj@odbc_connection)
		flmatobj2 - flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj@odbc_connection)
		flmatobj2 - flmatobj
	}
	else
	{
		op <- .Primitive("-")
		op(x,flmatobj)
	}

}

`-.dgeMatrix` <- function(x,flmatobj)
{
	if(is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj@odbc_connection)
		flmatobj2 - flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj@odbc_connection)
		flmatobj2 - flmatobj
	}
	else
	{
		op <- .Primitive("-")
		op(x,flmatobj)
	}

}

`-.dsCMatrix` <- function(x,flmatobj)
{
	if(is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj@odbc_connection)
		flmatobj2 - flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj@odbc_connection)
		flmatobj2 - flmatobj
	}
	else
	{
		op <- .Primitive("-")
		op(x,flmatobj)
	}

}

`-.dgTMatrix` <- function(x,flmatobj)
{
	if(is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj@odbc_connection)
		flmatobj2 - flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj@odbc_connection)
		flmatobj2 - flmatobj
	}
	else
	{
		op <- .Primitive("-")
		op(x,flmatobj)
	}

}
