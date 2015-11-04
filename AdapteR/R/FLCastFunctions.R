#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

#' Converts FLMatrix object to vector in R
as.vector.FLMatrix <- function(object,mode="any")
{
	temp_m <- as.matrix(object)
	return(as.vector(temp_m))
}

#' Converts FLVector object to vector in R
as.vector.FLVector <- function(object,mode="any")
{
	if(object@table@isDeep)
	{
		sqlstr <- paste0("SELECT *
						 FROM ",object@table@db_name,".",object@table@table_name,"
						 WHERE ",object@table@primary_key,"=",object@vector_id_value,"
						 ORDER BY 1,2")
	}
	else
	{
		sqlstr <- paste0("SELECT *
						 FROM ",object@table@db_name,".",object@table@table_name,"
						 ORDER BY 1,2")
	}
	return(sqlQuery(object@table@odbc_connection,sqlstr)[,object@col_name])
}

#' Converts FLSparseMatrix object to vector in R
as.vector.FLSparseMatrix <- function(object,mode="any")
{
	Rmatrix <- as.matrix(object)
	return(as.vector(Rmatrix))
}

##############################################################################################################
#' Converts \code{x} to matrix in R
as.matrix <- function(x)
{
	UseMethod("as.matrix",x)
}

as.matrix.data.frame <- base::as.matrix.data.frame
as.matrix.integer <- base::as.matrix.default
as.matrix.numeric <- base::as.matrix.default

#' Converts input FLMatrix object to matrix in R
as.matrix.FLMatrix <- function(flmatobj1)
{
	sqlQuery(flmatobj1@odbc_connection,
			 paste("DATABASE", flmatobj1@db_name,";
			 		SET ROLE ALL;"))
	df <- sqlQuery(flmatobj1@odbc_connection,
				   paste0("SELECT *
				   		   FROM ",flmatobj1@matrix_table,"
				   		   WHERE ",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,"
				   		   ORDER BY 1,2,3"))
	ncol <- max(df[,flmatobj1@col_id_colname])
	nrow <- max(df[,flmatobj1@row_id_colname])
	vec <- df[,flmatobj1@cell_val_colname]
	return(matrix(vec,nrow,ncol,byrow=TRUE,dimnames=flmatobj1@dimnames))
}

#' Converts FLSparseMatrix object to a matrix in R
as.matrix.FLSparseMatrix <- function(object)
{
	sqlQuery(object@odbc_connection,
			 paste0("DATABASE", object@db_name,";
			 		 SET ROLE ALL;"))
	nrow <- object@nrow
	valuedf <- sqlQuery(object@odbc_connection,
					    paste0("SELECT *
					    		FROM ",object@matrix_table,
					    	  " WHERE ",object@matrix_id_colname,"=",object@matrix_id_value,
					    	  " ORDER BY 1,2,3"))
	matrix(sparseMatrix(i=valuedf[,object@row_id_colname],
						   j=valuedf[,object@col_id_colname],
						   x=valuedf[,object@cell_val_colname],
						   dimnames = object@dimnames),object@nrow,object@ncol,dimnames=object@dimnames)
}

#' Converts FLVector object to a matrix in R
as.matrix.FLVector <- function(obj)
{
	Rvector <- as.vector(obj)
	return(as.matrix(Rvector))
}

###############################################################################################################
#' Casting to FLMatrix
#'
#' Converts input \code{m} to FLMatrix object
#' In addition, one can specify number of rows and columns
#' of resulting flmatrix object
#' @param m matrix,vector,data frame,sparseMatrix,FLVector or FLSparseMatrix which
#' needs to be casted to FLMatrix
#' @param connection ODBC connection object
#' @param nr number of rows in resulting FLMatrix
#' @param nc number of columns in resulting FLMatrix.
#' nr and nc inputs are applicable only in case of vector,FLVector
#' @return FLMatrix object after casting.

as.FLMatrix <- function(m,connection,nr=nrow(m),nc=ncol(m))
{
	if(is.matrix(m) || class(m)=="dgeMatrix" || class(m)=="dgCMatrix" || is.data.frame(m))
	{
		if((is.matrix(m) && !is.numeric(m)) || (is.data.frame(m) && !is.numeric(as.matrix(m))))
		{
			stop("ERROR: ONLY NUMERIC ENTRIES ALLOWED IN FLMATRIX")
		}
		else
		{
			 flag1Check(connection)

       ## gk: important!  refactor this to a single sql call!
       ## gk: please apply throughout: NO sqlQuery inside loops!
       ## gk: also: R does not at all like loops.  Use apply-like functional approaches.
			 mdeep <- summary(m)

			 sqlstatements <- apply(mdeep,1,function(r)
			   paste0(" INSERT INTO ",result_matrix_table,
" (matrix_id, row_id, col_id, cell_val) VALUES (",
			          paste0(c(max_matrix_id_value,r), collapse=", "),
			          ");"))

			 sqlQuery(connection,paste0(sqlstatements,collapse="\n"))
		 	 max_matrix_id_value <<- max_matrix_id_value + 1

		 	 if(length(dimnames(m))==0) { dimnames(m) <- list(c(),c()) }
		 	 if(length(rownames(m))==0) { rownames(m) <- c() }
		 	 if(length(colnames(m))==0) { colnames(m) <- c() }

			 return(new("FLMatrix",
			 			odbc_connection = connection,
			 			db_name = result_db_name,
			 			matrix_table = result_matrix_table,
				 	    matrix_id_value = max_matrix_id_value-1,
					    matrix_id_colname = "MATRIX_ID",
					    row_id_colname = "ROW_ID",
					    col_id_colname = "COL_ID",
					    cell_val_colname = "CELL_VAL",
					    nrow = nrow(m),
					    ncol = ncol(m),
					    dimnames = dimnames(m)))
		}
	}

	if(is.FLVector(m))
	{
		sqlQuery(connection,
				 paste0("DATABASE ",result_db_name,";
				 		 SET ROLE ALL;"))
		flag1Check(connection)
		k<-1

    ## gk: important!  refactor this to a single sql call!
		## gk: please apply throughout: NO sqlQuery inside loops!
		## gk: also: R does not at all like loops.  Use apply-like functional approaches.
		for(i in 1:nc)
		for(j in 1:nr)
		{
			if(k > length(m))
			k<-1
			if(!m@table@isDeep)
			sqlQuery(connection,
					 paste0(" INSERT INTO ",result_matrix_table,
					 		" SELECT ",max_matrix_id_value,",
					 				 ",j,",
					 				 ",i,",
					 				 b.",m@col_name,
							" FROM ",m@table@db_name,".",m@table@table_name," b",
							" WHERE b.",m@table@primary_key,"=",k))
			else
			sqlQuery(connection,
					 paste0(" INSERT INTO ",result_matrix_table,
					 		" SELECT ",max_matrix_id_value,",
					 				 ",j,",
					 				 ",i,",
					 				 b.",m@col_name,
							" FROM ",m@table@db_name,".",m@table@table_name," b",
							" WHERE b.",m@table@var_id_name,"=",k,"
							  AND b.",m@table@primary_key,"=",m@vector_id_value))
			k<-k+1
		}
		max_matrix_id_value <<- max_matrix_id_value + 1
		return(new("FLMatrix",
					odbc_connection = connection,
					db_name = result_db_name,
					matrix_table = result_matrix_table,
					matrix_id_value = max_matrix_id_value-1,
				    matrix_id_colname = "MATRIX_ID",
				    row_id_colname = "ROW_ID",
				    col_id_colname = "COL_ID",
				    cell_val_colname = "CELL_VAL",
				    nrow = nr,
				    ncol = nc))
	}

	if(is.vector(m))
	{
		if(!is.numeric(m))
		{
			stop("ERROR: ONLY NUMERIC ENTRIES ALLOWED IN FLMATRIX")
		}
		else
		{
			 sqlQuery(connection,
			 		  paste0("DATABASE ",result_db_name,";
			 		  		  SET ROLE ALL;"))

			 flag1Check(connection)
			 if(missing(nr))
			 {
			 	if(missing(nc))
			 	{
			 		nr <- length(m)
			 		nc <- 1
			 	}
			 	else
			 	{
			 		nr <- ceiling(length(m)/nc)
			 	}
			 }
			 else
			 {
			 	if(missing(nc))
			 	nc <- ceiling(length(m)/nr)
			 }

			 ## gk: important!  refactor this to a single sql call!
			 ## gk: please apply throughout: NO sqlQuery inside loops!
			 ## gk: also: R does not at all like loops.  Use apply-like functional approaches.
			 k <- 1
			 for (i in 1:nc)
			 for (j in 1:nr)
			 {
			 	if(k > length(m))
			 	k <- 1

		 	 	sqlQuery(connection,
		 	 			 paste0(" INSERT INTO ",result_matrix_table,"
		 	 			 		  SELECT ",max_matrix_id_value,",
		 	 			 		  ",j,",
		 	 			 		  ",i,",
		 	 			 		  ",m[k]))
		 	 	k <- k+1
		 	 }

		 	 max_matrix_id_value <<- max_matrix_id_value + 1

			 return(new("FLMatrix",
			 			odbc_connection = connection,
			 			db_name = result_db_name,
			 			matrix_table = result_matrix_table,
				 	    matrix_id_value = max_matrix_id_value-1,
					    matrix_id_colname = "MATRIX_ID",
					    row_id_colname = "ROW_ID",
					    col_id_colname = "COL_ID",
					    cell_val_colname = "CELL_VAL",
					    nrow = nr,
					    ncol = nc))
		}
	}

	if(is.FLSparseMatrix(m))
	{
		 sqlQuery(connection,
		 		  paste0("DATABASE ",result_db_name,";
		 		  		  SET ROLE ALL;"))

		 flag1Check(connection)

		 ## gk: important!  refactor this to a single sql call!
		 ## gk: please apply throughout: NO sqlQuery inside loops!
		 ## gk: also: R does not at all like loops.  Use apply-like functional approaches.
		 for (i in 1:ncol(m))
		 for (j in 1:nrow(m))
		 {
	 	 	sqlQuery(connection,
	 	 			 paste0(" INSERT INTO ",result_matrix_table,"
	 	 			 		  SELECT ",max_matrix_id_value,",
	 	 			 		  ",j,",
	 	 			 		  ",i,",
	 	 			 		  ",0))
	 	 }

 	 	sqlstr <- paste0("UPDATE ",result_matrix_table,"
 	 			 		  FROM (SELECT ",max_matrix_id_value," AS mid,
 	 			 		  			   ",m@row_id_colname," AS rid,
 	 			 		  			   ",m@col_id_colname," AS cid,
 	 			 		  	           ",m@cell_val_colname," AS cval
 	 			 		  	    FROM ",m@db_name,".",m@matrix_table,"
 	 			 		  	    WHERE ",m@matrix_id_colname,"=",m@matrix_id_value,") c
 	 			          SET CELL_VAL = c.cval
 	 			          WHERE ROW_ID = c.rid AND
 	 			                COL_ID = c.cid AND
 	 			                MATRIX_ID = c.mid")

		sqlQuery(connection,sqlstr)

	 	 max_matrix_id_value <<- max_matrix_id_value + 1

		 return(new("FLMatrix",
		 			odbc_connection = connection,
		 			db_name = result_db_name,
		 			matrix_table = result_matrix_table,
			 	    matrix_id_value = max_matrix_id_value-1,
				    matrix_id_colname = "MATRIX_ID",
				    row_id_colname = "ROW_ID",
				    col_id_colname = "COL_ID",
				    cell_val_colname = "CELL_VAL",
				    nrow = nrow(m),
				    ncol = ncol(m),
				    dimnames = dimnames(m)))
	}
}

#################################################################################################################
#' Casting to FLSparseMatrix
#'
#' Converts input \code{m} to FLMatrix object
#' @param m a sparseMatrix in R of type "dgCMatrix" which
#' needs to be casted to FLSparseMatrix
#' @param connection ODBC connection object
#' @return FLSparseMatrix object after casting.

as.FLSparseMatrix <- function(m,connection)
{
	if(class(m)=="dgCMatrix")
	{
		ROW_ID <- m@i + 1
		COL_ID <- pToj(m@p)
		CELL_VAL <- m@x

		# setting last element of matrix to zero if it is not non-zero to store the order of matrix
		if(max(ROW_ID) < m@Dim[1])
		{
			ROW_ID <- append(ROW_ID,m@Dim[1])
			CELL_VAL <- append(CELL_VAL,0)
			if(max(COL_ID) < m@Dim[2]) { COL_ID <- append(COL_ID,m@Dim[2]) }
			else { COL_ID <- append(COL_ID,max(COL_ID)) }
		}
		else if(max(COL_ID) < m@Dim[2])
		{
			ROW_ID <- append(ROW_ID,max(ROW_ID))
			COL_ID <- append(COL_ID,m@Dim[2])
			CELL_VAL <- append(CELL_VAL,0)
		}

		sqlQuery(connection,paste0("DATABASE ",result_db_name," ;SET ROLE ALL;"))

		if(length(dimnames(m))==0) { dimnames(m) <- list(c(),c())}
		flag2Check(connection)

		## gk: important!  refactor this to a single sql call!
		## gk: please apply throughout: NO sqlQuery inside loops!
		## gk: also: R does not at all like loops.  Use apply-like functional approaches.
		for(i in 1:length(ROW_ID))
		sqlQuery(connection,
				 paste("INSERT INTO ",result_db_name,".",result_Sparsematrix_table,
				 	   " SELECT ",max_Sparsematrix_id_value,",",
				 	   			ROW_ID[i],",",
				 	   			COL_ID[i],",",
				 	   			CELL_VAL[i]))

		max_Sparsematrix_id_value <<- max_Sparsematrix_id_value + 1

		return(new("FLSparseMatrix",
			 odbc_connection = connection,
			 db_name = result_db_name,
			 matrix_table = result_Sparsematrix_table,
			 matrix_id_value = max_Sparsematrix_id_value - 1,
			 matrix_id_colname = "MATRIX_ID",
			 row_id_colname = "ROW_ID",
			 col_id_colname = "COL_ID",
			 cell_val_colname = "CELL_VAL",
			 nrow = nrow(m),
			 ncol = ncol(m),
			 dimnames = dimnames(m)))
	}
	else stop("input a sparse matrix of class dgCMatrix")
}

######################################################################################################################
#' casting to FLVector
#'
#' Converts input \code{obj} to FLVector object
#' @param obj matrix,vector,data frame,sparseMatrix,FLMatrix or FLSparseMatrix which
#' needs to be casted to FLVector
#' @param connection ODBC connection object
#' @param size number of elements in resulting FLVector.
#' size input is not applicable only in case of FLMatrix,FLSparseMatrix
#' @return FLVector object after casting.

as.FLVector <- function(obj,connection,size=length(obj))
{
	if(is.vector(obj) || is.matrix(obj) || class(obj)=="dgCMatrix" || is.data.frame(obj))
	{
		if(size<0) { stop("ERROR: INVALID SIZE") }

		if(is.data.frame(obj))
		{
			obj <- as.matrix(obj)
		}
		sqlQuery(connection,
		 		  paste0("DATABASE ",result_db_name,";
		 		  		  SET ROLE ALL;"))

		flag3Check(connection)

		## gk: important!  refactor this to a single sql call!
		## gk: please apply throughout: NO sqlQuery inside loops!
		## gk: also: R does not at all like loops.  Use apply-like functional approaches.
		j <- 1
		for(i in 1:size)
		{
			sqlQuery(connection,
					 paste0(" INSERT INTO ",result_db_name,".",result_vector_table,
							" SELECT ",max_vector_id_value,", ",i,", '",obj[j],"'"))
			j<-j+1
			if(j>length(obj)) { j<-1 }
		}

		max_vector_id_value <<- max_vector_id_value + 1

		table <- FLTable(connection,
						 result_db_name,
						 result_vector_table,
						 "VECTOR_ID",
						 "VECTOR_INDEX",
						 "VECTOR_VALUE")
		return(new("FLVector",
			table = table,
			col_name = "VECTOR_VALUE",
			vector_id_value = max_vector_id_value-1,
			size = size))
	}

	if(is.FLMatrix(obj))
	{
		 sqlQuery(connection,
		 		  paste0("DATABASE ",result_db_name,";
		 		  		  SET ROLE ALL;"))

		flag3Check(connection)

		sqlQuery(connection,
				 paste0(" INSERT INTO ",result_db_name,".",result_vector_table,
						" SELECT ",max_vector_id_value,
						        ", ROW_NUMBER() OVER (ORDER BY a.",obj@col_id_colname,
						        	                         ",a.",obj@row_id_colname,") AS ROW_NUM
				                ,CAST(a.",obj@cell_val_colname," AS NUMBER)
				         FROM ",obj@db_name,".",obj@matrix_table," a
				          WHERE a.",obj@matrix_id_colname,"=",obj@matrix_id_value))

		max_vector_id_value <<- max_vector_id_value + 1

		table <- FLTable(connection,
						 result_db_name,
						 result_vector_table,
						 "VECTOR_ID",
						 "VECTOR_INDEX",
						 "VECTOR_VALUE")

		return(new("FLVector",
			table = table,
			col_name = table@num_val_name,
			vector_id_value = max_vector_id_value-1,
			size = size))
	}

	if(is.FLSparseMatrix(obj))
	{
		sqlQuery(obj@odbc_connection,
			 paste0("DATABASE", obj@db_name,";
			 		 SET ROLE ALL;"))

	    valuedf <- sqlQuery(obj@odbc_connection,
					    paste0("SELECT *
					    		FROM ",obj@matrix_table,
					    	  " WHERE ",obj@matrix_id_colname,"=",obj@matrix_id_value,
					    	  " ORDER BY 1,2,3"))

	    RSparseMatrix <- sparseMatrix(i=valuedf[,obj@row_id_colname],
						   j=valuedf[,obj@col_id_colname],
						   x=valuedf[,obj@cell_val_colname],
						   dimnames = obj@dimnames)

	    return(as.FLVector(RSparseMatrix,obj@odbc_connection))
	}
}
