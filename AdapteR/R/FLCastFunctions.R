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
						 FROM ",remoteTable(object)," 
						 WHERE ",object@table@primary_key,"=",object@vector_id_value,"
						 ORDER BY 1,2")
	}
	else
	{
		sqlstr <- paste0("SELECT *
						 FROM ",remoteTable(object)," 
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

## #' Converts input FLMatrix object to matrix in R
## as.matrix.FLMatrix <- function(flmatobj1)
## {
## 	df <- sqlQuery(flmatobj1@odbc_connection,
## 				   paste0("SELECT *
## 				   		   FROM ",flmatobj1@matrix_table,"
## 				   		   WHERE ",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value,"
## 				   		   ORDER BY 1,2,3"))
## 	ncol <- max(df[,flmatobj1@col_id_colname])
## 	nrow <- max(df[,flmatobj1@row_id_colname])
## 	vec <- df[,flmatobj1@cell_val_colname]
## 	return(matrix(vec,nrow,ncol,byrow=TRUE,dimnames=flmatobj1@dimnames))
## }
as.matrix.FLMatrix <- function(object) {
    ##browser()
    valuedf <- sqlQuery(object@odbc_connection, 
                        paste0(" SELECT ",
                               object@row_id_colname,",",
                               object@col_id_colname,",",
                               object@cell_val_colname,  
                               " FROM ",remoteTable(object),
                               constructWhere(constraintsSQL(object))))
    i <- valuedf[[1]]
    i <- as.factor(i)
    j <- valuedf[[2]]
    j <- as.factor(j)
    m <- sparseMatrix(i = as.numeric(i),
                      j = as.numeric(j),
                      x = valuedf[[3]],
                      dimnames = list(levels(i),levels(j)))
    matrix(m,
           nrow(object),
           ncol(object),
           dimnames=object@dimnames)
}

## setGeneric("as.matrix", function(object){
##     standardGeneric("as.matrix")
## })
## #' Converts FLSparseMatrix object to a matrix in R
## setMethod("as.matrix", signature(object="FLMatrix"),
##           as.matrix.FLMatrix)


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

as.SQL <- function(object) {
    return(sqlstatements)

}

##' Convert a R matrix into a FLMatrix, send INSERT statements and return handle.
##'
##' @param object 
##' @param connection 
##' @param sparse 
##' @param ... 
##' @return  FLMatrix object after casting.
##' @author  Gregor Kappler <g.kappler@@gmx.net>
as.FLMatrix.Matrix <- function(object,connection,sparse=TRUE) {
    if((is.matrix(object) && !is.numeric(object)) || (is.data.frame(object) && !is.numeric(as.matrix(object))))
    {
        stop("ERROR: ONLY NUMERIC ENTRIES ALLOWED IN FLMATRIX")
    }
    else
    {
        ##browser()
        mwide <- Matrix(object, sparse=TRUE)
        mdeep <- Matrix::summary(mwide)
        ## insert one 0 at nrow,ncol for
        ## "storing" matrix dimensions
        if(object[nrow(object),ncol(object)]==0)
            mdeep <- rbind(mdeep,
                           c(i=nrow(object),j=ncol(object),
                             x=0))
        sqlstatements <-
            base::apply(mdeep,1,
                        function(r)
                            paste0(" INSERT INTO ",
                                   getRemoteTableName(
                                       result_db_name,
                                       result_matrix_table),
                                   " (matrix_id, row_id, col_id, cell_val) VALUES (",
                                   paste0(c(max_matrix_id_value,r), collapse=", "),
                                   ");"))
        ##flag1Check(connection)
        retobj<-sqlSendUpdate(connection,
                              paste(sqlstatements,
                                    collapse="\n"))
        max_matrix_id_value <<- max_matrix_id_value + 1
        if(length(dimnames(object))==0) { dimnames(object) <- list(c(),c()) }
        if(length(rownames(object))==0) { rownames(object) <- c() }
        if(length(colnames(object))==0) { colnames(object) <- c() }
        mydims <- list(rownames(object),
                       colnames(object))
        if(is.null(mydims[[1]]))
            mydims[[1]] <- 1:nrow(object)
        if(is.null(mydims[[2]]))
            mydims[[2]] <- 1:ncol(object)
        return(FLMatrix(
                   connection = connection,
                   database = result_db_name,
                   matrix_table = result_matrix_table,
                   matrix_id_value = max_matrix_id_value-1,
                   matrix_id_colname = "MATRIX_ID",
                   row_id_colname = "ROW_ID",
                   col_id_colname = "COL_ID",
                   cell_val_colname = "CELL_VAL",
                   nrow = nrow(object),
                   ncol = ncol(object),
                   dimnames = mydims))
    }
}

setGeneric("remoteTable", function(object) {
    standardGeneric("remoteTable")
})
setMethod("remoteTable", signature(object = "FLMatrix"),
          function(object)
              getRemoteTableName(object@db_name,object@matrix_table))
setMethod("remoteTable", signature(object = "FLTable"),
          function(object)
              getRemoteTableName(object@db_name,object@table_name))
setMethod("remoteTable", signature(object = "FLVector"),
          function(object)
              remoteTable(object@table))


setGeneric("as.FLMatrix", function(object,connection,sparse=TRUE) {
    standardGeneric("as.FLMatrix")
})

setMethod("as.FLMatrix", signature(object = "matrix",
                                   connection="ANY",
                                   sparse="missing"),
          function(object,connection,sparse=TRUE)
              as.FLMatrix.Matrix(object,connection,sparse))
setMethod("as.FLMatrix", signature(object = "matrix",
                                   connection="ANY",
                                   sparse="logical"),
          function(object,connection,sparse)
              as.FLMatrix.Matrix(object,connection,sparse))
setMethod("as.FLMatrix", signature(object = "dgeMatrix",
                                   connection="ANY",
                                   sparse="logical"),
          function(object,connection,sparse=TRUE)
              as.FLMatrix.Matrix(object,connection,sparse))
setMethod("as.FLMatrix", signature(object = "dgCMatrix",
                                   connection="ANY",
                                   sparse="missing"),
          function(object,connection,sparse=TRUE)
              as.FLMatrix.Matrix(object,connection,sparse))
setMethod("as.FLMatrix", signature(object = "dgCMatrix",
                                   connection="ANY",
                                   sparse="logical"),
          function(object,connection,sparse=TRUE)
              as.FLMatrix.Matrix(object,connection,sparse))
##
setMethod("as.FLMatrix", signature(object = "dgCMatrix",
                                   connection="ANY",
                                   sparse="logical"),
          function(object,connection,sparse=TRUE)
              as.FLMatrix.Matrix(object,connection,sparse))
setMethod("as.FLMatrix", signature(object = "dgCMatrix",
                                   connection="ANY",
                                   sparse="missing"),
          function(object,connection,sparse=TRUE)
              as.FLMatrix.Matrix(object,connection,sparse))

## gk: refactor to ONE sql query from matrix table to vactor table!
setMethod("as.FLMatrix", signature(object = "FLVector",
                                   connection="ANY",
                                   sparse="logical"),
          function(object,connection,sparse=TRUE) {
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
                          sqlSendUpdate(connection,
                                   paste0(" INSERT INTO ",result_matrix_table,
                                          " SELECT ",max_matrix_id_value,",
					 				 ",j,",
					 				 ",i,",
					 				 b.",m@col_name,
                                     " FROM ",remoteTable(m@table)," b",
                                     " WHERE b.",m@table@primary_key,"=",k))
                      else
                          sqlSendUpdate(connection,
                                   paste0(" INSERT INTO ",result_matrix_table,
                                          " SELECT ",max_matrix_id_value,",
					 				 ",j,",
					 				 ",i,",
					 				 b.",m@col_name,
                                     " FROM ",remoteTable(m@table)," b",
                                     " WHERE b.",m@table@var_id_name,"=",k,"
							  AND b.",m@table@primary_key,"=",m@vector_id_value))
                      k<-k+1
                  }
              max_matrix_id_value <<- max_matrix_id_value + 1
              return(FLMatrix(
                         connection = connection,
                         database = result_db_name,
                         matrix_table = result_matrix_table,
                         matrix_id_value = max_matrix_id_value-1,
                         matrix_id_colname = "MATRIX_ID",
                         row_id_colname = "ROW_ID",
                         col_id_colname = "COL_ID",
                         cell_val_colname = "CELL_VAL",
                         nrow = nr,
                         ncol = nc))
          })


as.FLMatrix.FLSparseMatrix <- function(m,connection,nr=nrow(m),nc=ncol(m))
{
	if(is.FLSparseMatrix(m))
	{
        flag1Check(connection)

        ## gk: important!  refactor this to a single sql call!
        ## gk: please apply throughout: NO sqlQuery inside loops!
        ## gk: also: R does not at all like loops.  Use apply-like functional approaches.
        for (i in 1:ncol(m))
            for (j in 1:nrow(m))
            {
                sqlSendUpdate(connection,
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
 	 			 		  	    FROM ",remoteTable(m),"
 	 			 		  	    WHERE ",m@matrix_id_colname,"=",m@matrix_id_value,") c
 	 			          SET CELL_VAL = c.cval
 	 			          WHERE ROW_ID = c.rid AND
 	 			                COL_ID = c.cid AND
 	 			                MATRIX_ID = c.mid")

        sqlQuery(connection,sqlstr)

        max_matrix_id_value <<- max_matrix_id_value + 1

        return(FLMatrix(
                   connection = connection,
                   database = result_db_name,
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
    as.FLMatrix(m,connection,TRUE)

## as.FLSparseMatrix <- function(m,connection)
## {
## 	if(class(m)=="dgCMatrix")
## 	{
## 		ROW_ID <- m@i + 1
## 		COL_ID <- pToj(m@p)
## 		CELL_VAL <- m@x

##                                         # setting last element of matrix to zero if it is not non-zero to store the order of matrix
## 		if(max(ROW_ID) < m@Dim[1])
## 		{
## 			ROW_ID <- append(ROW_ID,m@Dim[1])
## 			CELL_VAL <- append(CELL_VAL,0)
## 			if(max(COL_ID) < m@Dim[2]) { COL_ID <- append(COL_ID,m@Dim[2]) }
## 			else { COL_ID <- append(COL_ID,max(COL_ID)) }
## 		}
## 		else if(max(COL_ID) < m@Dim[2])
## 		{
## 			ROW_ID <- append(ROW_ID,max(ROW_ID))
## 			COL_ID <- append(COL_ID,m@Dim[2])
## 			CELL_VAL <- append(CELL_VAL,0)
## 		}

## 		if(length(dimnames(m))==0) { dimnames(m) <- list(c(),c())}
## 		flag2Check(connection)

## 		## gk: important!  refactor this to a single sql call!
## 		## gk: please apply throughout: NO sqlQuery inside loops!
## 		## gk: also: R does not at all like loops.  Use apply-like functional approaches.
## 		for(i in 1:length(ROW_ID))
##             sqlQuery(connection,
##                      paste("INSERT INTO ",result_db_name,".",result_Sparsematrix_table,
##                            " SELECT ",max_Sparsematrix_id_value,",",
##                            ROW_ID[i],",",
##                            COL_ID[i],",",
##                            CELL_VAL[i]))

## 		max_Sparsematrix_id_value <<- max_Sparsematrix_id_value + 1

## 		return(new("FLSparseMatrix",
##                    odbc_connection = connection,
##                    db_name = result_db_name,
##                    matrix_table = result_Sparsematrix_table,
##                    matrix_id_value = max_Sparsematrix_id_value - 1,
##                    matrix_id_colname = "MATRIX_ID",
##                    row_id_colname = "ROW_ID",
##                    col_id_colname = "COL_ID",
##                    cell_val_colname = "CELL_VAL",
##                    nrow = nrow(m),
##                    ncol = ncol(m),
##                    dimnames = dimnames(m)))
## 	}
## 	else stop("input a sparse matrix of class dgCMatrix")
## }

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

		flag3Check(connection)

		## gk: important!  refactor this to a single sql call!
		## gk: please apply throughout: NO sqlQuery inside loops!
		## gk: also: R does not at all like loops.  Use apply-like functional approaches.
		j <- 1
		for(i in 1:size)
		{
			sqlSendUpdate(connection,
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
		flag3Check(connection)

		sqlSendUpdate(connection,
				 paste0(" INSERT INTO ",result_db_name,".",result_vector_table,
						" SELECT ",max_vector_id_value,
                        ", ROW_NUMBER() OVER (ORDER BY a.",obj@col_id_colname,
                        ",a.",obj@row_id_colname,") AS ROW_NUM
				                ,CAST(a.",obj@cell_val_colname," AS NUMBER)
				         FROM ",remoteTable(obj)," a
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

	    valuedf <- sqlQuery(obj@odbc_connection,
                            paste0("SELECT *
					    		FROM ",remoteTable(obj),
                                " WHERE ",obj@matrix_id_colname,"=",obj@matrix_id_value,
                                " ORDER BY 1,2,3"))

	    RSparseMatrix <- sparseMatrix(i=valuedf[,obj@row_id_colname],
                                      j=valuedf[,obj@col_id_colname],
                                      x=valuedf[,obj@cell_val_colname],
                                      dimnames = obj@dimnames)

	    return(as.FLVector(RSparseMatrix,obj@odbc_connection))
	}
}
