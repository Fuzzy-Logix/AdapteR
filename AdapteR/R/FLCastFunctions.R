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
    if(ncol(object)==1)
        x <- as.data.frame(object)[[1]]
    if(nrow(object)==1)
        x <- as.vector(as.data.frame(object)[1,])
    if(!any(names(x)!=1:length(x)))
        names(x) <- NULL
    return(x)
}

#' Converts FLSparseMatrix object to vector in R
as.vector.FLSparseMatrix <- function(object,mode="any")
{
	Rmatrix <- as.matrix(object)
	return(as.vector(Rmatrix))
}

as.data.frame <- function(x, ...)
{
	UseMethod("as.data.frame",x)
}
as.data.frame.FLTable <- function(x, ...){
    if(!x@isDeep) {
        
            sqlstr <- paste0("SELECT ",
                            ifelse(x@obs_id_colname %in% colnames(x),paste0(" "),paste0(x@obs_id_colname,",")),
                            paste(colnames(x),collapse=", "),
                             " FROM ",remoteTable(x),
                             constructWhere(c(constraintsSQL(x))),
                             " ORDER BY ",x@obs_id_colname)
      D <- sqlQuery(x@odbc_connection,sqlstr)
    } else {
        ##browser()
        sqlstr <- paste0("SELECT ",
                         paste(c(x@obs_id_colname,
                             x@var_id_colname,
                             x@cell_val_colname),
                           collapse=", "),
                     " FROM ",remoteTable(x),
                     constructWhere(c(constraintsSQL(x))))
        D <- sqlQuery(x@odbc_connection,sqlstr)
        names(D) <- toupper(names(D))
        ##i <- match(D[[1]],rownames(x))
        ##j <- match(D[[2]],colnames(x))
        ##if(any(is.na(i)) | any(is.na(j)))
        ##    stop("matrix rowname mapping needs to be implemented")
        ## m <- sparseMatrix(i = i,
        ##                   j = j,
        ##                   x = D[[3]],
        ##                   dims = dim(x),
        ##                   dimnames = dimnames(x))
        ## D <- as.data.frame(as.matrix(m))
        D <- dcast(D, paste0(toupper(x@obs_id_colname),
                             " ~ ",
                             toupper(x@var_id_colname)),
                   value.var = toupper(x@cell_val_colname))
        ##rownames(D) <- D$
    }
    names(D) <- toupper(names(D))
    i <- match(rownames(x),D[[toupper(x@obs_id_colname)]])
    D <- D[i,]
    if(any(D[[toupper(x@obs_id_colname)]]!=1:nrow(D)))
        rownames(D) <- D[[toupper(x@obs_id_colname)]]
    D[[toupper(x@obs_id_colname)]] <- NULL
    return(D)
}

as.data.frame.FLMatrix <- function(x,...)
{
  temp_m <- as.matrix(x)
  return(as.data.frame(temp_v))
}
##############################################################################################################
#' Converts \code{x} to matrix in R
as.matrix <- function(x, ...)
{
	UseMethod("as.matrix",x)
}

as.matrix.data.frame <- base::as.matrix.data.frame
as.matrix.integer <- base::as.matrix.default
as.matrix.numeric <- base::as.matrix.default


## #' Converts input FLMatrix object to matrix in R
as.matrix.sparseMatrix <- function(object,sparse=FALSE) {
    if(sparse)
        return(object)
    else matrix(as.vector(object),
                nrow(object),
                ncol(object),
                dimnames=dimnames(object))
}

## #' Converts input FLMatrix object to matrix in R
as.matrix.FLMatrix <- function(object,sparse=FALSE) {
    m <- as.sparseMatrix.FLMatrix(object)
    if(sparse)
        m
    else matrix(as.vector(m),
                nrow(m),
                ncol(m),
                dimnames=dimnames(m))
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

as.matrix.FLTable <- function(x,...)
{
  temp_df <- as.data.frame(x)
  return(as.matrix(temp_df))
}

###############################################################################################################
#' Casting to FLMatrix
#'
#' Converts input \code{m} to FLMatrix object
#' In addition, one can specify number of rows and columns
#' of resulting flmatrix object
#' @param object matrix,vector,data frame,sparseMatrix,FLVector or FLSparseMatrix which
#' needs to be casted to FLMatrix
#' @param connection ODBC connection object
#' @param nr number of rows in resulting FLMatrix
#' @param nc number of columns in resulting FLMatrix.
#' nr and nc inputs are applicable only in case of vector,FLVector
#' @return FLMatrix object after casting.
#' @param sparse 
as.FLMatrix.Matrix <- function(object,connection,sparse=TRUE,...) {
    ##browser()
    if((is.matrix(object) && !is.numeric(object)) || (is.data.frame(object) && !is.numeric(as.matrix(object))))
    {
        stop("ERROR: ONLY NUMERIC ENTRIES ALLOWED IN FLMATRIX")
    }
    else
    {
        ##browser()
        mwide <- Matrix(object, sparse=TRUE)
        if(class(mwide)=="dsCMatrix")
        mwide <- as(mwide,"dgTMatrix")
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
        #browser()
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
                   dimnames = mydims))
    }
}

setGeneric("remoteTable", function(object, table) {
    standardGeneric("remoteTable")
})
setMethod("remoteTable", signature(object = "FLMatrix", table="missing"),
          function(object)
              getRemoteTableName(object@db_name,object@matrix_table))
setMethod("remoteTable", signature(object = "FLTable", table="missing"),
          function(object)
              getRemoteTableName(object@db_name,object@table_name))
setMethod("remoteTable", signature(object = "character", table="character"),
          function(object,table)
              getRemoteTableName(object,table))


setGeneric("as.FLMatrix", function(object,connection,sparse=TRUE,...) {
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
setMethod("as.FLMatrix", signature(object = "dgeMatrix",
                                   connection="ANY",
                                   sparse="missing"),
          function(object,connection,sparse=TRUE)
              as.FLMatrix.Matrix(object,connection,sparse))
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
setMethod("as.FLMatrix", signature(object = "dgTMatrix",
                                   connection="ANY",
                                   sparse="logical"),
          function(object,connection,sparse=TRUE)
              as.FLMatrix.Matrix(object,connection,sparse))
setMethod("as.FLMatrix", signature(object = "dgTMatrix",
                                   connection="ANY",
                                   sparse="missing"),
          function(object,connection,sparse=TRUE)
              as.FLMatrix.Matrix(object,connection,sparse))
setMethod("as.FLMatrix", signature(object = "dsCMatrix",
                                   connection="ANY",
                                   sparse="logical"),
          function(object,connection,sparse=TRUE)
              as.FLMatrix.Matrix(object,connection,sparse))
setMethod("as.FLMatrix", signature(object = "dsCMatrix",
                                   connection="ANY",
                                   sparse="missing"),
          function(object,connection,sparse=TRUE)
              as.FLMatrix.Matrix(object,connection,sparse))
###########################################################################
setMethod("as.FLMatrix", signature(object = "vector",
                                   connection="ANY",
                                   sparse="logical"),
          function(object,connection,sparse=TRUE,rows=length(object),cols=1,...)
              as.FLMatrix.vector(object,connection,sparse,rows,cols,...))
setMethod("as.FLMatrix", signature(object = "vector",
                                   connection="ANY",
                                   sparse="missing"),
          function(object,connection,sparse=TRUE,rows=length(object),cols=1,...)
              as.FLMatrix.vector(object,connection,sparse,rows,cols,...))
setMethod("as.FLMatrix", signature(object = "data.frame",
                                   connection="ANY",
                                   sparse="logical"),
          function(object,connection,sparse=TRUE)
              as.FLMatrix.data.frame(object,connection,sparse))
setMethod("as.FLMatrix", signature(object = "data.frame",
                                   connection="ANY",
                                   sparse="missing"),
          function(object,connection,sparse=TRUE)
              as.FLMatrix.data.frame(object,connection,sparse))
setMethod("as.FLMatrix", signature(object = "FLVector",
                                   connection="ANY",
                                   sparse="logical"),
          function(object,connection,sparse=TRUE,rows=length(object),cols=1,...)
              as.FLMatrix.vector(object,connection,sparse,rows,cols,...))
setMethod("as.FLMatrix", signature(object = "FLVector",
                                   connection="ANY",
                                   sparse="missing"),
          function(object,connection,sparse=TRUE,rows=length(object),cols=1,...)
              as.FLMatrix.vector(object,connection,sparse,rows,cols,...))


as.sparseMatrix.FLMatrix <- function(object) {
  valuedf <- sqlQuery(object@odbc_connection, 
            paste0(" SELECT ",
                   object@row_id_colname,",",
                   object@col_id_colname,",",
                   object@cell_val_colname,  
                   " FROM ",remoteTable(object),
                   constructWhere(constraintsSQL(object))))

    i <- match(valuedf[[1]],rownames(object))
    if(any(is.na(i)))
    i <- match(valuedf[[1]],match(rownames(object),rownames(object)))
    j <- match(valuedf[[2]],colnames(object))
    if(any(is.na(j)))
    j <- match(valuedf[[2]],match(colnames(object),colnames(object)))
    m <- sparseMatrix(i = i,
                      j = j,
                      x = valuedf[[3]],
                      dims = dim(object),
                      dimnames = dimnames(object))
    return(m)
}

as.FLMatrix.FLVector <- function(object,connection=object@odbc_connection,sparse=TRUE,rows,cols)
{
  ### Phani-- working on this.. 9th Dec,2015
  sqlstr <- paste0("INSERT INTO ",getRemoteTableName(result_db_name,result_matrix_table),
                  " SELECT ",max_matrix_id_value,
                            ",floor(a.",object@obs_id_colname,"+0.1 MOD ",rows,")
                             ,a.",object@obs_id_colname,"-floor(a.",object@obs_id_colname,"+0.1 MOD ",rows,")
                             ,a.",object@dimnames[[2]],
                  " FROM ",object@db_name,".",object@table_name," AS a",
                  constructWhere(constraintsSQL(object)))

}

as.FLMatrix.vector <- function(object,connection,sparse=TRUE,rows=length(x),cols=1)
{
  temp_m <- Matrix::Matrix(object,rows,cols,sparse=TRUE)
  return(as.FLMatrix(temp_m,connection))
}

as.FLMatrix.data.frame <- function(object,connection,sparse=TRUE)
{
  temp_m <- Matrix::Matrix(as.matrix(object),sparse=TRUE)
  return(as.FLMatrix(temp_m,connection))
}
# as.FLMatrix.FLSparseMatrix <- function(m,connection,nr=nrow(m),nc=ncol(m))
# {
# 	if(is.FLSparseMatrix(m))
# 	{
#         flag1Check(connection)

#         ## gk: important!  refactor this to a single sql call!
#         ## gk: please apply throughout: NO sqlQuery inside loops!
#         ## gk: also: R does not at all like loops.  Use apply-like functional approaches.
#         for (i in 1:ncol(m))
#             for (j in 1:nrow(m))
#             {
#                 sqlSendUpdate(connection,
#                          paste0(" INSERT INTO ",result_matrix_table,"
# 	 	 			 		  SELECT ",max_matrix_id_value,",
# 	 	 			 		  ",j,",
# 	 	 			 		  ",i,",
# 	 	 			 		  ",0))
#             }

#  	 	sqlstr <- paste0("UPDATE ",result_matrix_table,"
#  	 			 		  FROM (SELECT ",max_matrix_id_value," AS mid,
#  	 			 		  			   ",m@row_id_colname," AS rid,
#  	 			 		  			   ",m@col_id_colname," AS cid,
#  	 			 		  	           ",m@cell_val_colname," AS cval
#  	 			 		  	    FROM ",remoteTable(m),"
#  	 			 		  	    WHERE ",m@matrix_id_colname,"=",m@matrix_id_value,") c
#  	 			          SET CELL_VAL = c.cval
#  	 			          WHERE ROW_ID = c.rid AND
#  	 			                COL_ID = c.cid AND
#  	 			                MATRIX_ID = c.mid")

#         sqlSendUpdate(connection,sqlstr)

#         max_matrix_id_value <<- max_matrix_id_value + 1

#         return(FLMatrix(
#                    connection = connection,
#                    database = result_db_name,
#                    matrix_table = result_matrix_table,
#                    matrix_id_value = max_matrix_id_value-1,
#                    matrix_id_colname = "MATRIX_ID",
#                    row_id_colname = "ROW_ID",
#                    col_id_colname = "COL_ID",
#                    cell_val_colname = "CELL_VAL",
#                    nrow = nrow(m),
#                    ncol = ncol(m),
#                    dimnames = dimnames(m)))
# 	}
# }

#################################################################################################################
#' Casting to FLSparseMatrix
#'
#' Converts input \code{m} to FLMatrix object
#' @param m a sparseMatrix in R of type "dgCMatrix" which
#' needs to be casted to FLSparseMatrix
#' @param connection ODBC connection object
#' @return FLSparseMatrix object after casting.

# as.FLSparseMatrix <- function(m,connection)
#     as.FLMatrix(m,connection,TRUE)


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

    ### Phani-- I will work on this and remove the for loops.. 9th Dec,2015
		## gk: important!  refactor this to a single sql call!
		## gk: please apply throughout: NO sqlQuery inside loops!
		## gk: also: R does not at all like loops.  Use apply-like functional approaches.

    ### Phani-- I will work on this and remove the for loops.. 9th Dec,2015
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
        ##browser()
		table <- FLTable(connection,
						 database=result_db_name,
						 table=result_vector_table,
						 obs_id_colname="VECTOR_ID",
						 var_id_colnames="VECTOR_INDEX",
						 cell_val_colname="VECTOR_VALUE",
                         equalityConstraint(
                             remoteTable(result_vector_table,"VECTOR_ID"),max_vector_id_value-1))
		return(new("FLVector",table))
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
                   col_name = table@cell_val_colname,
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
