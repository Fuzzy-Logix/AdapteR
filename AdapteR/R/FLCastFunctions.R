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
        if(!any(is.na(as.numeric(x))))
        x <- as.numeric(x)
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
    sqlstr <- constructSelect(x)
    D <- sqlQuery(x@odbc_connection,sqlstr)
    names(D) <- toupper(names(D))
    if(x@isDeep) {
        D <- sqlQuery(x@odbc_connection,sqlstr)
        D <- dcast(D, paste0(toupper(x@obs_id_colname),
                             " ~ ",
                             toupper(x@var_id_colname)),
                   value.var = toupper(x@cell_val_colname))
    } 
    ## gk:  this is broken
    i <- charmatch(rownames(x),D[[toupper(x@obs_id_colname)]],nomatch=0)
                                        # print(i)
    D <- D[i,]
    # print(D[1:20,])
    # print(any(D[[toupper(x@obs_id_colname)]]!=1:nrow(D)))
    if(any(D[[toupper(x@obs_id_colname)]]!=1:nrow(D)))
        rownames(D) <- D[[toupper(x@obs_id_colname)]]
    D[[toupper(x@obs_id_colname)]] <- NULL
    return(D)
}

as.data.frame.FLMatrix <- function(x,...)
{
  temp_m <- as.matrix(x)
  return(as.data.frame(temp_m))
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
    dn <- dimnames(object)
    if(is.null(dn[[1]]) & is.null(dn[[2]]))
        matrix(as.vector(object),
               nrow(object),
               ncol(object))
    else matrix(as.vector(object),
                nrow(object),
                ncol(object),
                dimnames=dn)
}

## #' Converts input FLMatrix object to matrix in R
as.matrix.FLMatrix <- function(object,sparse=FALSE) {
    m <- as.sparseMatrix.FLMatrix(object)
    if(sparse)
        m
    dn <- dimnames(m)
    if(is.null(dn[[1]]) & is.null(dn[[2]]))
        matrix(as.vector(m),
                nrow(m),
                ncol(m))
    else matrix(as.vector(m),
                nrow(m),
                ncol(m),
                dimnames=dn)
}
as.matrix.FLUnionMatrix <- as.matrix.FLMatrix

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
              as.FLMatrix.vector(object,connection,sparse=TRUE,rows,cols,...))
setMethod("as.FLMatrix", signature(object = "data.frame",
                                   connection="ANY",
                                   sparse="logical"),
          function(object,connection,sparse=TRUE)
              as.FLMatrix.data.frame(object,connection,sparse))
setMethod("as.FLMatrix", signature(object = "data.frame",
                                   connection="ANY",
                                   sparse="missing"),
          function(object,connection,sparse=TRUE)
              as.FLMatrix.data.frame(object,connection,sparse=TRUE))
setMethod("as.FLMatrix", signature(object = "FLVector",
                                   connection="ANY",
                                   sparse="logical"),
          function(object,connection,sparse=TRUE,rows=length(object),cols=1,...)
              as.FLMatrix.FLVector(object,connection=getConnection(object),sparse,rows,cols,...))
setMethod("as.FLMatrix", signature(object = "FLVector",
                                   connection="ANY",
                                   sparse="missing"),
          function(object,connection,sparse=TRUE,rows=length(object),cols=1,...)
              as.FLMatrix.FLVector(object,connection=getConnection(object),sparse=TRUE,rows,cols,...))

as.sparseMatrix.FLMatrix <- function(object) {
  valuedf <- sqlQuery(getConnection(object), constructSelect(object))
  i <- match(valuedf[[1]],rownames(object))
  j <- match(valuedf[[2]],colnames(object))
  if(any(is.na(i)) | any(is.na(j)))
      stop("matrix rowname mapping needs to be implemented")
  
  dn <- dimnames(object)
    for(index in 1:2)
        if(!is.null(dn[[index]])){
            if(all(dn[[index]]==as.character(1:(dim(object)[[index]]))))
                dn[index] <- list(NULL)
        }
    ##browser()

    if(is.null(dn[[1]]) & is.null(dn[[2]]))
        m <- sparseMatrix(i = i,
                          j = j,
                          x = valuedf[[3]],
                          dims = dim(object))
    else
        m <- sparseMatrix(i = i,
                          j = j,
                          x = valuedf[[3]],
                          dims = dim(object),
                          dimnames = dn)
    return(m)
}

as.FLMatrix.FLVector <- function(object,connection=getConnection(object),sparse=TRUE,rows=length(object),cols=1)
{
  ### Phani-- not an in-database approach...
  Rvector <- as.vector(object)
  if (class(Rvector)=="data.frame")
  {
    if(any(is.na(as.numeric(Rvector))))
    {
      stop("only numeric entries allowed in FLMatrix")
    }
    else
    return(as.FLMatrix.vector(as.numeric(Rvector),connection,sparse=TRUE,rows=rows,cols=cols))
  }
  else
  return(as.FLMatrix.vector(Rvector,connection,sparse=TRUE,rows=rows,cols=cols))

  # sqlstr <- paste0("INSERT INTO ",getRemoteTableName(result_db_name,result_matrix_table),
  #                 " SELECT ",max_matrix_id_value,
  #                           ",floor(a.",object@obs_id_colname,"+0.1 MOD ",rows,")
  #                            ,a.",object@obs_id_colname,"-floor(a.",object@obs_id_colname,"+0.1 MOD ",rows,")
  #                            ,a.",object@dimnames[[2]],
  #                 " FROM ",object@db_name,".",object@table_name," AS a",
  #                 constructWhere(constraintsSQL(object)))

}

as.FLMatrix.vector <- function(object,connection,sparse=TRUE,rows=length(object),cols=1)
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

setGeneric("as.FLVector", function(object,connection,...) {
    standardGeneric("as.FLVector")
})
setMethod("as.FLVector", signature(object = "vector",
                        connection="ANY"),
          function(object,connection)
              as.FLVector.vector(object,connection))
setMethod("as.FLVector", signature(object = "matrix",
                        connection="ANY"),
          function(object,connection)
              as.FLVector.vector(object,connection))
setMethod("as.FLVector", signature(object = "dgeMatrix",
                        connection="ANY"),
          function(object,connection)
              as.FLVector.vector(object,connection))
setMethod("as.FLVector", signature(object = "dgCMatrix",
                        connection="ANY"),
          function(object,connection)
              as.FLVector.vector(object,connection))
setMethod("as.FLVector", signature(object = "dsCMatrix",
                        connection="ANY"),
          function(object,connection)
              as.FLVector.vector(object,connection))
setMethod("as.FLVector", signature(object = "dgTMatrix",
                        connection="ANY"),
          function(object,connection)
              as.FLVector.vector(object,connection))
setMethod("as.FLVector", signature(object = "data.frame",
                                   connection="ANY"),
          function(object,connection)
              as.FLVector.vector(as.matrix(object),connection))
setMethod("as.FLVector", signature(object = "FLMatrix",
                                   connection="ANY"),
          function(object,connection)
              as.FLVector.FLMatrix(object,connection))
setMethod("as.FLVector", signature(object = "FLMatrix",
                                   connection="missing"),
          function(object,connection=getConnection(object))
              as.FLVector.FLMatrix(object,connection=getConnection(object)))

as.FLVector.vector <- function(object,connection)
{
  if(!is.numeric(object))
  stop("only numeric entries allowed in vector")
  flag3Check(connection)

  sqlstr<-sapply(1:length(object),FUN=function(x) paste0("INSERT INTO ",
           getRemoteTableName(result_db_name,result_vector_table),
           " SELECT ",max_vector_id_value,",",x,",",object[x],";"
                   ))
  
  retobj<-sqlSendUpdate(connection,
                              paste(sqlstr,
                                    collapse="\n"))

  max_vector_id_value <<- max_vector_id_value + 1

  table <- FLTable(connection,
                 result_db_name,
                 result_vector_table,
                 "VECTOR_INDEX",
                 whereconditions=paste0("VECTOR_ID = ",max_vector_id_value-1)
                 )

  return(table[,"VECTOR_VALUE"])
}

as.FLVector.FLMatrix <- function(object,connection=getConnection(object))
{
  flag3Check(connection)
  sqlstr <- paste0(" INSERT INTO ",
                    getRemoteTableName(result_db_name,result_vector_table),
                   " SELECT ",max_vector_id_value,
                   ", ROW_NUMBER() OVER (ORDER BY a.",object@col_id_colname,
                   ",a.",object@row_id_colname,") AS ROW_NUM
                   ,a.",object@cell_val_colname,
                   " FROM ",remoteTable(object)," a ",
                   constructWhere(constraintsSQL(object,localName="a")))

  sqlSendUpdate(connection,sqlstr)
  max_vector_id_value <<- max_vector_id_value + 1

  table <- FLTable(connection,
                 result_db_name,
                 result_vector_table,
                 "VECTOR_INDEX",
                 whereconditions=paste0("VECTOR_ID = ",max_vector_id_value-1)
                 )

  return(table[,"VECTOR_VALUE"])
}

# as.FLVector <- function(obj,connection,size=length(obj))
# {
# 	if(is.vector(obj) || is.matrix(obj) || class(obj)=="dgCMatrix" || is.data.frame(obj))
# 	{
# 		if(size<0) { stop("ERROR: INVALID SIZE") }

# 		if(is.data.frame(obj))
# 		{
# 			obj <- as.matrix(obj)
# 		}

# 		flag3Check(connection)
# 		j <- 1
# 		for(i in 1:size)
# 		{
# 			sqlSendUpdate(connection,
# 					 paste0(" INSERT INTO ",result_db_name,".",result_vector_table,
# 							" SELECT ",max_vector_id_value,", ",i,", '",obj[j],"'"))
# 			j<-j+1
# 			if(j>length(obj)) { j<-1 }
# 		}

# 		max_vector_id_value <<- max_vector_id_value + 1
#         ##browser()
# 		table <- FLTable(connection,
# 						 database=result_db_name,
# 						 table=result_vector_table,
# 						 obs_id_colname="VECTOR_ID",
# 						 var_id_colnames="VECTOR_INDEX",
# 						 cell_val_colname="VECTOR_VALUE",
#                          equalityConstraint(
#                              remoteTable(result_vector_table,"VECTOR_ID"),max_vector_id_value-1))
# 		return(new("FLVector",table))
# 	}

# 	if(is.FLMatrix(obj))
# 	{
# 		flag3Check(connection)

# 		sqlSendUpdate(connection,
# 				 paste0(" INSERT INTO ",result_db_name,".",result_vector_table,
# 						" SELECT ",max_vector_id_value,
#                         ", ROW_NUMBER() OVER (ORDER BY a.",obj@col_id_colname,
#                         ",a.",obj@row_id_colname,") AS ROW_NUM
# 				                ,CAST(a.",obj@cell_val_colname," AS NUMBER)
# 				         FROM ",remoteTable(obj)," a
# 				          WHERE a.",obj@matrix_id_colname,"=",obj@matrix_id_value))

# 		max_vector_id_value <<- max_vector_id_value + 1

# 		table <- FLTable(connection,
# 						 result_db_name,
# 						 result_vector_table,
# 						 "VECTOR_ID",
# 						 "VECTOR_INDEX",
# 						 "VECTOR_VALUE")

# 		return(new("FLVector",
#                    table = table,
#                    col_name = table@cell_val_colname,
#                    vector_id_value = max_vector_id_value-1,
#                    size = size))
# 	}

	# if(is.FLSparseMatrix(obj))
	# {

	#     valuedf <- sqlQuery(obj@odbc_connection,
 #                            paste0("SELECT *
	# 				    		FROM ",remoteTable(obj),
 #                                " WHERE ",obj@matrix_id_colname,"=",obj@matrix_id_value,
 #                                " ORDER BY 1,2,3"))

	#     RSparseMatrix <- sparseMatrix(i=valuedf[,obj@row_id_colname],
 #                                      j=valuedf[,obj@col_id_colname],
 #                                      x=valuedf[,obj@cell_val_colname],
 #                                      dimnames = obj@dimnames)

	#     return(as.FLVector(RSparseMatrix,obj@odbc_connection))
	# }
# }
