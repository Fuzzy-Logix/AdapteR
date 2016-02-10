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
        x <- as.data.frame.FLVector(object)[[1]]
    if(nrow(object)==1)
        x <- as.vector(as.data.frame.FLVector(object)[1,])
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
    sqlstr <- gsub("'%insertIDhere%'",1,sqlstr)
    D <- sqlQuery(getConnection(x),sqlstr)
    names(D) <- toupper(names(D))
    D <- arrange(D,OBS_ID_COLNAME)
    if(x@isDeep) {
        # D <- sqlQuery(getConnection(x),sqlstr)
        D <- dcast(D, paste0(toupper("obs_id_colname"),
                             " ~ ",
                             toupper("var_id_colname")),
                   value.var = toupper("cell_val_colname"))
    } 
    ## gk:  this is broken
    i <- charmatch(rownames(x),D[[toupper("obs_id_colname")]],nomatch=0)
                                        # print(i)
    D <- D[i,]
    # print(D[1:20,])
    # print(any(D[[toupper(x@obs_id_colname)]]!=1:nrow(D)))
    if(any(D[[toupper("obs_id_colname")]]!=1:nrow(D)))
        rownames(D) <- D[[toupper("obs_id_colname")]]
    D[[toupper("obs_id_colname")]] <- NULL
    return(D)
}

as.data.frame.FLVector <- function(x, ...){
    sqlstr <- constructSelect(x)
    sqlstr <- gsub("'%insertIDhere%'",1,sqlstr)
    D <- sqlQuery(getConnection(x),sqlstr)
    names(D) <- toupper(names(D))
    if(x@isDeep) {
        D <- sqlQuery(getConnection(x),sqlstr)
        D <- dcast(D, paste0(toupper("vectorIdColumn"),
                             " ~ ",
                             toupper("vectorIndexColumn")),
                   value.var = toupper("vectorValueColumn"))
    } 
     i <- charmatch(rownames(x),D[[toupper("vectorIndexColumn")]],nomatch=0)
                                        # print(i)
    D <- D[i,]
    # print(D[1:20,])
    # print(any(D[[toupper(x@obs_id_colname)]]!=1:nrow(D)))
    if(any(D[[toupper("vectorIndexColumn")]]!=1:nrow(D)))
        rownames(D) <- D[[toupper("vectorIndexColumn")]]
    D[[toupper("vectorIndexColumn")]] <- NULL
    D[[toupper("vectorIdColumn")]] <- NULL
    ## gk:  this is broken
    # i <- charmatch(rownames(x),D[[toupper(getVariables(x)$obs_id_colname)]],nomatch=0)
    #                                     # print(i)
    # D <- D[i,]
    # # print(D[1:20,])
    # # print(any(D[[toupper(x@obs_id_colname)]]!=1:nrow(D)))
    # if(any(D[[toupper(getVariables(x)$obs_id_colname)]]!=1:nrow(D)))
    #     rownames(D) <- D[[toupper(getVariables(x)$obs_id_colname)]]
    # D[[toupper(getVariables(x)$obs_id_colname)]] <- NULL
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
as.matrix.FLMatrixBind <- as.matrix.FLMatrix

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
            mdeep <- base::rbind(mdeep,
                           c(i=nrow(object),j=ncol(object),
                             x=0))
        MID <- getMaxMatrixId(connection)
        sqlstatements <-
            base::apply(mdeep,1,
                        function(r)
                            paste0(" INSERT INTO ",
                                   getRemoteTableName(
                                       getOption("ResultDatabaseFL"),
                                       getOption("ResultMatrixTableFL")),
                                   " (matrix_id, rowIdColumn, colIdColumn, valueColumn) VALUES (",
                                   paste0(c(MID,r), collapse=", "),
                                   ");"))

        ##flag1Check(connection)
        retobj<-sqlSendUpdate(connection,
                              paste(sqlstatements,
                                    collapse="\n"))
        #max_matrix_id_value <<- max_matrix_id_value + 1
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
                   database = getOption("ResultDatabaseFL"),
                   matrix_table = getOption("ResultMatrixTableFL"),
                   matrix_id_value = MID,
                   matrix_id_colname = "MATRIX_ID",
                   row_id_colname = "rowIdColumn",
                   col_id_colname = "colIdColumn",
                   cell_val_colname = "valueColumn",
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
  sqlstr <- gsub("'%insertIDhere%'",1,constructSelect(object))
  valuedf <- sqlQuery(getConnection(object), sqlstr)
  i <- match(valuedf$rowIdColumn,rownames(object))
  j <- match(valuedf$colIdColumn,colnames(object))
  if(any(is.na(i)) | any(is.na(j)))
      stop("matrix rowname mapping needs to be implemented")
  
  dn <- dimnames(object)
  for(index in 1:2){
        if(!is.null(dn[[index]])){
          dn[[index]] <- as.character(dn[[index]])
            if(all(as.character(dn[[index]])==as.character(1:(dim(object)[[index]]))))
                dn[index] <- list(NULL)
        }
      ##gk: fix this, currently dimnames needed
      if(is.null(dn[[index]]))
          dn[[index]] <- 1:dim(object)[[index]]
  }
 ##browser()
  values <- valuedf$valueColumn
  if(is.null(values))
      m <- sparseMatrix(i = i,
                        j = j,
                        x = i,
                        dims = dim(object),
                        dimnames = dn)
  else if(is.null(dn[[1]]) & is.null(dn[[2]]))
      m <- sparseMatrix(i = i,
                        j = j,
                        x = values,
                        dims = dim(object))
  else
      m <- sparseMatrix(i = i,
                        j = j,
                        x = values,
                        dims = dim(object),
                        dimnames = dn)
  return(m)
}

as.FLMatrix.FLVector <- function(object,connection=getConnection(object),sparse=TRUE,rows=length(object),cols=1)
{
  if(rows==length(object) && cols==1)
  {
    if(class(object@select)=="FLTableFunctionQuery" || ncol(object)>1) object <- store(object)
    select <- new(
        "FLSelectFrom",
        odbc_connection = connection, 
        db_name = object@select@db_name, 
        table_name = object@select@table_name, 
        variables=list(
            rowIdColumn=getVariables(object)[["obs_id_colname"]],
            colIdColumn="1",
            valueColumn=object@dimnames[[2]]),
        whereconditions=object@select@whereconditions,
        order = "")
    flm <- new("FLMatrix",
              select=select,
              dimnames=list(object@dimnames[[1]],
                            "1"))
    return(flm)
  }
  MID <- getMaxMatrixId(connection)
  k <- 1
  sqlstr <- ""
  colnames <- renameDuplicates(colnames(object))
  batchStore <- function(sqlstr)
  {
    sqlstr <- sqlstr[sqlstr!=""]
    sqlstr <- paste0(sqlstr,collapse=" UNION ALL ")
    vSqlStr <- paste0(" INSERT INTO ",
                    getRemoteTableName(result_db_name,
                                      result_matrix_table),
                    "\n",
                   gsub("'%insertIDhere%'",MID,sqlstr),
                    "\n")
    sqlSendUpdate(connection,
                  vSqlStr)
  }
  for (i in 1:cols)
  for (j in 1:rows)
  {
    a <- genRandVarName()
    if(ncol(object)>1)
    {
      if(k>ncol(object)) k <- 1 
      sqlstr0 <- paste0(" SELECT '%insertIDhere%' AS MATRIX_ID,",
                              j," AS rowIdColumn,",
                              i," AS colIdColumn,",a,".",
                              colnames[k]," AS valueColumn 
                       FROM(",constructSelect(object),") AS ",a)
    }
    else
    {
      if(k>nrow(object)) k <- 1 
      sqlstr0 <- paste0(" SELECT '%insertIDhere%' AS MATRIX_ID,",
                              j," AS rowIdColumn,",
                              i," AS colIdColumn,",a,".",
                              "vectorValueColumn AS valueColumn 
                       FROM(",constructSelect(object),") AS ",a,"  
                       WHERE ",a,".vectorIndexColumn in ",rownames(object)[k])
    }
    sqlstr <- c(sqlstr,sqlstr0)
    if(checkMaxQuerySize(sqlstr) && (!(i==cols && j==rows)))
    {
      batchStore(sqlstr)
      sqlstr <- ""
    }
    k <- k+1
  }
  batchStore(sqlstr)
  sqlstr <- ""

  # sqlstr <- paste0("INSERT INTO ",getRemoteTableName(getOption("ResultDatabaseFL"),getOption("ResultMatrixTableFL")),
  #                 " SELECT ",max_matrix_id_value,
  #                           ",floor(a.",object@obs_id_colname,"+0.1 MOD ",rows,")
  #                            ,a.",object@obs_id_colname,"-floor(a.",object@obs_id_colname,"+0.1 MOD ",rows,")
  #                            ,a.",object@dimnames[[2]],
  #                 " FROM ",object@db_name,".",object@table_name," AS a",
  #                 constructWhere(constraintsSQL(object)))
  return(FLMatrix(
            connection = connection,
            database = result_db_name, 
            matrix_table = result_matrix_table, 
            matrix_id_value = MID,
            matrix_id_colname = "MATRIX_ID", 
            row_id_colname = "rowIdColumn", 
            col_id_colname = "colIdColumn", 
            cell_val_colname = "valueColumn",
            ))
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
  VID <- getMaxVectorId(connection)
  sqlstr<-sapply(1:length(object),FUN=function(x) paste0("INSERT INTO ",
           getRemoteTableName(getOption("ResultDatabaseFL"),getOption("ResultVectorTableFL")),
           " SELECT ",VID," AS vectorIdColumn,",
                     x," AS vectorIndexColumn,",
                     object[x]," AS vectorValueColumn;"
                   ))
  
  retobj<-sqlSendUpdate(connection,
                              paste(sqlstr,
                                    collapse="\n"))

  #max_vector_id_value <<- max_vector_id_value + 1

  table <- FLTable(connection,
                 getOption("ResultDatabaseFL"),
                 getOption("ResultVectorTableFL"),
                 "VECTOR_INDEX",
                 whereconditions=paste0(getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL"),".VECTOR_ID = ",VID)
                 )

  return(table[,"vectorValueColumn"])
}

as.FLVector.FLMatrix <- function(object,connection=getConnection(object))
{
  flag3Check(connection)
  VID <- getMaxVectorId(connection)
  k <- 1
  sqlstr <- ""
  batchStore <- function(sqlstr)
  {
    sqlstr <- sqlstr[sqlstr!=""]
    sqlstr <- paste0(sqlstr,collapse=" UNION ALL ")
    vSqlStr <- paste0(" INSERT INTO ",
                    getRemoteTableName(result_db_name,
                                      result_vector_table),
                    "\n",
                   gsub("'%insertIDhere%'",VID,sqlstr),
                    "\n")
    sqlSendUpdate(connection,
                  vSqlStr)
  }
  colnames <- colnames(object)
  for(i in colnames)
  {
    a <- genRandVarName()
    sqlstr0 <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
                              k:(k+length(rownames(object))-1)," AS vectorIndexColumn,",
                              a,".valueColumn AS vectorValueColumn 
                       FROM(",constructSelect(object),") AS ",a,
                       " WHERE ",a,".rowIdColumn in ",rownames(object),
                       " AND ",a,".colIdColumn in ",i)
    sqlstr <- c(sqlstr,sqlstr0)
    if(checkMaxQuerySize(sqlstr) && i!=colnames[length(colnames)])
    {
      batchStore(sqlstr)
      sqlstr <- ""
    }
    k <- k+length(rownames(object))
  }

    batchStore(sqlstr)
    sqlstr <- ""
    table <- FLTable(getConnection(object),
                   result_db_name,
                   result_vector_table,
                   "vectorIndexColumn",
                   whereconditions=paste0(result_db_name,".",result_vector_table,".vectorIdColumn = ",VID)
                  )

    return(table[,"vectorValueColumn"])
}

	
