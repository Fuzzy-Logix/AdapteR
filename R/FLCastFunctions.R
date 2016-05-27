#' @include utilities.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

#' Converts FLMatrix object to vector in R
#' @export
as.vector.FLMatrix <- function(object,mode="any")
{
	temp_m <- as.matrix(object)
	return(as.vector(temp_m))
}

#' @export
as.vector.FLMatrixBind <- function(object,mode="any")
{
	temp_m <- as.matrix(object)
	return(as.vector(temp_m))
}

#' Converts FLVector object to vector in R
#' @export
as.vector.FLVector <- function(object,mode="any")
{
  #browser()
    vprev1 <- getOption("stringsAsFactors")
    vprev2 <- getOption("warn")
    options(stringsAsFactors=FALSE)
    options(warn=-1)
    if(ncol(object)==1)
        x <- as.data.frame.FLVector(object)[[1]]
    if(ncol(object)>1)
        x <- as.vector(as.data.frame.FLVector(object)[1,])
        if(!any(is.na(as.numeric(x))))
        x <- as.numeric(x)
    if(ncol(object)==1) vnames <- rownames(object)
    else vnames <- colnames(object)
    if(is.character(vnames) && !all(vnames==1:length(vnames)))
    names(x) <- vnames[1:length(x)]

    options(stringsAsFactors=vprev1)
    options(warn=0)
    return(x)
}

#' Converts in-database objects to a data frame in R
#' 
#' Caution: data is fetched into R session
#' @param x can be FLTable, FLVector or FLMatrix
#' @param ... any additional arguments
#' @return R data.frame object
#' @export
as.data.frame <- function(x, ...)
{
	UseMethod("as.data.frame",x)
}

#' @export
as.data.frame.FLTable <- function(x, ...){
    sqlstr <- constructSelect(x)
    sqlstr <- gsub("'%insertIDhere%'",1,sqlstr)
    tryCatch(D <- sqlQuery(getConnection(x),sqlstr),
      error=function(e){stop(e)})
    names(D) <- toupper(names(D))
    D <- plyr::arrange(D,D[["OBS_ID_COLNAME"]])
    ##browser()
    if(x@isDeep) {
        D <- reshape2::dcast(D, paste0(toupper("obs_id_colname"),
                             " ~ ",
                             toupper("var_id_colname")),
                   value.var = toupper("cell_val_colname"))
    } 
    i <- charmatch(rownames(x),D[[toupper("obs_id_colname")]],nomatch=0)
                                        # print(i)
    D <- D[i,]
    if(any(D[[toupper("obs_id_colname")]]!=1:nrow(D)))
        rownames(D) <- D[[toupper("obs_id_colname")]]
    D[[toupper("obs_id_colname")]] <- NULL
    return(D)
}

#' @export
as.data.frame.FLVector <- function(x, ...){
    sqlstr <- constructSelect(x)
    sqlstr <- gsub("'%insertIDhere%'",1,sqlstr)

   tryCatch(D <- sqlQuery(getConnection(x),sqlstr),
      error=function(e){stop(e)})
   
    names(D) <- toupper(names(D))
    vrownames <- rownames(x)
    vcolnames <- colnames(x)
    # if(ncol(x)<=1 && !(!x@isDeep && nrow(x)==1 && ncol(x)==1))
    #if(ncol(x)<=1 && class(x@select)!="FLTableFunctionQuery")
    if(ncol(x)<=1)
    {
      if(is.character(rownames(x)) && !all(rownames(x)==1:length(rownames(x))))
      vrownames<-1:length(rownames(x))
      if(is.character(colnames(x)) && !all(colnames(x)==1:length(colnames(x))))
      vcolnames<-1:length(colnames(x))
    }
    
     i <- charmatch(vrownames,D[[toupper("vectorIndexColumn")]],nomatch=0)
     if(x@isDeep) {
        if(length(colnames(x))>1)
        i <- charmatch(vcolnames,D[[toupper("vectorIndexColumn")]],nomatch=0)
    }
    D <- D[i,]
    if(x@isDeep) {
        if(length(colnames(x))>1)
         D <- reshape2::dcast(D, paste0(toupper("vectorIdColumn"),
                             " ~ ",
                             toupper("vectorIndexColumn")),
                   value.var = toupper("vectorValueColumn"))
    }
    if(any(D[[toupper("vectorIndexColumn")]]!=1:nrow(D)))
        rownames(D) <- renameDuplicates(D[[toupper("vectorIndexColumn")]])
    D[[toupper("vectorIndexColumn")]] <- NULL
    D[[toupper("vectorIdColumn")]] <- NULL
    return(D)
}

#' @export
as.data.frame.FLMatrix <- function(x,...)
{
  temp_m <- as.matrix(x)
  return(as.data.frame(temp_m))
}
##############################################################################################################
#' Converts in-database objects to a matrix in R
#' 
#' Caution: data is fetched into R session
#' @param x can be FLTable, FLVector or FLMatrix
#' @return R matrix object
#' @export
as.matrix <- function(x, ...)
{
	UseMethod("as.matrix",x)
}

#' @export
as.matrix.data.frame <- base::as.matrix.data.frame
#' @export
as.matrix.integer <- base::as.matrix.default
#' @export
as.matrix.numeric <- base::as.matrix.default


#' Converts input FLMatrix object to matrix in R
#' @export
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
#' @export
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

#' @export
as.matrix.FLMatrixBind <- as.matrix.FLMatrix


#' Converts FLVector object to a matrix in R
#' @export
as.matrix.FLVector <- function(obj)
{
	Rvector <- as.vector(obj)
	return(as.matrix(Rvector))
}

#' @export
as.matrix.FLTable <- function(x,...)
{
  temp_df <- as.data.frame(x)
  return(as.matrix(temp_df))
}


###############################################################################################################

#' @export
as.FLMatrix.Matrix <- function(object,sparse=TRUE,connection=NULL,...) {
    ##browser()
    if(!is.logical(sparse)) stop("sparse must be logical")
    if(is.null(connection)) connection <- getConnection(object)
    options(warn=-1)
    if(is.integer(as.vector(as.matrix(object))))
    tablename <- getOption("ResultIntMatrixTableFL")
    else if(is.numeric(as.vector(as.matrix(object))))
    tablename <- getOption("ResultMatrixTableFL")
    else if(is.character(as.vector(as.matrix(object))))
    tablename <- getOption("ResultCharMatrixTableFL")
    else stop("only integer,numeric and character type matrices allowed in as.FLMatrix\n")
    
        mwide <- Matrix::Matrix(object, sparse=TRUE)
        if(class(mwide)=="dsCMatrix")
        mwide <- as(mwide,"dgTMatrix")
        mdeep <- Matrix::summary(mwide)
        ## check for empty rows or columns
        ## and add a 0
        fillEmptyDims <- function(mdeep,dims)
        {
          i<-setdiff(1:dims[1],mdeep$i)
          j<-setdiff(1:dims[2],mdeep$j)
          ir<-c(rep(i[1],length(j)),i)
          jr<-c(j,rep(j[1],length(i)))
          if(length(ir)==0 && length(jr)==0) return(mdeep)
          if(is.na(ir)) ir <- rep(dims[1],length(jr))
          if(is.na(jr)) jr <- rep(dims[2],length(ir))
          sr <- Matrix::summary(Matrix::sparseMatrix(i=ir,j=jr,x=0))
          return(base::rbind(mdeep,sr))
        }
        mdeep <- fillEmptyDims(mdeep,dims=dim(object))
        ## insert one 0 at nrow,ncol for
        ## "storing" matrix dimensions
        # if(object[nrow(object),ncol(object)]==0)
        #     mdeep <- base::rbind(mdeep,
        #                    c(i=nrow(object),j=ncol(object),
        #                      x=0))
        MID <- getMaxMatrixId(connection)
        remoteTable <- getRemoteTableName(
            getOption("ResultDatabaseFL"),
            getOption("ResultMatrixTableFL"))

        #analysisID <- paste0("AdapteR",remoteTable,MID)
        if(class(connection)=="RODBC")
        {
          sqlstatements <-
            base::apply(mdeep,1,
                        function(r)
                            paste0(" INSERT INTO ",
                                   remoteTable,
                                   " (matrix_id, rowIdColumn, colIdColumn, valueColumn) VALUES (",
                                   paste0(c(MID,r), collapse=", "),
                                   ");"))
        ##flag1Check(connection)
        retobj<-sqlSendUpdate(connection,
                              paste(sqlstatements,
                                    collapse="\n"))
        }
        else if(class(connection)=="JDBCConnection")
        {
          mdeep <- base::cbind(MATRIX_ID=as.integer(MID),mdeep)
          mdeep <- as.data.frame(mdeep)
          colnames(mdeep) <- c("MATRIX_ID","rowIdColumn","colIdColumn","valueColumn")
          t <- as.FLTable.data.frame(mdeep,connection,
            getOption("ResultMatrixTableFL"),1,drop=FALSE)
        }
        mydimnames <- dimnames(object)
        mydims <- dim(object)
        ##print(mydimnames)
        
        mapTable <- NULL
        for(i in 1:length(mydimnames))
            if(is.character(mydimnames[[i]])){
                mapTable <- getOption("MatrixNameMapTableFL")
                mydimnames[[i]] <- storeVarnameMapping(
                    connection,
                    mapTable,
                    MID,
                    i,
                    mydimnames[[i]])
            }

        return(FLMatrix(
            connection = connection,
            database = getOption("ResultDatabaseFL"),
            table_name = getOption("ResultMatrixTableFL"),
            map_table = mapTable,
            matrix_id_value = MID,
            matrix_id_colname = "MATRIX_ID",
            row_id_colname = "rowIdColumn",
            col_id_colname = "colIdColumn",
            cell_val_colname = "valueColumn",
            dim = mydims,
            dimnames = mydimnames))
}

#' Casting to FLMatrix
#'
#' Converts input to a FLMatrix object
##' @param object matrix, vector, data frame, sparseMatrix, FLVector which
##' needs to be casted to FLMatrix and inserted in-database
##' @param connection ODBC/JDBC connection object
##' @param sparse logical if sparse representation to be used
##' @param ... additional arguments like nr number of rows in resulting FLMatrix
##' nc number of columns in resulting FLMatrix.
##' nr and nc inputs are applicable only in case of vector,FLVector
#' @return FLMatrix object after casting.
#' @export

setGeneric("as.FLMatrix", function(object,sparse=TRUE,...) {
    standardGeneric("as.FLMatrix")
})
setMethod("as.FLMatrix", signature(object = "matrix",
                                   sparse="missing"),
          function(object,sparse=TRUE)
              as.FLMatrix.Matrix(object,sparse=sparse))
## setMethod("as.FLMatrix", signature(object = "matrix",
##                                    sparse="JDBCConnection"),
##           function(object,sparse){
##               warning("remove connection from cast")
##               stop()
##               as.FLMatrix.Matrix(object,connection=sparse)
##               })
setMethod("as.FLMatrix", signature(object = "matrix",
                                   sparse="logical"),
          function(object,sparse)
              as.FLMatrix.Matrix(object,sparse=sparse))
setMethod("as.FLMatrix", signature(object = "dgeMatrix",
                                   sparse="logical"),
          function(object,sparse=TRUE)
              as.FLMatrix.Matrix(object,sparse=sparse))
setMethod("as.FLMatrix", signature(object = "dgeMatrix",
                                   sparse="missing"),
          function(object,sparse=TRUE)
              as.FLMatrix.Matrix(object,sparse=sparse))
setMethod("as.FLMatrix", signature(object = "dgCMatrix",
                                   sparse="logical"),
          function(object,sparse=TRUE)
              as.FLMatrix.Matrix(object,sparse=sparse))
setMethod("as.FLMatrix", signature(object = "dgCMatrix",
                                   sparse="missing"),
          function(object,sparse=TRUE)
              as.FLMatrix.Matrix(object,sparse=sparse))
setMethod("as.FLMatrix", signature(object = "dgTMatrix",
                                   sparse="logical"),
          function(object,sparse=TRUE)
              as.FLMatrix.Matrix(object,sparse=sparse))
setMethod("as.FLMatrix", signature(object = "dgTMatrix",
                                   sparse="missing"),
          function(object,sparse=TRUE)
              as.FLMatrix.Matrix(object,sparse=sparse))
setMethod("as.FLMatrix", signature(object = "dsCMatrix",
                                   sparse="logical"),
          function(object,sparse=TRUE)
              as.FLMatrix.Matrix(object,sparse=sparse))
setMethod("as.FLMatrix", signature(object = "dsCMatrix",
                                   sparse="missing"),
          function(object,sparse=TRUE)
              as.FLMatrix.Matrix(object,sparse=sparse))
setMethod("as.FLMatrix", signature(object = "dtCMatrix",
                                   sparse="missing"),
          function(object,sparse=TRUE)
              as.FLMatrix.Matrix(object,sparse=sparse))
###########################################################################
setMethod("as.FLMatrix", signature(object = "vector",
                                   sparse="logical"),
          function(object,sparse=TRUE,rows=length(object),cols=1,...)
              as.FLMatrix.vector(object,sparse,rows,cols,...))
setMethod("as.FLMatrix", signature(object = "vector",
                                   sparse="missing"),
          function(object,sparse=TRUE,rows=length(object),cols=1,...)
              as.FLMatrix.vector(object,sparse=TRUE,rows,cols,...))
setMethod("as.FLMatrix", signature(object = "data.frame",
                                   sparse="logical"),
          function(object,sparse=TRUE)
              as.FLMatrix.data.frame(object,sparse=sparse))
setMethod("as.FLMatrix", signature(object = "data.frame",
                                   sparse="missing"),
          function(object,sparse=TRUE)
              as.FLMatrix.data.frame(object,sparse=TRUE))
setMethod("as.FLMatrix", signature(object = "FLVector",
                                   sparse="logical"),
          function(object,sparse=TRUE,...)
              as.FLMatrix.FLVector(object,sparse,...))
setMethod("as.FLMatrix", signature(object = "FLVector",
                                   sparse="missing"),
          function(object,sparse=TRUE,...)
              as.FLMatrix.FLVector(object,sparse=TRUE,...))


#' @export
as.sparseMatrix.FLMatrix <- function(object) {
    #browser()
    sqlstr <- gsub("'%insertIDhere%'",1,constructSelect(object, joinNames=FALSE))
    tryCatch(valuedf <- sqlQuery(getConnection(object), sqlstr),
      error=function(e){stop(e)})
    i <- valuedf$rowIdColumn
    j <- valuedf$colIdColumn
    i <- FLIndexOf(i,rownames(object))
    j <- FLIndexOf(j,colnames(object))

  dn <- dimnames(object)
    if(any(is.na(c(i,j))))
        browser()
  values <- valuedf$valueColumn
  if(is.null(values))
      m <- Matrix::sparseMatrix(i = i,
                        j = j,
                        x = i,
                        dims = dim(object),
                        dimnames = dn)
  else if(is.null(dn[[1]]) & is.null(dn[[2]]))
      m <- Matrix::sparseMatrix(i = i,
                        j = j,
                        x = values,
                        dims = dim(object))
  else
      m <- Matrix::sparseMatrix(i = i,
                        j = j,
                        x = values,
                        dims = dim(object),
                        dimnames = dn)
  return(m)
}

#' @export
as.FLMatrix.FLVector <- function(object,sparse=TRUE,
                rows=length(object),cols=1,connection=NULL)
{
  if(is.null(connection)) connection <- getConnection(object)
  ##Get names of vector
  if(ncol(object)>1)
  object <- store(object)

  if(ncol(object)==1) vnames <- object@dimnames[[1]]
  else vnames <- object@dimnames[[2]]

  if(class(object@select)=="FLTableFunctionQuery"
    && !all(vnames==1:length(vnames)))
  object <- store(object)

  if(!missing(rows) && missing(cols) && rows!=length(object))
  cols <- base::ceiling(length(object)/rows)
  if(!missing(cols) && missing(rows) && cols!=1)
  rows <- base::ceiling(length(object)/cols)

  k <- base::ceiling((rows*cols)/length(object))-1
  a<-genRandVarName()
  sqlstr <- paste0(" SELECT '%insertIDhere%' AS MATRIX_ID,",
                             a,".vectorIndexColumn + ",(0:k)*length(object),
                             " - (CAST((",a,".vectorIndexColumn + ",(0:k)*length(object),
                              "-0.355)/",rows," AS INT)*",rows,") AS rowIdColumn,",
                            " CAST((",a,".vectorIndexColumn + ",(0:k)*length(object),
                              "-0.355)/",rows," AS INT)+1 AS colIdColumn,",
                             a,".vectorValueColumn AS valueColumn",
                    " FROM(",constructSelect(object),") AS ",a,
                    " WHERE ",a,".vectorIndexColumn + ",(0:k)*length(object)," <= ",rows*cols)

  batchStore <- function(sqlstr,MID)
  {
    if(utils::object.size(paste0(sqlstr,collapse=" UNION ALL "))>300000)
    {
      newindex <- base::ceiling(length(sqlstr)/2)
      batchStore(sqlstr[1:newindex],MID)
      batchStore(sqlstr[(newindex+1):length(sqlstr)],MID)
    }
    else
    {
      sqlstr <- paste0(sqlstr,collapse=" UNION ALL ")
      vSqlStr <- paste0(" INSERT INTO ",
                    getRemoteTableName(getOption("ResultDatabaseFL"),
                                      getOption("ResultMatrixTableFL")),
                    "\n",
                   gsub("'%insertIDhere%'",MID,sqlstr),
                    "\n")
      sqlSendUpdate(connection,vSqlStr)
    }
  }

  if(utils::object.size(paste0(sqlstr,collapse=" UNION ALL "))>300000)
  {
    MID <- getMaxMatrixId(connection)
    batchStore(sqlstr,MID)
    return(FLMatrix(
            database = getOption("ResultDatabaseFL"),
            table_name = getOption("ResultMatrixTableFL"),
            map_table = NULL,
            matrix_id_value = MID,
            matrix_id_colname = "MATRIX_ID",
            row_id_colname = "rowIdColumn",
            col_id_colname = "colIdColumn",
            cell_val_colname = "valueColumn",
            connection = connection
            ))
  }

  sqlstr <- paste0(sqlstr,collapse=" UNION ALL ")
  tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = connection,
                        variables=list(
                            rowIdColumn="rowIdColumn",
                            colIdColumn="colIdColumn",
                            valueColumn="valueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

  flm <- new("FLMatrix",
              select= tblfunqueryobj,
              dim = c(rows,cols),
              dimnames=list(1:rows,1:cols))
  return(flm)
}

#' @export
as.FLMatrix.vector <- function(object,
                               sparse=TRUE,
                               rows=length(object),
                               cols=1,
                               connection=NULL
                               )
{
  temp_m <- Matrix::Matrix(object,rows,cols,sparse=TRUE)
  return(as.FLMatrix(temp_m))
}

#' @export
as.FLMatrix.data.frame <- function(object,
                                   sparse=TRUE,
                                   connection=NULL)
{
  temp_m <- Matrix::Matrix(as.matrix(object),sparse=TRUE)
  return(as.FLMatrix(temp_m))
}


######################################################################################################################
#' casting to FLVector
#'
#' Converts input \code{obj} to FLVector object
#' @param object matrix, vector, data frame, sparseMatrix, FLMatrix which
#' needs to be casted to FLVector
#' @param connection ODBC/JDBC connection object
#' @param ... additional arguments like size
#' @param size number of elements in resulting FLVector.
#' size input is not applicable only in case of FLMatrix
#' @return FLVector object after casting.
#' @export
setGeneric("as.FLVector", function(object,...) {
    standardGeneric("as.FLVector")
})
setMethod("as.FLVector", signature(object = "vector"),
          function(object,connection)
              as.FLVector.vector(object))
setMethod("as.FLVector", signature(object = "matrix"),
          function(object,connection)
              as.FLVector.vector(object))
setMethod("as.FLVector", signature(object = "dgeMatrix"),
          function(object,connection)
              as.FLVector.vector(object))
setMethod("as.FLVector", signature(object = "dgCMatrix"),
          function(object,connection)
              as.FLVector.vector(object))
setMethod("as.FLVector", signature(object = "dsCMatrix"),
          function(object,connection)
              as.FLVector.vector(object))
setMethod("as.FLVector", signature(object = "dgTMatrix"),
          function(object,connection)
              as.FLVector.vector(object))
setMethod("as.FLVector", signature(object = "data.frame"),
          function(object,connection)
              as.FLVector.vector(as.matrix(object)))
setMethod("as.FLVector", signature(object = "FLMatrix"),
          function(object,connection)
              as.FLVector.FLMatrix(object))
setMethod("as.FLVector", signature(object = "FLMatrix"),
          function(object)
              as.FLVector.FLMatrix(object))

#' @export
as.FLVector.vector <- function(object,connection=getConnection(object))
{
  flag3Check(connection)
  oldOption <- getOption("warn")
  options(warn=-1)
  if(!any(is.na(as.integer(object))) && 
    all(as.integer(object)==object)){
    tablename <- getOption("ResultIntVectorTableFL")
    object <- as.integer(object)
  }
  else if(is.numeric(object))
    tablename <- getOption("ResultVectorTableFL")
  else if(is.character(object))
    tablename <- getOption("ResultCharVectorTableFL")
  else stop("only numeric,integer and character vectors supported in as.FLVector")

  options(warn=oldOption)
  VID <- getMaxVectorId(connection,tablename)

  if(class(connection)=="RODBC")
  {
    sqlstr<-sapply(1:length(object),FUN=function(x) paste0("INSERT INTO ",
           getRemoteTableName(getOption("ResultDatabaseFL"),tablename),
           " SELECT ",VID," AS vectorIdColumn,",
                     x," AS vectorIndexColumn,",
                     ifelse(is.character(object),fquote(object[x]),object[x]),
                     " AS vectorValueColumn;"
                   ))
    retobj<-sqlSendUpdate(connection,
                              paste(sqlstr,
                                    collapse="\n"))
  }
  else if(class(connection)=="JDBCConnection")
  {
    vdataframe <- data.frame(vectorIdColumn=as.integer(VID),
                            vectorIndexColumn=as.integer(1:length(object)),
                            vectorValueColumn=as.vector(object))
    t <- as.FLTable.data.frame(vdataframe,connection,tablename,1,drop=FALSE)
  }
  select <- new(
                "FLSelectFrom",
                connection = connection, 
                database = getOption("ResultDatabaseFL"), 
                table_name = c(flt=tablename),
                variables = list(
                        obs_id_colname = "flt.vectorIndexColumn"),
                whereconditions=paste0(getOption("ResultDatabaseFL"),".",
                  tablename,".vectorIdColumn = ",VID),
                order = "")

  if(!is.null(names(object)) && !all(names(object)==1:length(object)))
  newnames <- as.character(names(object))
  else newnames <- 1:length(object)
  return(new("FLVector",
                select=select,
                dimnames=list(newnames,"vectorValueColumn"),
                isDeep=FALSE))
}

#' @export
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
                    getRemoteTableName(getOption("ResultDatabaseFL"),
                                     getOption("ResultVectorTableFL")),
                    "\n",
                   gsub("'%insertIDhere%'",VID,sqlstr),
                    "\n")
    sqlSendUpdate(connection,
                  vSqlStr)
  }
  colnames <- colnames(object)
  if(is.null(colnames(object))) 
  colnames <- 1:object@dim[[2]]
  else if(!is.null(names(colnames)))
  colnames <- names(colnames)
  object@dimnames[[2]] <- colnames

  rownames <- rownames(object)
  if(is.null(rownames(object))) 
  rownames <- 1:object@dim[[1]]
  else if(!is.null(names(rownames)))
  rownames <- names(rownames)
  object@dimnames[[1]] <- rownames
  ## FOR loop used only for generating SQL query.
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
    table <- FLTable(connection = getConnection(object),
                   getOption("ResultDatabaseFL"),
                   getOption("ResultVectorTableFL"),
                   "vectorIndexColumn",
                   whereconditions=paste0(getOption("ResultDatabaseFL"),
                    ".",getOption("ResultVectorTableFL"),".vectorIdColumn = ",VID)
                  )

    return(table[,"vectorValueColumn"])
}

#####################################################################################################################
#' casting to FLTable
#'
#' Converts input \code{obj} to FLVector object
#' @param object data frame which
#' needs to be casted to FLTable
#' @param connection ODBC/JDBC connection object
#' @param ... additional arguments like size
#' @return FLTable object after casting.
#' @export
setGeneric("as.FLTable", function(object,...) {
    standardGeneric("as.FLTable")
})
setMethod("as.FLTable", signature(object = "data.frame"),
          function(object,...)
              as.FLTable.data.frame(object,...))

#' @export
as.FLTable.data.frame <- function(object,
                                  connection=getOption("connectionFL"),
                                  tableName,
                                  uniqueIdColumn=0,
                                  drop=TRUE,
                                  batchSize=10000){
  if(missing(tableName))
  tableName <- genRandVarName()
  if(uniqueIdColumn==0 && is.null(rownames(object)) || length(rownames(object))==0)
  stop("please provide primary key of the table as rownames when uniqueIdColumn=0")
  if(uniqueIdColumn==0)
  {
    object <- base::cbind(rownames=rownames(object),object)
    obsIdColname <- "rownames"
  }
  else if(is.numeric(uniqueIdColumn))
  {
    uniqueIdColumn <- as.integer(uniqueIdColumn)
    if(uniqueIdColumn < 0 || uniqueIdColumn > ncol(object))
    stop("uniqueIdColumn is out of bounds")
    else
    obsIdColname <- colnames(object)[uniqueIdColumn]
  }
  if(class(connection)=="RODBC")
  {
    tryCatch(RODBC::sqlSave(connection,object,tableName,rownames=FALSE,safer=drop),
      error=function(e){stop(e)})
  }
  else if(class(connection)=="JDBCConnection")
  {
    vcols <- ncol(object)
    #vcolnames <- apply(object,2,class) ## wrong results with apply!
    vcolnames <- c()
    #browser()
    for(i in 1:vcols)
    vcolnames <- c(vcolnames,class(object[[i]]))
    names(vcolnames) <- colnames(object)
    # Changing any factors to characters
    object[,vcolnames=="factor"] <- apply(as.data.frame(object[,vcolnames=="factor"]),
                                    2,as.character)
    vcolnames[vcolnames=="factor"] <- "character"
    # Removing "." if any from colnames
    names(vcolnames) <- gsub("\\.","",names(vcolnames),fixed=TRUE)
    vcolnamesCopy <- vcolnames
    vcolnamesCopy[vcolnamesCopy=="character"] <- " VARCHAR(255) "
    vcolnamesCopy[vcolnamesCopy=="numeric"] <- " FLOAT "
    vcolnamesCopy[vcolnamesCopy=="integer"] <- " INT "
    vcolnamesCopy[vcolnamesCopy=="logical"] <- " VARCHAR(255) "
    if(!all(vcolnamesCopy %in% c(" VARCHAR(255) "," INT "," FLOAT "))==TRUE)
    stop("currently class(colnames(object)) can be only character,numeric,integer. Use casting if possible")

    if(drop)
    {
      if(RJDBC::dbExistsTable(connection,tableName))
      t<-sqlSendUpdate(connection,paste0("drop table ",
                    getOption("ResultDatabaseFL"),".",tableName,";"))
      vstr <- paste0(names(vcolnamesCopy)," ",vcolnamesCopy,collapse=",")
      vstr <- paste0(names(vcolnamesCopy)," ",vcolnamesCopy,collapse=",")
      sql <- paste0("create table ",getOption("ResultDatabaseFL"),".",tableName,"(",vstr,");")
      if (getOption("debugSQL")) cat(sql)
      t<-RJDBC::dbSendUpdate(connection,sql)
      if(!is.null(t)) stop(paste0("colnames unconvenional. Error Mssg is:-",t))
    }
    
    .jcall(connection@jc,"V","setAutoCommit",FALSE)
    sqlstr <- paste0("INSERT INTO ",getOption("ResultDatabaseFL"),".",
                tableName," VALUES(",paste0(rep("?",vcols),collapse=","),")")
    ps = .jcall(connection@jc,"Ljava/sql/PreparedStatement;","prepareStatement",sqlstr)
    myinsert <- function(namedvector,x){
                  vsetvector <- c()
                  vsetvector[" VARCHAR(255) "] <- "setString"
                  vsetvector[" FLOAT "] <- "setFloat"
                  vsetvector[" INT "] <- "setInt"
                  for(i in 1:length(namedvector))
                  {
                    .jcall(ps,"V",vsetvector[namedvector[i]],as.integer(i),
                      if(namedvector[i]==" VARCHAR(255) ") as.character(x[i])
                      else if(namedvector[i]==" FLOAT ") .jfloat(x[i])
                      else as.integer(x[i]))
                  }
                  .jcall(ps,"V","addBatch")
                }

    ##Chunking
    {
      if(batchSize>10000)
      {
        batchSize <- 10000
        cat("using max batchSize=10000")
      }
      k <- 1
      vnrow <- nrow(object)
      while(k <= vnrow)
      {
        j <- k + (batchSize-1)
        if(j > vnrow) j <- vnrow
        vsubset <- object[k:j,]
        apply(vsubset,1,function(x) myinsert(vcolnamesCopy,x))
        tryCatch(.jcall(ps,"[I","executeBatch"),
          error=function(e){stop("may be repeating primary key or bad column format.Error mssg recieved is:",e)})
        RJDBC::dbCommit(connection)
        k <- k + batchSize
      }
    }
    .jcall(connection@jc,"V","setAutoCommit",TRUE)
  }

  return(FLTable(getOption("ResultDatabaseFL"),
                  tableName,
                  obsIdColname
                  ))
}
