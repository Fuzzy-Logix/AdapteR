#' @include FLMatrix.R
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
        x <- as.vector(as.matrix(as.data.frame.FLVector(object)[1,]))
    # if(!any(is.na(as.numeric(x))) && !is.logical(x))
    # x <- as.numeric(x)

    ## Adjust Return Type:
    vTypeMapping <- c(as.logical="logical",
                      as.character="character",
                      as.integer="integer")
    vfunc <- names(vTypeMapping)[vTypeMapping==typeof(object)]
    if(length(vfunc)>0)
        x <- do.call(vfunc,list(x))
    
    if(ncol(object)==1) vnames <- rownames(object)
    else vnames <- colnames(object)
    if(is.character(vnames) && !all(vnames==1:length(vnames)))
    names(x) <- vnames[1:length(x)]

    options(stringsAsFactors=vprev1)
    options(warn=vprev2)
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
    #browser()
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
    ## For sparse deep table
    D[is.na(D)] <- 0
    return(D)
}

#' @export
as.data.frame.FLVector <- function(x, ...){
    sqlstr <- constructSelect(x)
    sqlstr <- gsub("'%insertIDhere%'",1,sqlstr)
    #browser()

   tryCatch(D <- sqlQuery(getConnection(x),sqlstr),
      error=function(e){stop(e)})
   
    names(D) <- toupper(names(D))

    x <- populateDimnames(x)
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
        MID <- getMaxMatrixId(vconnection=connection,
                              vtable=tablename)
        remoteTable <- tablename

        #analysisID <- paste0("AdapteR",remoteTable,MID)
        if(is.ODBC())
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
        else if(is.JDBC())
        {
          mdeep <- base::cbind(MATRIX_ID=as.integer(MID),mdeep)
          mdeep <- as.data.frame(mdeep)
          colnames(mdeep) <- c("MATRIX_ID","rowIdColumn","colIdColumn","valueColumn")
          t <- as.FLTable.data.frame(mdeep,connection,
                                    tablename,1,drop=FALSE)
        }
        mydimnames <- dimnames(object)
        mydims <- dim(object)
        ##print(mydimnames)
        
        mapTable <- NULL
        for(i in 1:length(mydimnames))
            if(is.character(mydimnames[[i]])){
                mapTable <- getOption("NameMapTableFL")
                mydimnames[[i]] <- storeVarnameMapping(
                    connection,
                    mapTable,
                    MID,
                    i,
                    mydimnames[[i]])
            }

        return(FLMatrix(
            connection = connection,
            table_name = tablename,
            map_table = mapTable,
            matrix_id_value = MID,
            matrix_id_colname = "MATRIX_ID",
            row_id_colname = "rowIdColumn",
            col_id_colname = "colIdColumn",
            cell_val_colname = "valueColumn",
            dims = mydims,
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

setMethod("as.FLMatrix", signature(object = "dpoMatrix",
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

setMethod("as.FLMatrix",signature(object="FLTable"),
          function(object,sparse=TRUE,...)
            as.FLMatrix.FLTable(object=object,
                                sparse=sparse,...))
###########################################################################


#' @export
setGeneric("as.R", function(flobject) standardGeneric("as.R"))
setMethod("as.R","FLMatrix", function(flobject) as.matrix(flobject))
setMethod("as.R","FLTable", function(flobject) as.data.frame(flobject))
setMethod("as.R","environment", function(flobject) as.REnvironment(flobject))
setMethod("as.R","FLVector", function(flobject) as.vector(flobject))

#' @export
setGeneric("as.FL", function(object) standardGeneric("as.FL"))
setMethod("as.FL","numeric", function(object) as.FLVector(object))
setMethod("as.FL","complex", function(object) stop("complex numbers not currently supported."))
setMethod("as.FL","character", function(object) as.FLVector(object))
setMethod("as.FL","vector", function(object) as.FLVector(object))
setMethod("as.FL","matrix", function(object) as.FLMatrix(object))
setMethod("as.FL","dpoMatrix", function(object) as.FLMatrix(object))
setMethod("as.FL","dsCMatrix", function(object) as.FLMatrix(object))
setMethod("as.FL","dgCMatrix", function(object) as.FLMatrix(object))
setMethod("as.FL","dgeMatrix", function(object) as.FLMatrix(object))
setMethod("as.FL","data.frame", function(object) as.FLTable(object))
setMethod("as.FL","environment", function(object) as.FLEnvironment(object))

as.REnvironment<-function(FLenv){
  Renv<-new.env()
  for(n in ls(FLenv)){
      object <- get(n,envir = FLenv)
      assign(n, as.R(object), envir=Renv)
  }
  return(Renv)
}

as.FLEnvironment <- function(Renv){
    FLenv <- new.env(parent = parent.env(Renv))
    for(n in ls(envir = Renv)){
        object <- get(n,envir = Renv)
        assign(n, as.FL(object), envir=FLenv)
    }
    FLenv
}


#' @export
as.sparseMatrix.FLMatrix <- function(object) {
    #browser()
    sqlstr <- gsub("'%insertIDhere%'",1,constructSelect(object, joinNames=FALSE))
    tryCatch(valuedf <- sqlQuery(getConnection(object), sqlstr),
      error=function(e){stop(e)})
    i <- valuedf[[object@dimColumns[[1]]]]
    j <- valuedf[[object@dimColumns[[2]]]]
    i <- FLIndexOf(i,rownames(object))
    j <- FLIndexOf(j,colnames(object))

    dn <- dimnames(object)
    if(any(is.na(c(i,j))))
        browser()
    values <- valuedf[[object@dimColumns[[3]]]]

  if(is.factor(values))
  return(matrix(values,dim(object),
          dimnames=dn))
  else if(is.logical(values)){
    vsummary <- base::rbind(Matrix::summary(Matrix(TRUE,
                                                dim(object)[1],
                                                dim(object)[2],
                                                sparse=TRUE)),
                            Matrix::summary(Matrix::sparseMatrix(i=i,
                              j=j,
                              x=values,
                              dims=dim(object)))
                            )
    vsparseRes <- Matrix::sparseMatrix(i=vsummary$i,
                                      j=vsummary$j,
                                      x=vsummary$x,
                                      dims=dim(object),
                                      dimnames=dn,
                                      use.last.ij = TRUE)
    return(matrix(vsparseRes,
                  dim(object),
                  dimnames=dn))
  }

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

  if(ncol(object)==1) vnames <- object@Dimnames[[1]]
  else vnames <- object@Dimnames[[2]]

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
      vSqlStr <- paste0(" INSERT INTO ", getOption("ResultMatrixTableFL"),
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

  flm <- newFLMatrix(
              select= tblfunqueryobj,
              dims = c(rows,cols),
              Dimnames=list(1:rows,1:cols),
              type=typeof(object))
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

as.FLMatrix.FLTable <- function(object,
                                sparse=TRUE,...){
  object <- setAlias(object,"")
  if(!object@isDeep)
  object <- wideToDeep(object=object)[["table"]]

  vdimnames <- lapply(dimnames(object),
                  function(x){
                      if(all(x==1:length(x)))
                      return(NULL)
                      else return(x)
                  })
  return(FLMatrix(table_name=object@select@table_name,
                  row_id_colname=getVariables(object)[["obs_id_colname"]],
                  col_id_colname=getVariables(object)[["var_id_colname"]],
                  cell_val_colname=getVariables(object)[["cell_val_colname"]],
                  dimnames=vdimnames,
                  whereconditions=object@select@whereconditions))
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
    ##flag3Check(connection)
  if(!is.null(names(object)) && !all(names(object)==1:length(object)))
  newnames <- as.character(names(object))
  else newnames <- 1:length(object)

  if(is.factor(object))
    object <- as.character(object)
  if(is.logical(object))
  tablename <- getOption("ResultCharVectorTableFL")
  else if(suppressWarnings(!any(is.na(as.integer(object))) && 
    all(as.integer(object)==object) &&
    !is.character(object))){
    tablename <- getOption("ResultIntVectorTableFL")
    object <- as.integer(object)
  }
  else if(is.numeric(object))
    tablename <- getOption("ResultVectorTableFL")
  else if(is.character(object))
    tablename <- getOption("ResultCharVectorTableFL")
  else stop("only numeric,integer and character vectors supported in as.FLVector")

  VID <- getMaxVectorId(connection,tablename)

  #vobjcopy <- ifelse(is.character(object),fquote(object[x]),object[x])
  #object <- c(1,"NULL")
  if(is.ODBC())
  {
    sqlstr<-sapply(1:length(object),FUN=function(x) paste0("INSERT INTO ",
           tablename,
           " SELECT ",VID," AS vectorIdColumn,",
                     x," AS vectorIndexColumn,",
                     ifelse(tablename==getOption("ResultCharVectorTableFL"),
                            fquote(object[x]),
                            object[x]),
                     " AS vectorValueColumn;"
                   ))
    retobj<-sqlSendUpdate(connection,
                              paste(sqlstr,
                                    collapse="\n"))
  }
  else if(is.JDBC())
  {
    #browser()
    vdataframe <- data.frame(vectorIdColumn=as.integer(VID),
                            vectorIndexColumn=as.integer(1:length(object)),
                            vectorValueColumn=as.vector(object))
    t <- as.FLTable.data.frame(vdataframe,connection,tablename,1,drop=FALSE)
  }
  select <- new("FLSelectFrom",
                connection = connection, 
                table_name = c(flt=tablename),
                variables = list(
                        obs_id_colname = "flt.vectorIndexColumn"),
                whereconditions=paste0(tablename,".vectorIdColumn = ",VID),
                order = "")

  return(newFLVector(
                select=select,
                Dimnames=list(newnames,"vectorValueColumn"),
                isDeep=FALSE,
                type=typeof(object)))
}

#' @export
as.FLVector.FLMatrix <- function(object,connection=getConnection(object))
{
    ##flag3Check(connection)
  VID <- getMaxVectorId(connection)
  k <- 1
  sqlstr <- ""
  batchStore <- function(sqlstr)
  {
    sqlstr <- sqlstr[sqlstr!=""]
    sqlstr <- paste0(sqlstr,collapse=" UNION ALL ")
    vSqlStr <- paste0(" INSERT INTO ",getOption("ResultVectorTableFL"),
                    "\n",
                   gsub("'%insertIDhere%'",VID,sqlstr),
                    "\n")
    sqlSendUpdate(connection,
                  vSqlStr)
  }
  colnames <- colnames(object)
  if(is.null(colnames(object))) 
  colnames <- 1:object@dims[[2]]
  else if(!is.null(names(colnames)))
  colnames <- names(colnames)
  object@Dimnames[[2]] <- colnames

  rownames <- rownames(object)
  if(is.null(rownames(object))) 
  rownames <- 1:object@dims[[1]]
  else if(!is.null(names(rownames)))
  rownames <- names(rownames)
  object@Dimnames[[1]] <- rownames
  ## FOR loop used only for generating SQL query.
  for(i in colnames)
  {
    a <- genRandVarName()
    sqlstr0 <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
                              k:(k+length(rownames(object))-1)," AS vectorIndexColumn,",
                              a,".valueColumn AS vectorValueColumn 
                       FROM(",constructSelect(object),") AS ",a,
                       " WHERE ",a,".",object@dimColumns[[1]]," in ",rownames(object),
                         " AND ",a,".",object@dimColumns[[2]]," in ",i)
    sqlstr <- c(sqlstr,sqlstr0)
    if(checkQueryLimits(sqlstr) && i!=colnames[length(colnames)])
    {
      batchStore(sqlstr)
      sqlstr <- ""
    }
    k <- k+length(rownames(object))
  }
    batchStore(sqlstr)
    sqlstr <- ""
    table <- FLTable(connection = getConnection(object),
                     table=getOption("ResultVectorTableFL"),
                     obs_id_colname="vectorIndexColumn",
                     whereconditions=paste0(getOption("ResultVectorTableFL"),".vectorIdColumn = ",VID)
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
setMethod("as.FLTable",signature(object="FLMatrix"),
          function(object,...)
          as.FLTable.FLMatrix(object=object,...))


as.FLTable.FLMatrix <- function(object=object,...){
  object <- setAlias(object,"")
  return(FLTable(table=object@select@table_name,
                obs_id_colname=getVariables(object)[["rowIdColumn"]],
                var_id_colnames=getVariables(object)[["colIdColumn"]],
                cell_val_colname=getVariables(object)[["valueColumn"]],
                whereconditions=object@select@whereconditions))
}
#' @export
as.FLTable.data.frame <- function(object,
                                  connection=getFLConnection(),
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
    vrownames <- rownames(object)
    if(!any(is.na(as.numeric(vrownames))))
    vrownames <- as.numeric(vrownames)
    object <- base::cbind(rownames=vrownames,object)
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
  connection <- getRConnection(connection)
  if(is.ODBC(connection))
  {
    vcolnames <- gsub("\\.","",colnames(object),fixed=FALSE)
    if(drop)
        t <- dropTable(pTableName=tableName)
    tryCatch(RODBC::sqlSave(channel=connection,
                            dat=object,
                            tablename=tableName,
                            rownames=FALSE),
      error=function(e){stop(e)})
  }
  else if(is.JDBC(connection))
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
    object[,as.logical(vcolnames=="logical")] <- apply(as.data.frame(object[,as.logical(vcolnames=="logical")]),
                                    2,as.character)
    vcolnames[vcolnames=="factor"] <- "character"
    # Removing "." if any from colnames
    names(vcolnames) <- gsub("\\.","",names(vcolnames),fixed=FALSE)
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
      t<-sqlSendUpdate(connection,paste0("drop table ",tableName,";"))
      vstr <- paste0(names(vcolnamesCopy)," ",vcolnamesCopy,collapse=",")
      t <- createTable(pTableName=tableName,
                      pColNames=names(vcolnamesCopy),
                      pColTypes=vcolnamesCopy,
                      pTemporary=FALSE)
      # sql <- paste0("create table ",getOption("ResultDatabaseFL"),".",tableName,"(",vstr,");")
      # if (getOption("debugSQL")) cat(sql)
      # t<-RJDBC::dbSendUpdate(connection,sql)
      updateMetaTable(pTableName=t,
                    pType="wideTable")
    }
    
    .jcall(connection@jc,"V","setAutoCommit",FALSE)
    sqlstr <- paste0("INSERT INTO ",
                tableName," VALUES(",paste0(rep("?",vcols),collapse=","),")")
    ps = .jcall(connection@jc,"Ljava/sql/PreparedStatement;","prepareStatement",sqlstr)
    myinsert <- function(namedvector,x){
                  vsetvector <- c()
                  vsetvector[" VARCHAR(255) "] <- "setString"
                  vsetvector[" FLOAT "] <- "setFloat"
                  vsetvector[" INT "] <- "setInt"
                  for(i in 1:length(namedvector))
                  {
                    #browser()
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
        warning("using max batchSize=10000")
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
    vcolnames <- names(vcolnames)
  }
  # browser()
  select <- new("FLSelectFrom",
                # connection = getFLConnection(), 
                table_name = tableName, 
                variables = list(
                    obs_id_colname = obsIdColname),
                ## var_id_colname = var_id_colnames,
                ## cell_val_colname = cell_val_colname),
                whereconditions=character(0),
                order = "")

  return(newFLTable(
              select = select,
              Dimnames = list(object[,obsIdColname],
                              vcolnames),
              dims=dim(object),
              isDeep = FALSE,
              type=sapply(object,typeof)))
}

as.FLByteInt <- function(x){
    vtbl <- getOption("ResultByteIntVectorTableFL")
    VID <- getMaxVectorId(getFLConnection(),
                          vtbl)
    vsqlstr <- constructSelect(x)
    vsqlstr <- gsub("'%insertIDhere%'",VID,vsqlstr)
    vtemp <- insertIntotbl(vtbl,
                            pSelect=vsqlstr)
    if(!vtemp)
        stop("invalid input: x and y should be of BYTEINT in-database type \n ")
    select <- new("FLSelectFrom",
                connection = getFLConnection(), 
                table_name = c(flt=vtbl),
                variables = list(
                        obs_id_colname = "flt.vectorIndexColumn"),
                whereconditions=paste0("flt.vectorIdColumn = ",VID),
                order = "")

    return(newFLVector(
                select=select,
                Dimnames=list(x@Dimnames[[1]],
                            "vectorValueColumn"),
                isDeep=FALSE,
                type="integer"))
}

setGeneric("populateDimnames",
    function(x,...){
        standardGeneric("populateDimnames")
        })

setMethod("populateDimnames",
    signature(x="ANY"),
    function(x,...){
        if(is.null(rownames(x)))
            x@Dimnames[[1]] <- 1:x@dims[1]
        if(x@isDeep)
            x@Dimnames[[2]] <- 1:x@dims[2]
        return(x)
})
