# Gives the dimensions of the objects

# nrow and NROW gives the number of rows of the objects
#' @export
nrow<-function(x, ...)
{
	UseMethod("nrow",x)
}

#' @export
nrow.integer<-base::nrow
#' @export
nrow.matrix<-base::nrow
#' @export
nrow.default<-base::nrow

#' @export
nrow.FLMatrixBind<-function(object)
{
	if(object@by==1)
        return(sum(sapply(object@parts,nrow)))
	else
        return(nrow(object@parts[[1]]))
}

#' @export
nrow.FLTable<-function(object)	return(length(object@dimnames[[1]]))
#' @export
nrow.FLVector<-function(object)	return(length(object@dimnames[[1]]))

#' @export
NROW<-function(x, ...){
	UseMethod("NROW",x)
}
#' @export
NROW.integer<-base::NROW
#' @export
NROW.default<-base::NROW

#' @export
NROW.FLMatrix<-function(object){
	nrow(object)
}

#' @export
NROW.FLTable<-function(object){
	nrow(object)
}

# ncol and NCOL gives the number of rows of the objects
#' @export
ncol<-function(x, ...){
	UseMethod("ncol",x)
}
#' @export
ncol.integer<-base::ncol
#' @export
ncol.default<-base::ncol

#' @export
ncol.FLMatrixBind<-function(object)
{
	if(object@by==2)
        return(sum(sapply(object@parts,ncol)))
	else
        return(ncol(object@parts[[1]]))
}
#' @export
ncol.FLTable<-function(object) 	return(length(object@dimnames[[2]]))
#' @export
ncol.FLVector<-function(object) return(length(object@dimnames[[2]]))

#' @export
NCOL<-function(x, ...){
	UseMethod("NCOL",x)
}
#' @export
NCOL.integer<-base::NCOL
#' @export
NCOL.default<-base::NCOL

#' @export
NCOL.FLMatrix<-function(object){
	ncol(object)
}

#' @export
NCOL.FLTable<-function(object){
	ncol(object)
}

#' Returns the dimensions of the object
#'
#' @param object FLMatrix, FLVector or FLTable object
#' @return R vector giving dimensions of input object
#' @export
dim<-function(object){
	UseMethod("dim",object)
}

#' @export
dim.default <- base::dim
#' @export
dim.FLMatrix <- function(object)
{
	 return(object@dim)
}

# Returns the dimensions of the object
#' @export
dim.FLMatrixBind <- function(object)
{
	 return(c(nrow(object),ncol(object)))
}

#' @export
dim.FLTable <- function(object)
{
	 return(c(nrow(object),ncol(object)))
}

#' @export
dim.FLVector <- function(object)
{
	 return(c(nrow(object),ncol(object)))
}


