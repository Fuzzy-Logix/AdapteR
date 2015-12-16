# Gives the dimensions of the objects

# nrow and NROW gives the number of rows of the objects
nrow<-function(x, ...)
{
	UseMethod("nrow",x)
}
nrow.integer<-base::nrow
nrow.matrix<-base::nrow
nrow.default<-base::nrow

nrow.FLMatrix<-function(object)
{
	if(is.numeric(object@dimnames[[1]]))
	return(max(object@dimnames[[1]]))
	else
	return(length(object@dimnames[[1]]))
}

# nrow.FLSparseMatrix<-function(object)
# {
# 	return(length(object@dimnames[[1]]))
# }

nrow.FLTable<-function(object)	return(length(object@dimnames[[1]]))

NROW<-function(x, ...){
	UseMethod("NROW",x)
}
NROW.integer<-base::NROW
NROW.default<-base::NROW

NROW.FLMatrix<-function(object){
	nrow(object)
}

NROW.FLSparseMatrix<-function(object){
	nrow(object)
}

NROW.FLTable<-function(object){
	nrow(object)
}

# ncol and NCOL gives the number of rows of the objects
ncol<-function(x, ...){
	UseMethod("ncol",x)
}
ncol.integer<-base::ncol
ncol.default<-base::ncol

ncol.FLMatrix<-function(object)
{
	if(is.numeric(object@dimnames[[2]]))
	return(max(object@dimnames[[2]]))
	else
	return(length(object@dimnames[[2]]))
}

# ncol.FLSparseMatrix<-function(object){
# 	return(length(object@dimnames[[2]]))
# }

ncol.FLTable<-function(object) 	return(length(object@dimnames[[2]]))


NCOL<-function(x, ...){
	UseMethod("NCOL",x)
}
NCOL.integer<-base::NCOL
NCOL.default<-base::NCOL

NCOL.FLMatrix<-function(object){
	ncol(object)
}

# NCOL.FLSparseMatrix<-function(object){
# 	ncol(object)
# }

NCOL.FLTable<-function(object){
	ncol(object)
}


# Returns the dimensions of the object
dim.FLMatrix <- function(object)
{
	 return(c(nrow(object),ncol(object)))
}

# dim.FLSparseMatrix <- function(object)
# {
# 	 return(c(nrow(object),ncol(object)))
# }

dim.FLTable <- function(object)
{
	 return(c(nrow(object),ncol(object)))
}

dim.default<-base::dim
