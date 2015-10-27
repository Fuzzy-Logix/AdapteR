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
	return(object@nrow)
}

nrow.FLSparseMatrix<-function(object)
{
	return(object@nrow)
}

nrow.FLTable<-function(object)
{
	connection<-object@odbc_connection
	sqlQuery(object@odbc_connection,paste0("DATABASE ",object@db_name,";"," SET ROLE ALL;"))
	t<-sqlQuery(object@odbc_connection, paste0(" SELECT max(",object@primary_key,") FROM ",object@table_name))[1,1]
	return(t)
}

nrow.FLVector<-function(object)
{
	return(object@size)
}

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

NROW.FLVector<-function(object){
	nrow(object)
}

# ncol and NCOL gives the number of rows of the objects
ncol<-function(x, ...){
	UseMethod("ncol",x)
}
ncol.integer<-base::ncol
ncol.default<-base::ncol

ncol.FLMatrix<-function(object){
	return(object@ncol)
}

ncol.FLSparseMatrix<-function(object){
	return(object@ncol)
}

ncol.FLTable<-function(object){
	connection<-object@odbc_connection
	sqlQuery(object@odbc_connection,paste0("DATABASE ",object@db_name,";"," SET ROLE ALL;"))
	if(object@isDeep)
	{
	t<-sqlQuery(object@odbc_connection, paste0(" SELECT max(",object@var_id_name,") FROM ",object@table_name))[1,1]
	return(t)
	}
}

ncol.FLVector<-function(object){
	return(1)
}

NCOL<-function(x, ...){
	UseMethod("NCOL",x)
}
NCOL.integer<-base::NCOL
NCOL.default<-base::NCOL

NCOL.FLMatrix<-function(object){
	ncol(object)
}

NCOL.FLSparseMatrix<-function(object){
	ncol(object)
}

NCOL.FLTable<-function(object){
	ncol(object)
}

NCOL.FLVector<-function(object){
	return(1)
}

# Returns the dimensions of the object
dim.FLMatrix <- function(object)
{
	 return(c(object@nrow,object@ncol))
}

dim.FLSparseMatrix <- function(object)
{
	 return(c(object@nrow,object@ncol))
}

dim.FLTable <- function(object)
{
	 return(c(nrow(object),ncol(object)))
}

dim.FLVector <- function(object)
{
	 return(c(object@size,1))
}

dim.default<-base::dim