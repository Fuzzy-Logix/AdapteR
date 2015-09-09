#' @include utilities.R
NULL
setOldClass("RODBC")
#' An S4 class to represent FLMatrix
#'
#' @slot odbc_connection ODBC connectivity for R
#' @slot db_name character
#' @slot matrix_table character
#' @slot matrix_id_value numeric id value
#' @slot matrix_id_colname character
#' @slot row_id_colname character
#' @slot col_id_colname character
#' @slot cell_val_colname character
#'
setClass(
	"FLMatrix",
	slots = list(
		odbc_connection = "RODBC",
		db_name = "character",
		matrix_table = "character",
		matrix_id_value	= "numeric",
		matrix_id_colname = "character",
		row_id_colname = "character",
		col_id_colname = "character",
		cell_val_colname = "character"
	)
)
#' Constructor function for FLMatrix.
#'
#' \code{FLMatrix} constructs an object of class \code{FLMatrix}.
#'
#' This object is used as input for matrix operation functions.
#' @param connection ODBC connection handle as returned by \code{\link[RODBC]{odbcConnect}}
#' @param database name of the database
#' @param matrix_table name of the matrix table
#' @param matrix_id_value identifier for the input matrix
#' @param matrix_id_colname matrix id value in \code{matrix_table}
#' @param row_id_colname row number in \code{matrix_table}
#' @param col_id_colname column number in \code{matrix_table}
#' @param cell_val_colname value of the matrix element in \code{matrix_table}
#' @return \code{FLMatrix} returns an object of class FLMatrix mapped
#' to a matrix.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' table <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' @export
FLMatrix <- function(connection, database, matrix_table, matrix_id_value,matrix_id_colname = "MATRIX_ID", row_id_colname = "ROW_ID", 
					col_id_colname = "COL_ID", cell_val_colname = "CELL_VAL")
{
	sqlQuery(connection, paste("DATABASE", database))
	sqlQuery(connection, "SET ROLE ALL")

	new("FLMatrix", odbc_connection = connection, db_name = database, matrix_table = matrix_table, matrix_id_value = matrix_id_value, 
		matrix_id_colname = matrix_id_colname, row_id_colname = row_id_colname, col_id_colname = col_id_colname, cell_val_colname = cell_val_colname)
}

"+" <- function(x, value)
{
    UseMethod("+", x)
}

`+.default` <- function(vec,flmatobj1)
{
	op <- .Primitive("+")
	op(vec,flmatobj1)
}

`+.matrix` <- function(x,flmatobj1)
{
	if(is.FLMatrix(flmatobj1))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj1@odbc_connection)
		flmatobj2+flmatobj1
	}
	else if(is.FLSparseMatrix(flmatobj1))
	{
		flmatobj2 <- as.FLMatrix(x,flmatobj1@odbc_connection)
		flmatobj1+flmatobj2
	}
	else 
	{
		op <- .Primitive("+")
		op(x,flmatobj1)
	}
}

`+.FLMatrix` <- function(flmatobj1, flmatobj2)
{
	sqlQuery(flmatobj1@odbc_connection, paste("DATABASE", flmatobj1@db_name))
	sqlQuery(flmatobj1@odbc_connection, "SET ROLE ALL")
	
	nrow1 <- sqlQuery(flmatobj1@odbc_connection, paste0("SELECT max(",flmatobj1@row_id_colname,") FROM ",
					flmatobj1@matrix_table," WHERE ",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value))[1,1]
	
	ncol1 <- sqlQuery(flmatobj1@odbc_connection, paste0("SELECT max(",flmatobj1@col_id_colname,") FROM ",
					flmatobj1@matrix_table," WHERE ",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value))[1,1]
	
	if(is.FLMatrix(flmatobj2))
	{
	nrow2 <- sqlQuery(flmatobj2@odbc_connection, paste0("SELECT max(",flmatobj2@row_id_colname,") FROM ",
					flmatobj2@db_name,".",flmatobj2@matrix_table," WHERE ",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value))[1,1]
	
	ncol2 <- sqlQuery(flmatobj2@odbc_connection, paste0("SELECT max(",flmatobj2@col_id_colname,") FROM ",
					flmatobj2@db_name,".",flmatobj2@matrix_table," WHERE ",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value))[1,1]

	if(nrow1 == nrow2 && ncol1 == ncol2)
	{
	matrix_id_value <- sqlQuery(flmatobj1@odbc_connection,paste0(" SELECT max(",flmatobj1@matrix_id_colname,") FROM ",flmatobj1@matrix_table))[1,1] + 1
	
	sqlQuery(flmatobj1@odbc_connection, paste0(" INSERT INTO ",flmatobj1@db_name,".",flmatobj1@matrix_table," SELECT ",matrix_id_value," AS ",
		flmatobj1@matrix_id_colname,",a.",flmatobj1@row_id_colname," AS ",flmatobj1@row_id_colname,",a.",flmatobj1@col_id_colname," AS ",
		flmatobj1@col_id_colname,",a.",flmatobj1@cell_val_colname,"+b.",flmatobj2@cell_val_colname," AS ",flmatobj1@cell_val_colname," FROM ",
		flmatobj1@db_name,".",flmatobj1@matrix_table," a,",flmatobj2@db_name,".",flmatobj2@matrix_table," b WHERE a.",flmatobj1@matrix_id_colname,"=",
		flmatobj1@matrix_id_value," and b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," and a.",flmatobj1@row_id_colname,"=b.",
		flmatobj2@row_id_colname," and a.",flmatobj1@col_id_colname,"=b.",flmatobj2@col_id_colname))
	
	new("FLMatrix", odbc_connection = flmatobj1@odbc_connection, db_name = flmatobj1@db_name, matrix_table = flmatobj1@matrix_table, 
		matrix_id_value = matrix_id_value, matrix_id_colname = flmatobj1@matrix_id_colname, row_id_colname = flmatobj1@row_id_colname, 
		col_id_colname = flmatobj1@col_id_colname, cell_val_colname = flmatobj1@cell_val_colname)
	}
	else stop("ERROR: Invalid matrix dimensions for addition")
	}
	else if(is.vector(flmatobj2))
	{
		flmatobj2 <- as.FLMatrix(matrix(flmatobj2,nrow1,ncol1),flmatobj1@odbc_connection)
		flmatobj1+flmatobj2
	}
	else if(is.matrix(flmatobj2))
	{
		flmatobj2 <- as.FLMatrix(flmatobj2,flmatobj1@odbc_connection)
		flmatobj1+flmatobj2
	}
	else if(class(flmatobj2)=="dgCMatrix")
	{
		flmatobj2 <- as.FLSparseMatrix(flmatobj2,flmatobj1@odbc_connection)
		flmatobj2+flmatobj1
	}
	else if(is.FLSparseMatrix(flmatobj2))
	{
		flmatobj2+flmatobj1
	}
	else cat("ERROR::Operation Currently Not Supported")
}

print.FLMatrix <- function(object)
{
	sqlQuery(object@odbc_connection, paste0("DATABASE", object@db_name,"; SET ROLE ALL;"))
	nrows <- sqlQuery(object@odbc_connection, paste0("SELECT max(",object@row_id_colname,") FROM ",object@matrix_table," WHERE ",
		object@matrix_id_colname,"=",object@matrix_id_value))[1,1]
	valuedf <- sqlQuery(object@odbc_connection, paste0("SELECT * FROM ",object@matrix_table," WHERE ",object@matrix_id_colname,"=",
		object@matrix_id_value," ORDER BY 1,2,3"))
	print(matrix(valuedf[,object@cell_val_colname],nrows,length(valuedf[,object@cell_val_colname])/nrows,byrow=T))
}

setMethod("show","FLMatrix",print.FLMatrix)

as.FLMatrix <- function(m,connection)
{	
	if(is.matrix(m) || class(m)=="dgeMatrix")
	{
		if(is.matrix(m) && !is.numeric(m)) { stop("ERROR: ONLY NUMERIC ENTRIES ALLOWED IN FLMATRIX") }
		else{
		 sqlQuery(connection,"DATABASE FL_TRAIN")
		 sqlQuery(connection,"SET ROLE ALL")
		 ROW_ID <- c()
		 COL_ID <- c()
		 for (i in 1:ncol(m))
		 for (j in 1:nrow(m))
		 {
		 	ROW_ID <- append(ROW_ID,j)
		 	COL_ID <- append(COL_ID,i)
		 }
		 CELL_VAL <- as.vector(m)
		 MATRIX_ID <- sqlQuery(connection,"SELECT max(MATRIX_ID) FROM tblMatrixMulti")[1,1] + 1
		 df <- data.frame(MATRIX_ID,ROW_ID,COL_ID,CELL_VAL)
		 sqlSave(connection,df,"tblMatrixPhani")
		 sqlQuery(connection,paste("INSERT INTO tblMatrixMulti SELECT MATRIX_ID,ROW_ID,COL_ID,CELL_VAL FROM tblMatrixPhani"))
		 sqlQuery(connection,paste("DROP TABLE tblMatrixPhani"))
		 new("FLMatrix", odbc_connection = connection, db_name = "FL_TRAIN", matrix_table = "tblMatrixMulti", matrix_id_value = MATRIX_ID,
		  matrix_id_colname = "MATRIX_ID", row_id_colname = "ROW_ID", col_id_colname = "COL_ID", cell_val_colname = "CELL_VAL")
		}
	}
}

is.FLMatrix <- function(object)
{
	if ( class(object) == "FLMatrix")
			return (TRUE)
	else 	return (FALSE)
}
 
`+.numeric` <- function(x,obj1)
{	if(is.FLMatrix(obj1))
	{
		sqlQuery(obj1@odbc_connection, paste("DATABASE", obj1@db_name))
		sqlQuery(obj1@odbc_connection, "SET ROLE ALL")
		nrow1 <- sqlQuery(obj1@odbc_connection, paste0("SELECT max(",obj1@row_id_colname,") FROM ",obj1@matrix_table," WHERE ",obj1@matrix_id_colname,"=",obj1@matrix_id_value))[1,1]
		ncol1 <- sqlQuery(obj1@odbc_connection, paste0("SELECT max(",obj1@col_id_colname,") FROM ",obj1@matrix_table," WHERE ",obj1@matrix_id_colname,"=",obj1@matrix_id_value))[1,1]
		obj2 <- as.FLMatrix(matrix(x,nrow1,ncol1,byrow=TRUE),obj1@odbc_connection)
		obj2+obj1
	}
	else if(is.FLSparseMatrix(obj1))
	{
		sqlQuery(obj1@odbc_connection, paste("DATABASE", obj1@db_name))
		sqlQuery(obj1@odbc_connection, "SET ROLE ALL")
		nrow1 <- sqlQuery(obj1@odbc_connection, paste0("SELECT max(",obj1@row_id_colname,") FROM ",obj1@matrix_table," WHERE ",
						 obj1@matrix_id_colname,"=",obj1@matrix_id_value))[1,1]
		ncol1 <- sqlQuery(obj1@odbc_connection, paste0("SELECT max(",obj1@col_id_colname,") FROM ",obj1@matrix_table," WHERE ",
						 obj1@matrix_id_colname,"=",obj1@matrix_id_value))[1,1]
		obj2 <- as.FLMatrix(matrix(vec,nrow1,ncol1,byrow=TRUE),obj1@odbc_connection)
		obj1+obj2
	}
	else
	{
		op <- .Primitive("+")
		op(x,obj1)
	}
}

as.matrix <- function(x)
{
	UseMethod("as.matrix",x)
}

as.matrix.data.frame <- base::as.matrix.data.frame
as.matrix.integer <- base::as.matrix.default
as.matrix.numeric <- base::as.matrix.default

as.matrix.FLMatrix <- function(flmatobj1)
{
	sqlQuery(flmatobj1@odbc_connection, paste("DATABASE", flmatobj1@db_name))
	sqlQuery(flmatobj1@odbc_connection, "SET ROLE ALL")
	df <- sqlQuery(flmatobj1@odbc_connection, paste0("SELECT * FROM ",flmatobj1@matrix_table," WHERE ",flmatobj1@matrix_id_colname,"=",
					flmatobj1@matrix_id_value," ORDER BY 1,2,3"))
	ncol <- max(df[,flmatobj1@col_id_colname])
	nrow <- max(df[,flmatobj1@row_id_colname])
	vec <- df[,flmatobj1@cell_val_colname]
	matrix(vec,nrow,ncol,byrow=TRUE)
}

nrow<-function(x, ...){
	UseMethod("nrow",x)
}
nrow.integer<-base::nrow
nrow.matrix<-base::nrow
nrow.default<-base::nrow

nrow.FLMatrix<-function(object){
	connection<-object@odbc_connection
	sqlQuery(object@odbc_connection,paste0("DATABASE ",object@db_name,";"," SET ROLE ALL;"))
	sqlstr<-paste0("SELECT MAX(",object@row_id_colname,") FROM ",object@matrix_table," WHERE ",object@matrix_id_colname," = ",object@matrix_id_value)
	retobj<-sqlQuery(connection,sqlstr)[1,1]
	retobj
}

NROW<-function(x, ...){
	UseMethod("NROW",x)
}
NROW.integer<-base::NROW
NROW.default<-base::NROW

NROW.FLMatrix<-function(object){
	nrow(object)
}

ncol<-function(x, ...){
	UseMethod("ncol",x)
}
ncol.integer<-base::ncol
ncol.default<-base::ncol

ncol.FLMatrix<-function(object){
	connection<-object@odbc_connection
	sqlQuery(object@odbc_connection,paste0("DATABASE ",object@db_name,";"," SET ROLE ALL;"))
	sqlstr<-paste("SELECT MAX(",object@col_id_colname,") 
				   FROM ",object@matrix_table,
				   "WHERE ",object@matrix_id_colname," = ",object@matrix_id_value)
	
	retobj<-sqlQuery(connection,sqlstr)[1,1]
	retobj
}

NCOL<-function(x, ...){
	UseMethod("NCOL",x)
}
NCOL.integer<-base::NCOL
NCOL.default<-base::NCOL

NCOL.FLMatrix<-function(object){
	ncol(object)
}
