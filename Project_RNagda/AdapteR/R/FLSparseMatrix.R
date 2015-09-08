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
	"FLSparseMatrix",
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
#' Constructor function for FLSparseMatrix.
#'
#' \code{FLMatrix} constructs an object of class \code{FLSparseMatrix}.
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
#' @return \code{FLSparseMatrix} returns an object of class FLSparseMatrix mapped
#' to a matrix.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' table <- FLSparseMatrix(connection, "FL_TRAIN", "tblMatrixMultiSparse", 2)
#' @export
FLSparseMatrix <- function(connection, database, matrix_table, matrix_id_value,matrix_id_colname = "MATRIX_ID", row_id_colname = "ROW_ID", col_id_colname = "COL_ID", cell_val_colname = "CELL_VAL")
{
	sqlQuery(connection, paste("DATABASE", database))
	sqlQuery(connection, "SET ROLE ALL")

	new("FLSparseMatrix", odbc_connection = connection, db_name = database, matrix_table = matrix_table, matrix_id_value = matrix_id_value, matrix_id_colname = matrix_id_colname, row_id_colname = row_id_colname, col_id_colname = col_id_colname, cell_val_colname = cell_val_colname)
}

`+.FLSparseMatrix` <- function(flmatobj1, flmatobj2)
{
	sqlQuery(flmatobj1@odbc_connection, paste("DATABASE", flmatobj1@db_name))
	sqlQuery(flmatobj1@odbc_connection, "SET ROLE ALL")
	nrow1 <- sqlQuery(flmatobj1@odbc_connection, paste0("SELECT max(",flmatobj1@row_id_colname,") FROM ",flmatobj1@matrix_table," WHERE ",
		              flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value))[1,1]
	ncol1 <- sqlQuery(flmatobj1@odbc_connection, paste0("SELECT max(",flmatobj1@col_id_colname,") FROM ",flmatobj1@matrix_table," WHERE ",
		              flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value))[1,1]
	
	if(is.FLMatrix(flmatobj2) || is.FLSparseMatrix(flmatobj2))
	{
	nrow2 <- sqlQuery(flmatobj2@odbc_connection, paste0("SELECT max(",flmatobj2@row_id_colname,") FROM ",flmatobj2@db_name,".",flmatobj2@matrix_table,
		              " WHERE ",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value))[1,1]
	ncol2 <- sqlQuery(flmatobj2@odbc_connection, paste0("SELECT max(",flmatobj2@col_id_colname,") FROM ",flmatobj2@db_name,".",flmatobj2@matrix_table,
		              " WHERE ",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value))[1,1]

	if(nrow1 == nrow2 && ncol1 == ncol2)
	{
	if(is.FLSparseMatrix(flmatobj2))
	{
	matrix_id_value <- sqlQuery(flmatobj1@odbc_connection,paste(" SELECT max(",flmatobj1@matrix_id_colname,") FROM ",flmatobj1@matrix_table))[1,1] + 1
	
	sqlQuery(flmatobj1@odbc_connection, paste0(" INSERT INTO ",flmatobj1@db_name,".",flmatobj1@matrix_table," SELECT ",matrix_id_value," AS ",
		     flmatobj1@matrix_id_colname,",a.",flmatobj1@row_id_colname," AS ",flmatobj1@row_id_colname,",a.",flmatobj1@col_id_colname," AS ",
		     flmatobj1@col_id_colname,",a.",flmatobj1@cell_val_colname,"+b.",flmatobj2@cell_val_colname," AS ",flmatobj1@cell_val_colname," FROM ",
		     flmatobj1@db_name,".",flmatobj1@matrix_table," a,",flmatobj2@db_name,".",flmatobj2@matrix_table," b WHERE a.",flmatobj1@matrix_id_colname,
		     "=",flmatobj1@matrix_id_value," and b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," and a.",flmatobj1@row_id_colname,"=b.",
		     flmatobj2@row_id_colname," and a.",flmatobj1@col_id_colname,"=b.",flmatobj2@col_id_colname))
	
	sqlQuery(flmatobj1@odbc_connection, paste0(" INSERT INTO ",flmatobj1@db_name,".",flmatobj1@matrix_table," SELECT ",matrix_id_value," AS ",
			flmatobj1@matrix_id_colname,",a.",flmatobj1@row_id_colname," AS ",flmatobj1@row_id_colname,",a.",flmatobj1@col_id_colname," AS ",
			flmatobj1@col_id_colname,",a.",flmatobj1@cell_val_colname," AS ",flmatobj1@cell_val_colname," FROM ",flmatobj1@db_name,".",
			flmatobj1@matrix_table," a,",flmatobj2@db_name,".",flmatobj2@matrix_table," b WHERE a.",flmatobj1@matrix_id_colname,"=",
			flmatobj1@matrix_id_value," and b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," and a.",flmatobj1@row_id_colname,"!=b.",
			flmatobj2@row_id_colname," and a.",flmatobj1@col_id_colname,"!=b.",flmatobj2@col_id_colname))
	
	sqlQuery(flmatobj1@odbc_connection, paste0(" INSERT INTO ",flmatobj1@db_name,".",flmatobj1@matrix_table," SELECT ",matrix_id_value," AS ",
		flmatobj1@matrix_id_colname,",b.",flmatobj2@row_id_colname," AS ",flmatobj1@row_id_colname,",b.",flmatobj2@col_id_colname," AS ",
		flmatobj1@col_id_colname,",b.",flmatobj2@cell_val_colname," AS ",flmatobj1@cell_val_colname," FROM ",flmatobj1@db_name,".",flmatobj1@matrix_table,
		" a,",flmatobj2@db_name,".",flmatobj2@matrix_table," b WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," and b.",
		flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," and a.",flmatobj1@row_id_colname,"!=b.",flmatobj2@row_id_colname," and a.",
		flmatobj1@col_id_colname,"!=b.",flmatobj2@col_id_colname))

	new("FLSparseMatrix", odbc_connection = flmatobj1@odbc_connection, db_name = flmatobj1@db_name, matrix_table = flmatobj1@matrix_table, matrix_id_value = matrix_id_value, matrix_id_colname = flmatobj1@matrix_id_colname, row_id_colname = flmatobj1@row_id_colname, col_id_colname = flmatobj1@col_id_colname, cell_val_colname = flmatobj1@cell_val_colname)
	}
	else
	{
	matrix_id_value <- sqlQuery(flmatobj2@odbc_connection,paste(" SELECT max(",flmatobj2@matrix_id_colname,") FROM ",flmatobj2@matrix_table))[1,1] + 1
	sqlQuery(flmatobj2@odbc_connection, paste("DATABASE", flmatobj2@db_name))
	sqlQuery(flmatobj2@odbc_connection, "SET ROLE ALL")
	
	sqlQuery(flmatobj2@odbc_connection, paste0(" INSERT INTO ",flmatobj2@db_name,".",flmatobj2@matrix_table," SELECT ",matrix_id_value," AS ",
			flmatobj2@matrix_id_colname,",a.",flmatobj1@row_id_colname," AS ",flmatobj2@row_id_colname,",a.",flmatobj1@col_id_colname," AS ",
			flmatobj2@col_id_colname,",a.",flmatobj1@cell_val_colname,"+b.",flmatobj2@cell_val_colname," AS ",flmatobj2@cell_val_colname," FROM ",
			flmatobj1@db_name,".",flmatobj1@matrix_table," a,",flmatobj2@db_name,".",flmatobj2@matrix_table," b WHERE a.",flmatobj1@matrix_id_colname,
			"=",flmatobj1@matrix_id_value," and b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," and a.",flmatobj1@row_id_colname,"=b.",
			flmatobj2@row_id_colname," and a.",flmatobj1@col_id_colname,"=b.",flmatobj2@col_id_colname))
	
	sqlQuery(flmatobj2@odbc_connection, paste0(" INSERT INTO ",flmatobj2@db_name,".",flmatobj2@matrix_table," SELECT ",matrix_id_value," AS ",
			flmatobj2@matrix_id_colname,",b.",flmatobj2@row_id_colname," AS ",flmatobj2@row_id_colname,",b.",flmatobj2@col_id_colname," AS ",
			flmatobj2@col_id_colname,",b.",flmatobj2@cell_val_colname," AS ",flmatobj2@cell_val_colname," FROM ",flmatobj1@db_name,".",
			flmatobj1@matrix_table," a,",flmatobj2@db_name,".",flmatobj2@matrix_table," b WHERE a.",flmatobj1@matrix_id_colname,"=",
			flmatobj1@matrix_id_value," and b.",flmatobj2@matrix_id_colname,"=",flmatobj2@matrix_id_value," and a.",flmatobj1@row_id_colname,"!=b.",
			flmatobj2@row_id_colname," and a.",flmatobj1@col_id_colname,"!=b.",flmatobj2@col_id_colname))
	
	new("FLMatrix", odbc_connection = flmatobj2@odbc_connection, db_name = flmatobj2@db_name, matrix_table = flmatobj2@matrix_table, matrix_id_value = matrix_id_value, matrix_id_colname = flmatobj2@matrix_id_colname, row_id_colname = flmatobj2@row_id_colname, col_id_colname = flmatobj2@col_id_colname, cell_val_colname = flmatobj2@cell_val_colname)
	}
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
		flmatobj2 <- as.FLSparseMatrix(flmatobj2)
		flmatobj1+flmatobj2
	}
	else cat("ERROR::Operation Currently Not Supported")
}

print.FLSparseMatrix <- function(object)
{
	sqlQuery(object@odbc_connection, paste0("DATABASE", object@db_name,"; SET ROLE ALL;"))
	nrow <- sqlQuery(object@odbc_connection, paste0("SELECT max(",object@row_id_colname,") FROM ",object@matrix_table," WHERE ",
					object@matrix_id_colname,"=",object@matrix_id_value))[1,1]
	valuedf <- sqlQuery(object@odbc_connection, paste0("SELECT * FROM ",object@matrix_table," WHERE ",object@matrix_id_colname,"=",
						object@matrix_id_value," ORDER BY 1,2,3"))
	sparseMatrix(i=valuedf[,object@row_id_colname],j=valuedf[,object@col_id_colname],x=valuedf[,object@cell_val_colname])
}

setMethod("show","FLSparseMatrix",print.FLSparseMatrix)

as.FLSparseMatrix <- function(m,connection)
{	
	if(class(m)=="dgCMatrix")
	{
		ROW_ID <- m@i + 1
		COL_ID <- pToj(m@p)
		CELL_VAL <- m@x
		if(max(ROW_ID) < m@Dim[1]) 
		{ 
			append(ROW_ID,m@Dim[1])
			append(CELL_VAL,0)
			if(max(COL_ID) < m@Dim[2]) { append(COL_ID,m@Dim[2]) }
			else { append(COL_ID,max(COL_ID)) }
		}
		else if(max(COL_ID < m@Dim[2]))
		{
			append(ROW_ID,max(ROW_ID))
			append(COL_ID,m@Dim[2])
			append(CELL_VAL,0)
		}
		sqlQuery(connection,"DATABASE FL_TRAIN")
		sqlQuery(connection,"SET ROLE ALL")
		MATRIX_ID <- sqlQuery(connection,"SELECT max(MATRIX_ID) FROM tblMatrixMultiSparse")[1,1] + 1
		df <- data.frame(MATRIX_ID,ROW_ID,COL_ID,CELL_VAL)
		sqlSave(connection,df,"tblMatrixPhani")
		sqlQuery(connection,paste("INSERT INTO tblMatrixMultiSparse SELECT MATRIX_ID,ROW_ID,COL_ID,CELL_VAL FROM tblMatrixPhani"))
		sqlQuery(connection,paste("DROP TABLE tblMatrixPhani"))
		new("FLSparseMatrix", odbc_connection = connection, db_name = "FL_TRAIN", matrix_table = "tblMatrixMultiSparse", matrix_id_value = MATRIX_ID, matrix_id_colname = "MATRIX_ID", row_id_colname = "ROW_ID", col_id_colname = "COL_ID", cell_val_colname = "CELL_VAL")
	}
	else print("ERROR: input a sparse matrix of class dgCMatrix")
}

pToj <- function(p) {
  dp <- diff(p)
  rep(seq_along(dp), dp)
}

is.FLSparseMatrix <- function(object)
{
	if ( class(object) == "FLSparseMatrix")
			return (TRUE)
	else 	return (FALSE)
}

"+" <- function(x, value)
{
    UseMethod("+", x)
}
  
`+.numeric` <- function(vec,flmatobj1)
{	if(is.FLSparseMatrix(flmatobj1))
	{
		sqlQuery(flmatobj1@odbc_connection, paste("DATABASE", flmatobj1@db_name))
		sqlQuery(flmatobj1@odbc_connection, "SET ROLE ALL")
		nrow1 <- sqlQuery(flmatobj1@odbc_connection, paste0("SELECT max(",flmatobj1@row_id_colname,") FROM ",flmatobj1@matrix_table," WHERE ",
						flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value))[1,1]
		ncol1 <- sqlQuery(flmatobj1@odbc_connection, paste0("SELECT max(",flmatobj1@col_id_colname,") FROM ",flmatobj1@matrix_table," WHERE ",
						flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value))[1,1]
		flmatobj2 <- as.FLMatrix(matrix(vec,nrow1,ncol1,byrow=TRUE),flmatobj1@odbc_connection)
		flmatobj1+flmatobj2
	}
	else 
	{
		op <- .Primitive("+")
		op(vec,flmatobj1)
	}
}

`+.default` <- function(vec,flmatobj1)
{
	op <- .Primitive("+")
	op(vec,flmatobj1)
}

as.matrix <- function(x)
{
	UseMethod("as.matrix",x)
}

as.matrix.data.frame <- base::as.matrix.data.frame
as.matrix.integer <- base::as.matrix.default
as.matrix.numeric <- base::as.matrix.default

as.matrix.FLSparseMatrix <- function(object)
{
	sqlQuery(object@odbc_connection, paste0("DATABASE", object@db_name,"; SET ROLE ALL;"))
	nrow <- sqlQuery(object@odbc_connection, paste0("SELECT max(",object@row_id_colname,") FROM ",object@matrix_table," WHERE ",
			object@matrix_id_colname,"=",object@matrix_id_value))[1,1]
	valuedf <- sqlQuery(object@odbc_connection, paste0("SELECT * FROM ",object@matrix_table," WHERE ",object@matrix_id_colname,"=",
			object@matrix_id_value," ORDER BY 1,2,3"))
	as.matrix(sparseMatrix(i=valuedf[,object@row_id_colname],j=valuedf[,object@col_id_colname],x=valuedf[,object@cell_val_colname]))
}

`+.matrix` <- function(x,flmatobj1)
{
	if(is.FLSparseMatrix(flmatobj1))
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

`+.dgCMatrix` <- function(x,flmatobj)
{
	if(is.FLSparseMatrix(flmatobj) || is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLSparseMatrix(x,flmatobj@odbc_connection)
		flmatobj2 + flmatobj
	}
	else
	{
		op <- .Primitive("+")
		op(x,flmatobj)
	}

}