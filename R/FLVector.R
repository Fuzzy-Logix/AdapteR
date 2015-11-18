#' @include utilities.R
#' @include FLTable.R
NULL
#' An S4 class to represent FLVector
#'
#' @slot table object of class FLTable
#' @slot col_name A character column name
#' @slot vector_id_value numeric
#' @slot size numeric
setClass(
	"FLVector",
	slots = list(
		table = "FLTable",
		col_name = "character",
		vector_id_value = "numeric",
		size = "numeric"
	)
)

#' Constructor function for FLVector, representing a vector in database, either a deep or a wide matrix.
#' gk: how can we support row vectors?
#'
#' \code{FLVector} constructs an object of class \code{FLVector}.
#'
#' \code{FLVector} object is an in-database equivalent to a vector.
#' This object is used as input for matrix and arithmetic operation functions.
#' @param table FLTable object as returned by \code{\link[AdapteR]{FLTable}}
#'   where the vector is stored
#' @param colname name of the column in \code{table} where vector elements are stored
#' @param vector_id_value unique identifier for the vector if stored in deep table
#' @return \code{FLVector} returns an object of class FLVector mapped to an in-database vector.
#' @seealso \code{\link{FLTable}}
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' WideTable <- FLTable(connection, "FL_TRAIN", "tblVectorWide","vector_key")
#' flvectorWide <- FLVector(WideTable,"vector_value")
#' DeepTable <- FLTable(connection, "FL_TRAIN", "tblVectorDeep","vector_id","vector_key","vector_value")
#' flvectorDeep <- FLVector(DeepTable,"vector_value",1)
#' @export

FLVector <- function(table,
                     col_name,
                     vector_id_value=numeric(0), 
                     size=numeric(0))
{
	if(table@isDeep && length(vector_id_value))
	{
		size <- sqlQuery(table@odbc_connection,
						 paste0("SELECT max(",table@var_id_name,")
						 		 FROM ",
                                getRemoteTableName(table@db_name, table@table_name),
                                " WHERE ",table@primary_key,"=",vector_id_value))[1,1]
        new("FLVector",
        	table = table, 
        	col_name = table@num_val_name, 
        	vector_id_value = vector_id_value, 
        	size = size)
    }
    else if(!table@isDeep)
    {
        size <- sqlQuery(table@odbc_connection,
                         paste0("SELECT max(",table@primary_key,")
        				 		 FROM ",
                                getRemoteTableName(table@db_name,
                                                   table@table_name)))[1,1]
        new("FLVector", 
        	table = table, 
        	col_name = col_name, 
        	vector_id_value = vector_id_value, 
        	size = size)
    }
    else
    {
    	stop("column not in wide table or invalid inputs for deep table")	
    }
}

print.FLVector <- function(object)
{
    if(object@table@isDeep && length(object@vector_id_value))
    {
        valuedf <- sqlQuery(object@table@odbc_connection,
                            paste0("SELECT * FROM ",
                                   getRemoteTableName(object@table@db_name,
                                                      object@table@table_name),
                                   " WHERE ",object@table@primary_key,"=",
                                   object@vector_id_value,
                                   " ORDER BY ",object@table@var_id_name))
        print(as.vector(valuedf[,object@col_name]))
    }
    else if(!object@table@isDeep)
    {
        valuedf <- sqlQuery(object@table@odbc_connection,
                            paste0("SELECT ",
                                   object@table@primary_key,",",
                                   object@col_name,
                                   " FROM ",
                                   getRemoteTableName(object@table@db_name,
                                                      object@table@table_name),
                                   " ORDER BY ",object@table@primary_key))
        print(as.vector(valuedf[,object@col_name]))
    }
}

setMethod("show","FLVector",print.FLVector)
