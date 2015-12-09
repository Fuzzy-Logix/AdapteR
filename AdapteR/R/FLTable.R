#' @include utilities.R
NULL
setOldClass("RODBC")
#' An S4 class to represent FLTable
#'
#' @slot odbc_connection ODBC connectivity for R
#' @slot db_name character
#' @slot table_name character
#' @slot obs_id_colname character
#' @slot var_id_colnames character 
#' @slot cell_val_colname character
#' @slot isDeep logical
#' @method names FLTable
#' @param object retrieves the column names of FLTable object
setClass(
	"FLTable",
	slots = list(
		odbc_connection = "ANY",
		db_name         = "character",
		table_name      = "character",
		dimnames        = "list",
        whereconditions = "character",
		obs_id_colname  = "character", ## former primary_key
		isDeep = "logical",
        var_id_colname  = "character", ## 
		cell_val_colname = "character"
	)
)

#' Constructor function for FLTable.
#'
#' \code{FLTable} constructs an object of class \code{FLTable}.
#'
#' \code{FLTable} refers to an in-database table. This is equivalent to data.frame object. 
#' This object is commonly used as input for data mining functions.
#' @param connection ODBC connection handle as returned by \code{\link[RODBC]{odbcConnect}}
#' @param database name of the database
#' @param table name of the table
#' @param obs_id_colname column name set as primary key
#' @param var_id_colname column name where variable id's are stored if \code{FLTable} is deep
#' @param cell_val_colname column name where cell values are stored if \code{FLTable} is deep
#' @return \code{FLTable} returns an object of class FLTable mapped to a table
#' in Teradata.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' widetable  <- FLTable(connection, "FL_TRAIN", "tblAbaloneWide", "ObsID")
#' names(widetable)
#' @export
FLTable <- function(connection,
				    database, 
				    table,
				    obs_id_colname,
                    var_id_colnames=character(0), 
				    cell_val_colname=character(0),
                    whereconditions=character(0))
{

    ##browser()
    ## validate_args( 	list(database = database, table = table),
    ## 				list(database = "character", table = "character")
    ## )
	## if(xor(length(var_id_colnames) , length(cell_val_colname)))
	## {
	## 	stop("Unable to identify whether table is deep or wide")
	## }
    if(length(var_id_colnames) && length(cell_val_colname))
	{
        cols <- sort(sqlQuery(connection,
                         paste0("SELECT DISTINCT(",
                                var_id_colnames,") as VarID
						  FROM ",remoteTable(database,table),
                          " ",constructWhere(whereconditions)))$VarID)
        rows <- sort(sqlQuery(connection,
                         paste0("SELECT DISTINCT(",
                                obs_id_colname,") as VarID
						  FROM ",remoteTable(database,table),
                          " ",constructWhere(whereconditions)))$VarID)
        cols <- gsub("^ +| +$","",cols)
        rows <- gsub("^ +| +$","",rows)
		new("FLTable",
            odbc_connection = connection,
            db_name = database, 
            table_name = table,
            dimnames = list(rows,cols),
            obs_id_colname = obs_id_colname,
            var_id_colname = var_id_colnames,
            whereconditions=whereconditions,
            cell_val_colname = cell_val_colname,
            isDeep = TRUE)
	}
	else
	{
		cols <- sqlQuery(connection,
                         paste0("SELECT columnname 
						  FROM dbc.columns
						  WHERE tablename='",table,"' 
						  AND databasename='",database,"';"))$ColumnName
        rows <- sort(sqlQuery(connection,
                         paste0("SELECT DISTINCT(",
                                obs_id_colname,") as VarID
						  FROM ",remoteTable(database,table)))$VarID)
        cols <- gsub("^ +| +$","",cols)
        rows <- gsub("^ +| +$","",rows)
        if(length(var_id_colnames)==0)
            var_id_colnames <- cols
        if(length(setdiff(var_id_colnames,cols)))
            stop(paste0("columns do not exist: "))
		T <- new("FLTable", 
                 odbc_connection = connection,
                 db_name = database, 
                 table_name = table,
                 dimnames = list(rows,var_id_colnames),
                 whereconditions = whereconditions,
                 obs_id_colname = obs_id_colname,
                 #var_id_colname = "",
                 cell_val_colname = cell_val_colname,
                 isDeep = FALSE)
	}
}

#' @describeIn names 
#' Gives the column names of FLTable object
names.FLTable <- function(object) object@dimnames[[2]]
colnames.FLTable <- function(object) object@dimnames[[2]]
rownames.FLTable <- function(object) object@dimnames[[1]]


setMethod("show","FLTable",function(object) print(as.data.frame(object)))
