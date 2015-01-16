#' @include utilities.R
#' @include data_prep.R
#' @include FLFetch.R
NULL

#' Nearest neighbour matching
#'
#' \code{FLMatchIt} performs "nearest neighbor matching" without replacement.
#' The output of this function is a list of observation IDs which are not matched.

#' @param table an object of class \code{FLTable}
#' @param obs_id name of the Observation ID column name
#' @param treatment name of the treatment column
#' @param prop_score name of the propensity score column
#' @param match_order name of the match order column

#' @return \code{FLMatchtIt} returns an object of class \code{FLMatchIt}. An
#' object of this class is a list containing three components.
#' \item{odbc_connection}{ODBC connection handle as returned by \code{\link[RODBC]{odbcConnect}}.}
#' \item{out_table_name}{name of the permanent table which stores the output
#' in \code{Teradata}. This table does not exist in the R workspace.}
#' \item{unmatched_obs_id}{a \code{data.frame} in R which stores the list of \code{obs_ids}
#' which are not matched. This data frame is pulled in R using the generic
#' \code{FLFetch}.}

#' @examples
#' \dontrun{
#' connection <- odbcConnect("Gandalf")
#' db_name    <- FL_R_WRAP
#' table_name <- "TblMatchItAlt"
#' # Create FLTable object
#' table      <- FLTable(connection, db_name, table_name)
#' # Perform nearest neighbour matching
#' res        <- FLMatchIt(table, "person_id", "exposure", "prob", "prob")
#' }

#' @export
FLMatchIt <- function( 	table,
						obs_id,
						treatment,
						prop_score,
						match_order)

{
	argList  <- as.list(environment())
	typeList <- list(	table        = "FLTable",
						obs_id      = "character",
						treatment   = "character",
						prop_score  = "character",
						match_order = "character")
	validate_args(argList, typeList)
	
	connection      <- table@odbc_connection;
	sql        		<- "CALL FLMatchIt('";
	sqlParameters 	<- paste(	table@table_name,
								obs_id,
								treatment,
								prop_score,
								match_order,
								toString(1), sep="','")
	sql        		<- paste(sql, sqlParameters,"'",",OutTable",")", sep="")
	#print(sql)

	# run FLMatchIt
	res  			<- sqlQuery(connection, sql);
	volatileTable 	<- toString(res[[1,"OutTable"]]);

	# Create permanent OutTable
	outTable 		<- gen_out_table_name(volatileTable);
	sql 			<- paste("CREATE TABLE ", outTable, " AS (SELECT ", obs_id, " FROM ", volatileTable, ") WITH DATA", sep = "");
	#print(sql)
	sqlQuery(connection, sql);

	retData = new("FLMatchIt",	odbc_connection = connection, out_table_name = outTable);
	return(retData);
}
