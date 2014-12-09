#' Linear Discriminant Analysis
#'
#' \code{FLLDA} performs Fisher linear discriminant analysis (LDA).
#' @param table an object of class \code{FLTable}
#' @param primary_key name of primary key column of the table mapped to \code{table}
#' @param response name of the dependent variable column of the table mapped to
#' \code{table}
#' @param exclude vector of names of the columns which are to be excluded
#' @param class_spec list that identifies the value of the categorical variable
#' which is to be used as reference when converting to dummy binary variables
#' @param where_clause condition to filter out data from the table
#' @param note note
#' @return \code{FLLDA} returns an object of class \code{FLLDA}. The components
#' of this class mentioned below can be pulled in R using the generic
#' \code{FLFetch}.
#' \item{canonical_coeffs}{a \code{data.frame} which stores the the Canonical
#' Coefficients}
#' \item{fisher_coeffs}{a \code{data.frame} which stores the the Fisher
#' Coefficients}
#' \item{canonical_variates}{a \code{data.frame} which stores the Canonical
#' Variates}
#' \item{cross_tables}{a \code{data.frame} which stores the Cross Tables of
#' predicted and observed classfication}
#' @examples
#' \dontrun{
#' connection <- odbcConnect("Gandalf")
#' db_name    <- FL_R_WRAP
#' table_name <- "tblirisdata"
#' # Create FLTable object
#' table      <-  FLTable(connection, db_name, table_name)
#' # Perform LDA
#' result     <- FLLDA( table,
#'                      primary_key = "ObsID",
#'                      response = "SpeciesID",
#'                      exclude = c("Species"))
#' # Fetch reults in R
#' ldaResult <- FLFetch(result)
#' }
#' @export
FLLDA <- function( 	table,
					primary_key,
					response,
					exclude = c(),
					class_spec = list(),
					where_clause = "",
					note = "From RWrapper For DBLytix")
{

	obsID  <- "ObsID";
	varID  <- "VarID";
	value  <- "Num_Val";

	dataPrepRes 			<- regr_data_prep( 	table,
												response,
												obs_id = obsID,
												var_id = varID,
												value = value,
												primary_key = primary_key,
												exclude = exclude,
												class_spec = class_spec,
												where_clause = where_clause);

	deepTableName        	<- dataPrepRes$deepTableName;
	wideToDeepAnalysisID 	<- dataPrepRes$wideToDeepAnalysisID;
	connection         		<- table@odbc_connection;

	sql        				<- "CALL WorkaroundLDA('";
	sqlParameters 			<- paste(	deepTableName,
										obsID,
										varID,
										value,
										note, sep="','")
	sql          			<- paste(sql, sqlParameters,"')", sep="")
	print(sql)

	#run FLLDA
	ldaRes  				<- sqlQuery(connection, sql);
	analysisID 				<- toString(ldaRes[[1,"ANALYSISID"]]);
	retData = new("FLLDA",analysis_id = analysisID, odbc_connection = connection);

	return(retData);
}
