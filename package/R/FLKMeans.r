#' K-Means Clustering 
#' 
#' Performs K-Means clustering of input data.
#
#' @details Clustering algorithms can output slightly different centroid locations
#' and the associated cluster membership of each data point because the 
#' initialization centroid locations are random. In general, k-means has two 
#' steps: assigning data points to the nearest cluster and then moving each 
#' cluster’s centroid to the center of the members of the cluster. Occasionally
#' during this iterative process, a centroid can get so far away from the 
#' “central mass” of the data points (relative to other centroids) such that 
#' the cluster has no members. In this case, this centroid actually drops off 
#' of the dendrogram. This is a known characteristic of such clustering algorithms,
#' and in fact k-means is generally run multiple times to take into account this 
#' randomness, and which is why we built in the ability to run multiple hypothesis 
#' into the XSP to run in parallel and take advantage of in-database parallel 
#' processing capabilities.
#' 
#' @param table An object of class \code{FLTable}
#' @param primary_key name of primary key column of the table mapped to \code{table}
#' @param centers the number of clusters
#' @param max_iter the maximum number of iterations allowed
#' @param nstart Number of starting hypotheses
#' @param exclude vector of names of columns which are to be excluded
#' @param class_spec list that identifies the value of the categorical variable
#' which is to be used a reference when converting to dummy binary variables
#' @param where_clause condition to filter out data from the table
#' @param note note
#'
#' @return an object of class \code{FLKMeans} whose components can be 
#' pulled to R by running FLFetch. It has the following slots:
#' \item{centers}{ a \code{data.frame} with columns HypothesisID, Level, ClusterID, VarID, Centroid }
#' \item{cluster}{ a \code{data.frame} with columns HypothesisID, ObsID, ClusterID}
#'
#' @examples
#' \dontrun{
#'
#' #Simple example with only numerical variables
#' FLKMeans(KMeansTbl, PrimaryKey = "ID", centers = 2, max_iter = 20, nstart = 1)
#'
#' }
#'
#'@export
FLKMeans <- function( 	table,
						primary_key,
                  		centers,
						max_iter = 10, 
						nstart   = 1,
						exclude      = c(),
						class_spec    = list(),
						where_clause  = "",
						note     = "From RWrapper For DBLytix")
{

	obsID  <- "ObsID"
	varID  <- "VarID"
	value  <- "Num_Val"
	
	deepTableName 	<- wide_to_deep(	table,
										obs_id = obsID,
										var_id = varID,
										value =  value,
										primary_key  = primary_key,
										exclude      = exclude,
										class_spec   = class_spec,
										where_clause = where_clause)
	connection  	<- table@odbc_connection
	sql        		<- "CALL FLKMeans('"
	sqlParameters 	<- paste(	deepTableName,
								obsID,  
								varID, 
								value, 
								where_clause, 
								toString(centers),  
								toString(max_iter), 
								toString(nstart),
								note, sep="','")
	sql           	<- paste(sql, sqlParameters,"')", sep="")
	
	#run KMeans
	kMeansRes       <- sqlQuery(connection, sql)
	analysisID      <- toString(kMeansRes$ANALYSISID)

	retData = new("FLKMeans",analysis_id = analysisID, odbc_connection = connection, deep_table_name = deepTableName)
	
	return(retData)
}