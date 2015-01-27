#' @include utilities.R
#' @include data_prep.R
#' @include FLFetch.R
NULL

#' Mixed Discriminant Analysis
#'
#' \code{FLMDA} performs mixed discriminant analysis.

#' @details For the training data, MDA divides each class into a number of
#' artificial subclasses. It calibrates the mixture of Gaussians and the
#' mixing probability by maximizing the log likelihood with expectation
#' maximization. The training data is stored in a wide table, and the dependent
#' variable denotes the class ID.

#' @param table an object of class \code{FLTable}
#' @param primary_key name of primary key column of the table mapped to \code{table}
#' @param response name of the dependent variable column of the table mapped to
#' \code{table}
#' @param subclasses number of subclasses into which each class is divided
#' @param max_iter maximum number of iterations for expectation maximization.
#' The expectation maximization (EM) converges when the change in log likelihood
#' over all the hypotheses is < 10^{-6}
#' @param initialization indicator of initialization method for each obs' latent
#' variable \code{Prob(x is in the subclass of class)}. Can be either 1 or 2 as
#' explained below.
#' \itemize{
#'  \item{1}{:    Assign weight of 1 to a random subclass of its class 0 otherwise.}
#'  \item{2}{:    For each class, run K-means with # clusters = # subclasses. Assign
#' weight of 1 to each obs' K-Means cluster 0 otherwise. (K-Means max iterations
#' = MDA max iterations.) Also, K-Means occasionally drops a cluster if no data
#' was assigned to it during its EM iterations, which would cause a divide-by-zero
#' error in calculating mu these hypotheses are excluded from MDA.}
#' }
#' @param hypotheses number of hypotheses to run simultaneously
#' @param exclude vector of names of the columns which are to be excluded
#' @param class_spec list that identifies the value of the categorical variable
#' which is to be used as reference when converting to dummy binary variables
#' @param where_clause condition to filter out data from the table
#' @param note note

#' @return \code{MDA} returns an object of class \code{FLMDA}. The components
#' of this class mentioned below can be pulled in R using the generic
#' \code{FLFetch}.
#' \item{cluster_probability}{a \code{data.frame} which stores the the cluster
#' probabilities}
#' \item{mu}{a \code{data.frame} which stores the mean vector in each subclass}
#' \item{sigma}{a \code{data.frame} which stores the common covariance matrix
#' of the multivariate normal distribution in each subclass}
#' \item{mixing_probability}{a \code{data.frame} which stores the mixing probability
#' of each subclass}
#' \item{log_likelihood}{a \code{data.frame} which stores the log likelihood}
#' \item{classify}{a \code{data.frame} which stores the Class ID that has the maximum
#' posterior probability}

#' @examples
#' \dontrun{
#' connection <- odbcConnect("Gandalf")
#' db_name    <- FL_R_WRAP
#' table_name <- "tblMDA"
#' # Create FLTable object
#' table      <-  FLTable(connection, db_name, table_name)
#' # Perform MDA
#' result     <- FLLDA( table,
#'                      subclasses = 3,
#'                      max_iter = 10,
#'                      initialization = 2,
#'                      hypotheses = 5)
#' # Fetch reults in R
#' mdaResult <- FLFetch(result)
#' }

#' @export
FLMDA <- function( 	table,
					primary_key,
					response,
					subclasses,
					max_iter,
					initialization,
					hypotheses,
					exclude = c(),
					class_spec = list(),
					where_clause = "",
					note = "From RWrapper For DBLytix")
{
	subclasses     <- ifelse(	is_number(subclasses),
								as.integer(subclasses),
								stop("subclasses should be an integer"))

	max_iter       <- ifelse(	is_number(max_iter),
								as.integer(max_iter),
								stop("max_iter should be an integer"))	

	initialization <- ifelse(	(initialization == 1 || initialization == 2),
								as.integer(initialization),
								stop("initialization should be 1 or 2"))

	hypotheses     <- ifelse(	is_number(hypotheses),
								as.integer(hypotheses),
								stop("hypotheses should be an integer"))
	

	argList  <- as.list(environment())
	typeList <- list(	primary_key    = "character",
						response       = "character",
						subclasses     = "integer",					
						max_iter       = "integer",
						initialization = "integer",
						hypotheses     = "integer",
						exclude        = "character",
						class_spec     = "list",
						where_clause   = "character",
						note           = "character")
	classList <- list(	table        = "FLTable")
	validate_args(argList, typeList, classList)
	obsID  <- "ObsID"
	varID  <- "VarID"
	value  <- "Num_Val"

	dataPrepRes 			<- regr_data_prep( 	table,
												response,
												obs_id = obsID,
												var_id = varID,
												value = value,
												primary_key = primary_key,
												exclude = exclude,
												class_spec = class_spec,
												where_clause = where_clause)

	deepTableName       	<- dataPrepRes$deepTableName
	wideToDeepAnalysisID 	<- dataPrepRes$wideToDeepAnalysisID
	connection         	 	<- table@odbc_connection
	
	file <- "FLMDA.sql"
	sqlParameters <- list(	deepTableName  = deepTableName,
							obsID          = obsID,
							varID          = varID,
							value          = value,
							whereClause    = where_clause,
							subclasses     = subclasses,
							maxIter        = max_iter,
							initialization = initialization,
							hypotheses     = hypotheses,
							note           = note )
	#run FLMDA
	mdaRes     <- run_sql(connection, file, sqlParameters)
	
	#print(mdaRes)
	analysisID <- toString(mdaRes[[1,"ANALYSISID"]])
	retData    = new("FLMDA",analysis_id = analysisID, odbc_connection = connection)

	return(retData)
}
