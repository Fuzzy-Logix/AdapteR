# FLFitContDistr Object
setOldClass("RODBC")
#' @export
setClass(	"FLFitContDistr",
			slots = list(	odbc_connection = "RODBC",
							db_name         = "character",
							table_name      = "character",
							value  			= "character"))

# FLFitDiscDistr Object
#' @export
setClass(	"FLFitDiscDistr",
			slots = list(	odbc_connection = "RODBC",
							db_name         = "character",
							table_name      = "character",
							num_success 	= "character",
							num_trials 		= "character"))

#' Constructor function for FLFitDistr
#'
#' @details \code{FLFitDistrObject} constructs an object of class \code{FLFitContDistr}
#' or \code{FLFitDiscDistr} depending on whether a continuous or a discrete
#' distribution is to be fitted. This object is used as the input for fit
#' distribution functions.

#' @param connection ODBC connection handle as returned by \code{\link[RODBC]{odbcConnect}}
#' @param db_name name of the database in \code{Teradata} which contains the table
#' @param table_name name of the  table which contains the points for distribution
#' fitting
#' @param distribution_type type of distribution which is to be fitted. It has
#' to be "Cont" for continuous distribution or "Disc" for discrete distribution.
#' @param value name of the column which has the value of the input data points.
#' This is required only if the distribution is a continuous one.
#' @param num_success name of the column which has the number of successes for
#' a binomial distribution
#' @param num_trials name of the column which has the number of trials for a
#' binomial distribution

#' @return \code{FLFitDistrObject} returns an object of class \code{FLFitContDistr}
#' or or \code{FLFitDiscDistr} which is mapped to a table in Teradata. An object
#' of any of the above class has the following common components:
#' \describe{
#' \item{odbc_connection}{}
#' \item{db_name}{}
#' \item{table_name}{}
#' }
#' In addition to the above components, an object of class \code{FLFitContDistr}
#' contains the component \code{value}. \cr
#' An object of class \code{FLFitDiscDistr} contains the components
#' \code{num_success} and \code{num_trials}.

#' @examples
#' \dontrun{
#'    connection  <- odbcConnect("Gandalf")
#' 		database    <- "FL_R_WRAP"
#' 		table_name  <- "RWrapperFitNormalDistrTest"
#'   	# Create distribution object
#'   	data        <-  FLFitDistrObject( connection,
#'                                      db_name,
#'                                      table_name,
#'                                      distribution_type = "Cont")
#' 		# Fit Distribution
#'   	result      <- FLFitDistr(Data, "Normal", "MDE"))
#' }
#' @export
FLFitDistrObject <- function(connection, db_name, table_name, distribution_type, value = "NumVal", num_success = "NumOfSuccess", num_trials = "NumOfTrials")
{
	argList  <- as.list(environment())
	typeList <- list(	connection 			= "integer",
						db_name				= "character",
						table_name			= "character",
						distribution_type	= "character",
						value				= "character",												
						num_success			= "character",
						num_trials			= "character")
	validate_args(argList, typeList, classList = list())
	
	# DistributionTypes
	# Cont - Continuous
	# Disc - Discrete
	types <- c("Cont", "Disc")
		if(distribution_type %in% types)
		{
			sqlQuery(connection, paste("DATABASE", db_name))
			sqlQuery(connection, "SET ROLE ALL")

			if(distribution_type == "Cont")
			{
				new("FLFitContDistr", odbc_connection = connection, db_name = db_name, table_name = table_name, value = value)
			}
			else
			{
				new("FLFitDiscDistr", odbc_connection = connection, db_name = db_name, table_name = table_name, num_success = num_success, num_trials = num_trials)
			}
		}
		else
		{
			stop("Incorrect value for distribution_type parameter. distribution_type must be in {\"Cont\",\"Disc\"} ")
		}
}

# Wrapper for fitting distribution
#' Distribution fitting
#'
#' Fits a given distribution to a set of data points
#' @details \code{FLFitDistr} estimates parameter values of a given distribution
#' for a given data set. One of the methods used in fitting distribution is
#' by this function is Maximum Likelihood Estimation (MLE), that is estimating
#' parameters by maximizing log likelihood function. Apart from MLE, the function
#' can also estimate the parameters Minimum Distance Estimation (MDE), that is
#' estimating parameters by minimizing distance function.
#' @section Note:
#' \enumerate{
#'  \item When the starting values are very far away from the actual values or
#'  parameters, e.g. location, scale, shape, distance, probability, etc., do
#'  not converge, the functions will output NULLs.
#'  \item For certain distributions, if the input values contain zeros, the
#'  function may throw a system error such as, divide by zero or invalid
#'  arithmetic operation.
#' }
#' @section Distributions:
#' The following distributions can be fitted to a data set using \code{FLFitDistr}.
#' \enumerate{
#'  \item \code{Beta}
#'  \item \code{Binomial}
#'  \item \code{Cauchy}
#'  \item \code{ChiSq}
#'  \item \code{Normal}
#'  \item \code{Poisson}
#'  \item \code{Weibull}
#' }
#' 
#' @param distribution_object an object generated by \code{FLFitDistrObject}
#' which is mapped to a table in Teradata
#' @param distribution name of the distribution which is to be fitted. The
#' distribution to be fitted should be one from the list given in the
#' \code{Distributions} section
#' @param method indicator of the method used for estimating the fit distribution
#' parameters. Use \code{"MLE"} for Maximum Likelihood Estimation Method and
#' \code{"MDE"} for Minimum Distance Estimation Method
#' @return \code{FLFitDistr} returns a \code{data.frame} which has the estimates
#' of the parameters along with the value of the log-likelihood or the distance.
#' The parameters depend on the distribution being fitted.
#' @export
FLFitDistr <- function(distribution_object, distribution, method = "MLE")
{ 
	argList  <- as.list(environment())
	typeList <- list(	distribution	= "character",
						method			= "character")
	validate_args(argList, typeList, classList = list())
	# Distribution
  # -> Beta
  # -> Binomial
  # -> Cauchy
  # -> ChiSq
  # -> Poisson
  # -> Normal
	# -> Weibull


	# Method
	# MLE - Maximum Likelihood Estimation
	# MDE - Minimum Distance Estimation
	distributionObjectName <- c("FLFitContDistr", "FLFitDiscDistr")
	if(class(distribution_object) %in% distributionObjectName)
	{
		stop("Argument Type Mismatch: distribution_object must be in {\"FLFitContDistr"\, \"FLFitDiscDistr"\}")
	}
	distributionName <- c("Beta", "Binomial", "Cauchy", "ChiSq", "Normal", "Poisson", "Weibull")
	methodType       <- c("MLE", "MDE")
	if(distribution %in% distributionName && method %in% methodType)
	{
		if(class(distribution_object) == "FLFitContDistr")
		{
			path <- "FLFitContDistr.sql"
			stopifnot(file.exists(path))
			sql  <- readChar(path, nchar = file.info(path)$size)
			sql  <- sprintf(	sql,
								distribution_object@value,
								distribution_object@table_name,
								method,
								distribution)
			sql  <- gsub("[\r\n]", "", sql)
			print(sql)
			res <- sqlQuery(distribution_object@odbc_connection, sql, stringsAsFactors = FALSE)
		}

		if(class(distribution_object) == "FLFitDiscDistr")
		{
			path <- "FLFitDiscDistr.sql"
			stopifnot(file.exists(path))
			sql  <- readChar(path, nchar = file.info(path)$size)
			sql  <- sprintf(	sql,
								distribution_object@num_trials,
								distribution_object@num_success,
								distribution_object@table_name,
								method,
								distribution)
			sql  <- gsub("[\r\n]", "", sql)
			print(sql)
			res <- sqlQuery(distribution_object@odbc_connection, sql, stringsAsFactors = FALSE)
		}
	return(res)
	}

	else if((distribution %in% distributionName) == FALSE)
	{
		stop("Incorrect value for distribution parameter. distribution must be in {\"Beta\",\"Binomial\", \"Cauchy\", \"ChiSq\", \"Normal\", \"Poisson\", \"Weibull\"} ")
	}

	else
	{
		stop("Incorrect value for method parameter. method must be in {\"MLE\",\"MDE\"} ")
	}
}

