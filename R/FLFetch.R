#' @include utilities.R
NULL

setOldClass("RODBC")

#' Fetch Data Mining results from Database
#'
#' \code{FLFetch} fetches the slots of a fitted data mining model object from 
#' the database. For details see the documentation for the respective model
#' fitting function
#'
#' @param object an object inherited from class \code{FLDataMiningAnalysis}
#'
#' @return a fitted model object of the relevant type with the parameters
#' filled in
#'
#' @examples
#' \dontrun{
#'		KMeansAnalysis <- FLFetch(KMeansAnalysis)
#' }
#'
#'@export
setGeneric(	"FLFetch", 
			function(object) 
			{
				standardGeneric("FLFetch")
			}
		)

# define FLDataMiningAnalysis Class
setClass(	"FLDataMiningAnalysis", 
			slots = list(	analysis_id           		= "character",
							wide_to_deep_analysis_id 	= "character",						
							deep_table_name        		= "character",
							class_spec            		= "list",
							primary_key           		= "character",
							exclude              		= "character",
							odbc_connection       		= "RODBC"))
							
# define FLKMeans Class
setClass(	"FLKMeans",
			representation(	centers = "data.frame",
							cluster = "data.frame"),
			contains = "FLDataMiningAnalysis")

# FLFetch method for KMeans
setMethod(	"FLFetch",
			signature("FLKMeans"),
			function(object) 
			{
				connection 			<- object@odbc_connection
				sqlParameters <- list( analysisID = object@analysis_id)

				#Fetch Centers Dendrogram			
				kMeansDendrogram 	<- run_sql(connection, "FLKMeansDendrogram.sql", sqlParameters)
					
				#Fetch ClusterID Arrays
				kMeansClusterID  	<- run_sql(connection, "FLKMeansClusterID.sql", sqlParameters)
	
				object@centers 		= kMeansDendrogram
				object@cluster 		= kMeansClusterID
				return(object)
			}
)

# define FLLinRegr Class
setClass(	"FLLinRegr",
			representation(	coeffs 	= "data.frame",
							stats 	= "data.frame"),
			contains = "FLDataMiningAnalysis")

# FLFetch method for LinRegr
setMethod(	"FLFetch",
			signature("FLLinRegr"),
			function(object) 
			{
				connection 	   <- object@odbc_connection        		
				sqlParameters  <- list( 	wideToDeepAnalysisID = object@wide_to_deep_analysis_id,
										analysisID           = object@analysis_id)
				#Fetch regression coefficients
				linRegrCoeffs  <- run_sql(connection, "FLLinRegrCoeffs.sql", sqlParameters)
				
				#Fetch regression statistics
				linRegrStats   <- run_sql(connection, "FLLinRegrStats.sql", sqlParameters)
			
				object@coeffs  = linRegrCoeffs
				object@stats   = linRegrStats
				return(object)
          }
		)

# define FLVIF Class
setClass(	"FLVIF",
			representation(	stats = "data.frame" ),
			contains = "FLDataMiningAnalysis")

# FLFetch method for VIF
setMethod(	"FLFetch",
			signature("FLVIF"),
			function(object)
			{
				connection    <- object@odbc_connection
				sqlParameters <- list( 	wideToDeepAnalysisID = object@wide_to_deep_analysis_id,
										analysisID           = object@analysis_id)
				#Fetch regression R^2 and VIF
				object@stats  <- run_sql(connection, "FLvifstats.sql", sqlParameters)
				return(object)
			}
		)

# define FLNaiveBayes Class
setClass(	"FLNaiveBayes",
			representation(),
			contains = "FLDataMiningAnalysis")

# define FLDecisionTree Class
setClass("FLDecisionTree", 
		 representation(	node_info      = "data.frame",
							classification = "data.frame"),
							contains       = "FLDataMiningAnalysis")
							
# FLFetch method for FLDecisionTree
setMethod(	"FLFetch",
			signature("FLDecisionTree"),
			function(object)
			{
				connection    <- object@odbc_connection            
				sqlParameters <- list( analysisID = object@analysis_id)

				# Fetch nodes of the decision tree built
				nodeInfo       <- run_sql(connection, "FLDecisionTreeFetch.sql", sqlParameters)
				
				# Fetch observations' classification
				classification <- run_sql(connection, "FLDecisionTreePred.sql", sqlParameters)
				
				object@node_info 		= nodeInfo
				object@classification 	= classification			
				return(object)
			}
		)

# define FLLogRegr Class
setClass("FLLogRegr", 
		 representation(	coeffs   = "data.frame",
							stats    = "data.frame"),
							contains = "FLDataMiningAnalysis")
						
# FLFetch method for FLLogRegr
setMethod(	"FLFetch",
			signature("FLLogRegr"),
          	function(object) 
			{
				connection 	  <- object@odbc_connection            
				sqlParameters <- list( 	wideToDeepAnalysisID = object@wide_to_deep_analysis_id,
										analysisID           = object@analysis_id)						
				#Fetch logistic regression coefficients				
				logRegrCoeffs <- run_sql(connection, "FLLogRegrCoeffs.sql", sqlParameters)
				
				#Fetch logistic regression statistics
				logRegrStats  <- run_sql(connection, "FLLogRegrStats.sql", sqlParameters)
				
				object@coeffs 	= logRegrCoeffs
				object@stats 	= logRegrStats
				return(object)
			}
		)

# define FLLDA Class
setClass("FLLDA", 
		slots = list(	canonical_coeffs   = "data.frame",
						fisher_coeffs      = "data.frame",
						canonical_variates = "data.frame",
						cross_tables       = "data.frame"))
						
# FLFetch method for FLLDA
setMethod(	"FLFetch",
			signature("FLLDA"),
			function(object) 
			{
				connection <- object@odbc_connection            
				sqlParameters <- list( analysisID = object@analysis_id)

				# Fetch canonical coefficients table
				canonicalCoeffs   <- run_sql(connection, "FLLDACanCoeff.sql", sqlParameters)
				
				# Fetch fisher coefficients table
				fisherCoeffs      <- run_sql(connection, "FLLDAFisherCoeffs.sql", sqlParameters)
				
				# Fetch canonical variates table
				canonicalVariates <- run_sql(connection, "FLLDACanVariate.sql", sqlParameters)
				
				# Fetch Cross Tables of predicted and observed classification
				crossTables       <- run_sql(connection, "FLLDACrossTab.sql", sqlParameters)
				
				object@canonical_coeffs 	= canonicalCoeffs
				object@fisher_coeffs 		= fisherCoeffs
				object@canonical_variates 	= canonicalVariates
				object@cross_tables 		= crossTables
			
				return(object)
          }
)

# define FMLDA Class
setClass("FLMDA", 
		slots = list(	cluster_probability  	= "data.frame",
						mu 						= "data.frame",
						sigma  					= "data.frame",
						mixing_probability 		= "data.frame",
						log_likelihood 			= "data.frame",
						classify 				= "data.frame"),
						contains 				= "FLDataMiningAnalysis")
						
# FLFetch method for FLMDA
setMethod(	"FLFetch",
			signature("FLMDA"),
			function(object) 
			{
				connection <- object@odbc_connection            
				sqlParameters <- list( analysisID = object@analysis_id)

				#Fetch cluster probability table
				clusterProbability <- run_sql(connection, "FLMDAWeight.sql", sqlParameters)
				#Fetch mean vector table
				mu                 <- run_sql(connection, "FLMDAMu.sql", sqlParameters)		
				#Fetch common covariance matrix table
				sigma              <- run_sql(connection, "FLMDASigma.sql", sqlParameters)		
				#Fetch mixing probability table
				mixingProbabilty   <- run_sql(connection, "FLMDAMixing.sql", sqlParameters)
				#Fetch log likelihood table
				logLikelihood      <- run_sql(connection, "FLMDALogLikelihood.sql", sqlParameters)
				#Fetch  table which gives the classID that has the maximum posterior probability
				classify           <- run_sql(connection, "FLMDAClassify.sql", sqlParameters)
				
				object@cluster_probability 	= clusterProbability
				object@mu 					= mu
				object@sigma 				= sigma
				object@mixing_probability 	= mixingProbabilty
				object@log_likelihood	 	= logLikelihood
				object@classify 			= classify
			
				return(object)
			}
		)

# define FLMatchIt Class
setClass(	"FLMatchIt", 
			slots = list(	odbc_connection 	= "RODBC",
							out_table_name      = "character",
							unmatched_obs_id    = "data.frame"))
							
# FLFetch for FLMatchIt
setMethod(	"FLFetch",
			signature("FLMatchIt"),
			function(object) 
			{
				connection     <- object@odbc_connection            
				sqlParameters  <- list( outTableName = object@out_table_name)				

				#Fetch unmatched observation ids Table
				unmatchedObsID <- run_sql(connection, "FLMatchItFetch.sql", sqlParameters)
				
				object@unmatchedObsID = unmatchedObsID

				return(object)
			}
		)