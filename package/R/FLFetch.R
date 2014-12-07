setOldClass("RODBC");

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

setGeneric(	"FLFetch", 
			function(object) 
			{
				standardGeneric("FLFetch")
			}
		)

# FLFetch method for KMeans
setMethod(	"FLFetch",
			signature("FLKMeans"),
			function(object) 
			{
				connection 			<- object@odbc_connection;        		
				#Fetch Centers Dendrogram
				sql           		<- paste("SELECT HypothesisID,Level,ClusterID,VarID,Centroid FROM fzzlKMeansDendrogram WHERE AnalysisID = '", object@analysis_id,"' ORDER BY 1,2,3,4",sep = "");
				kMeansDendrogram 	<- sqlQuery(connection, sql);
					
				#Fetch ClusterID Arrays
				sql           		<- paste("SELECT HypothesisID,ObsID,ClusterID FROM fzzlKMeansClusterID WHERE AnalysisID = '", object@analysis_id,"' ORDER BY 1,2,3",sep = "");
				kMeansClusterID  	<- sqlQuery(connection, sql);
	
				object@centers 		= kMeansDendrogram;
				object@cluster 		= kMeansClusterID;
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
				connection 		<- object@odbc_connection;        		
				#Fetch regression coefficients
				sql             <- "SELECT b.COEFFID,a.VAR_TYPE,a.COLUMN_NAME,a.CATVALUE,b.COEFFVALUE,b.STDERR,b.TSTAT,b.PVALUE,b.NONZERODENSITY,b.CORRELWITHRES
				FROM fzzlRegrDataPrepMap a,fzzlLinRegrCoeffs b 
				WHERE a.AnalysisID = '%s' AND b.AnalysisID ='%s' AND a.Final_VarID = b.COEFFID
				ORDER BY b.COEFFID";
				sql        		<- sprintf(sql, object@wide_to_deep_analysis_id, object@analysis_id);
				sql        		<- gsub("[\r\n]", "", sql);
				linRegrCoeffs 	<- sqlQuery(connection, sql);
				
				#Fetch regression statistics
				sql       		<- paste("SELECT a.* FROM fzzlLinRegrStats a WHERE a.AnalysisID = '", object@analysis_id,"' ",sep = "");
				linRegrStats 	<- sqlQuery(connection, sql);
			
				object@coeffs 	= linRegrCoeffs;
				object@stats  	= linRegrStats;
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
				connection 		<- object@odbc_connection;     		
				#Fetch regression R^2 and VIF
				sql       		<- "SELECT b.VarID,a.VAR_TYPE,a.COLUMN_NAME,a.CATVALUE,b.RSquared,b.VIF FROM fzzlRegrDataPrepMap a,fzzlvifstats b WHERE a.AnalysisID = '%s' AND b.AnalysisID ='%s' AND a.Final_VarID = b.VarID ORDER BY b.VarID";
				sql       		<- sprintf(sql, object@wide_to_deep_analysis_id, object@analysis_id);
				sql       		<- gsub("[\r\n]", " ", sql);
				vifStats     	<- sqlQuery(connection, sql);							
				object@stats 	= vifStats;
				return(object)
			}
		)

# define FLNaiveBayes Class
setClass(	"FLNaiveBayes",
			representation(),
			contains = "FLDataMiningAnalysis")

# define FLDecisionTree Class
setClass("FLDecisionTree", 
		 representation(	node_info  = "data.frame",
							classification = "data.frame"),
							contains = "FLDataMiningAnalysis")
							
# FLFetch method for FLDecisionTree
setMethod(	"FLFetch",
			signature("FLDecisionTree"),
			function(object)
			{
				connection 				<- object@odbc_connection;            
				# Fetch nodes of the decision tree built
				sql 					<- paste("SELECT TreeLevel, NodeID, ParentNodeID, IsLeaf, SplitVarID, SplitVal, ChildNodeLeft, ChildNodeRight, NodeSize, PredictClass, PredictClassProb FROM fzzlDecisionTreeMN WHERE AnalysisID = '", object@analysis_id,"' ORDER BY 1,2,3,4",sep = "");
				nodeInfo 				<- sqlQuery(connection, sql);
				
				# Fetch observations' classification
				sql 					<- paste("SELECT ObsID, ObservedClass, NodeID, PredictedClass, PredictClassProb FROM fzzlDecisionTreeMNPred WHERE AnalysisID = '", object@analysis_id,"' ORDER BY 1,2,3,4",sep = "");
				classification 			<- sqlQuery(connection, sql);
				
				object@node_info 		= nodeInfo;
				object@classification 	= classification;			
				return(object)
			}
		)

# define FLLogRegr Class
setClass("FLLogRegr", 
		 representation(	coeffs  = "data.frame",
							stats = "data.frame"),
							contains = "FLDataMiningAnalysis")
						
# FLFetch method for FLLogRegr
setMethod(	"FLFetch",
			signature("FLLogRegr"),
          	function(object) 
			{
				connection 		<- object@odbc_connection;            
				#Fetch logistic regression coefficients
				sql             <- "SELECT b.COEFFID,a.VAR_TYPE,a.COLUMN_NAME,b.COEFFVALUE,b.STDERR,b.CHISQ,b.PVALUE
				FROM fzzlRegrDataPrepMap a,fzzlLogRegrCoeffs b 
				WHERE a.AnalysisID = '%s' AND b.AnalysisID ='%s' AND a.Final_VarID = b.COEFFID
				ORDER BY b.COEFFID";
				sql        		<- sprintf(sql, object@wide_to_deep_analysis_id, object@analysis_id);
				sql        		<- gsub("[\r\n]", "", sql);
				logRegrCoeffs 	<- sqlQuery(connection, sql);
				
				#Fetch logistic regression statistics
				sql 			<- paste("SELECT MODELID, NUMOFVARS, ITERATIONS, CONCORDANT, DISCORDANT, TIED, TOTALPAIRS, GINICOEFF, CSTATISTIC, GAMMA, HIGHESTPVALUE, EVENTS, NONEVENTS, NUMOFOBS, FALSEPOSITIVE, FALSENEGATIVE FROM fzzlLogRegrStats WHERE AnalysisID = '", object@analysis_id,"' ORDER BY 1,2,3,4",sep = "");
				logRegrStats 	<- sqlQuery(connection, sql);
				
				object@coeffs 	= logRegrCoeffs;
				object@stats 	= logRegrStats;
				return(object)
			}
		)

# define FLLDA Class
setClass("FLLDA", 
		slots = list(	canonical_coeffs  = "data.frame",
						fisher_coeffs = "data.frame",
						canonical_variates  = "data.frame",
						cross_tables = "data.frame"))
						
# FLFetch method for FLLDA
setMethod(	"FLFetch",
			signature("FLLDA"),
			function(object) 
			{
				connection <- object@odbc_connection;            
				# Fetch canonical coefficients table
				sql 				<- paste("SELECT CANTYPE, VARID, CANID, NUM_VAL FROM fzzlLDACanCoeff WHERE AnalysisID = '", object@AnalysisID,"' ORDER BY 1,2,3,4",sep = "");
				canonicalCoeffs 	<- sqlQuery(connection, sql);
				
				# Fetch fisher coefficients table
				sql 				<- paste("SELECT CATNUM_VAL, COEFFID, COEFFNUM_VAL FROM fzzlLDAFisherCoeffs WHERE AnalysisID = '", object@analysis_id,"' ORDER BY 1,2,3",sep = "");
				fisherCoeffs 		<- sqlQuery(connection, sql);
				
				# Fetch canonical variates table
				sql 				<- paste("SELECT OBSID, VARID, NUM_VAL FROM fzzlLDACanVariate WHERE AnalysisID = '", object@analysis_id,"' ORDER BY 1,2,3",sep = "");
				canonicalVariates 	<- sqlQuery(connection, sql);
				
				# Fetch Cross Tables of predicted and observed classification
				sql 				<- paste("SELECT Y, PREDICTEDY, OBS_COUNT FROM fzzlLDACrossTab WHERE AnalysisID = '", object@analysis_id,"' ORDER BY 1,2,3",sep = "");
				crossTables 		<- sqlQuery(connection, sql);
				
				object@canonical_coeffs 	= canonicalCoeffs;
				object@fisher_coeffs 		= fisherCoeffs;
				object@canonical_variates 	= canonicalVariates;
				object@cross_tables 		= crossTables;
			
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
				connection <- object@odbc_connection;            
				#Fetch cluster probability table
				sql <- paste("SELECT HypothesisID, ObsID, ClassID, SubclassID, Weight FROM fzzlMDAWeight WHERE AnalysisID = '", object@analysis_id,"' ORDER BY 1,2,3,4,5",sep = "");
				clusterProbability <- sqlQuery(connection, sql);
				
				#Fetch mean vector table
				sql <- paste("SELECT HypothesisID, ClassID, SubclassID, VarID, Mu FROM fzzlMDAMu WHERE AnalysisID = '", object@analysis_id,"' ORDER BY 1,2,3,4",sep = "");
				mu <- sqlQuery(connection, sql);
				
				#Fetch common covariance matrix table
				sql <- paste("SELECT HypothesisID, VarID1, VarID2, Sigma FROM fzzlMDASigma WHERE AnalysisID = '", object@analysis_id,"' ORDER BY 1,2,3,4",sep = "");
				sigma <- sqlQuery(connection, sql);
				
				#Fetch mixing probability table
				sql <- paste("SELECT HypothesisID, ClassID, SubClassID, Mixing FROM fzzlMDAMixing WHERE AnalysisID = '", object@analysis_id,"' ORDER BY 1,2,3,4",sep = "");
				mixingProbabilty <- sqlQuery(connection, sql);
				
				#Fetch log likelihood table
				sql <- paste("SELECT HypothesisID, Iteration, LogLikelihood FROM fzzlMDALogLikelihood WHERE AnalysisID = '", object@analysis_id,"' ORDER BY 1,2,3",sep = "");
				logLikelihood <- sqlQuery(connection, sql);
				
				#Fetch  table which gives the classID that has the maximum posterior probability
				sql <- paste("SELECT HypothesisID, ObsID, ClassID FROM fzzlMDAClassify WHERE AnalysisID = '", object@analysis_id,"' ORDER BY 1,2,3",sep = "");
				classify <- sqlQuery(connection, sql);
				
				object@cluster_probability 	= clusterProbability;
				object@mu 					= mu;
				object@sigma 				= sigma;
				object@mixing_probability 	= mixing_probability;
				object@log_likelihood	 	= logLikelihood;
				object@classify 			= classify;
			
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
				connection <- object@odbc_connection;            
				#Fetch unmatched observation ids Table
				sql <- paste("SELECT * FROM ", object@out_table_name," ORDER BY 1",sep = "");
				unmatchedObsID <- sqlQuery(connection, sql);
			
				object@unmatchedObsID = unmatched_obs_id;
				return(object)
			}
		)
