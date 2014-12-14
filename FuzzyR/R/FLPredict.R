#' @include FLTable.R
#' @include data_prep.R
#' @include utilities.R
#' @include FLFetch.R
NULL

#' Scoring function
#'
#' \code{FLPredict} scores a new dataset using an existing data mining analysis
#' model.
#'
#' @details \code{FLPredict} predicts the value of the dependent variable using
#' an existing model for the analysis objects of the following classes.
#' \itemize{
#' \item{FLLinRegr}
#' \item{FLLogRegr}
#' \item{FLNaiveBayes}
#' \item{FLDecisionTree}
#' }
#'
#' @param analysis a fitted model object of the relevant type with the parameters
#' filled in. The predict function currently scores the dataset for the analysis
#' objects mentioned in the Details section.
#' @param table an \code{FLTable} object mapped to the dataset which is to be
#' scored

#' @return \code{FLPredict} returns a \code{data.frame} having the scored value of the dependent variable
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#'@export
setGeneric("FLPredict", function(	analysis,
									table,
									... ) {
  standardGeneric("FLPredict")
})

# score function for FLLinRegr
setMethod(	"FLPredict",
			signature(	analysis = "FLLinRegr",
						table = "FLTable"),
			function(	analysis,
						table)
			{
				obsID 			<- "ObsID"
				varID 			<- "VarID"
				value 			<- "Num_Val"
				primaryKey   	<- analysis@primary_key
				exclude      	<- analysis@exclude
				classSpec    	<- analysis@class_spec
				inAnalysisID 	<- analysis@wide_to_deep_analysis_id
				whereClause  	<- ""

				#Convert wide table to deep format
				dataPrepRes 	<- regr_data_prep_score( 	table,
															obs_id 			= obsID,
															var_id 			= varID,
															value 			= value,
															primary_key   	= primaryKey,
															exclude     	= exclude,
															class_spec    	= classSpec,
															where_clause  	= whereClause,
															in_analysis_id 	= inAnalysisID)
				deepTableName      		<- dataPrepRes$deepTableName
				wideToDeepAnalysisID 	<- dataPrepRes$wideToDeepAnalysisID
				regrAnalysisID 		 	<- analysis@analysis_id
				connection         		<- table@odbc_connection

				sqlParameters <- list(  deepTableName  = deepTableName,
										obsID          = obsID,
										varID          = varID,
										value          = value,
										whereClause    = whereClause,
										regrAnalysisID = regrAnalysisID)

				#run LinRegrScore
				linRegrScoreRes <- run_sql(connection, "FLLinRegrScore.sql", sqlParameters)
				outTableName    <- toString(linRegrScoreRes[[1]])

				#Fetch
				sqlParameters   <- list(  outTableName  = outTableName )
				scoreTable      <- run_sql(connection, "FLLinRegrScoreFetch.sql", sqlParameters)
				return(scoreTable)
			}
		)

# score function for FLLogRegr
setMethod(	"FLPredict",
			signature(	analysis = "FLLogRegr",
						table = "FLTable"),
			function(	analysis,
						table)
			{
				obsID 		 	<- "ObsID"
				varID 		 	<- "VarID"
				value 		 	<- "Num_Val"
				primaryKey   	<- analysis@primary_key
				exclude      	<- analysis@exclude
				classSpec    	<- analysis@class_spec
				inAnalysisID 	<- analysis@wide_to_deep_analysis_id
				whereClause  	<- ""

				#Convert wide table to deep format
				dataPrepRes 	<- regr_data_prep_score( 	table,
															obs_id 			= obsID,
															var_id 			= varID,
															value 			= value,
															primary_key 	= primaryKey,
															exclude     	= exclude,
															class_spec  	= classSpec,
															where_clause	= whereClause,
															in_analysis_id 	= inAnalysisID)
				deepTableName        	<- dataPrepRes$deepTableName
				wideToDeepAnalysisID 	<- dataPrepRes$wideToDeepAnalysisID
				regrAnalysisID 		 	<- analysis@analysis_id
				connection          	<- table@odbc_connection

				sqlParameters <- list(  deepTableName  = deepTableName,
										obsID          = obsID,
										varID          = varID,
										value          = value,
										whereClause    = whereClause,
										regrAnalysisID = regrAnalysisID)

				#run LinRegrScore
				logRegrScoreRes <- run_sql(connection, "FLLogRegrScore.sql", sqlParameters)
				outTableName    <- toString(logRegrScoreRes[[1]])

				#Fetch
				sqlParameters   <- list(  outTableName  = outTableName )
				scoreTable      <- run_sql(connection, "FLLinRegrScoreFetch.sql", sqlParameters)
				return(scoreTable)

			}
		)

#score function for FLNaiveBayes
setMethod(	"FLPredict",
			signature(	analysis = "FLNaiveBayes",
						table = "FLTable"),
			function(	analysis,
						table,
						predict_table = "")
			{
				obsID 			<- "ObsID"
				varID 			<- "VarID"
				value 			<- "Num_Val"
				primaryKey   	<- analysis@primary_key
				exclude      	<- analysis@exclude
				classSpec    	<- analysis@class_spec
				inAnalysisID 	<- analysis@wide_to_deep_analysis_id
				whereClause  	<- ""

				#Convert wide table to deep format
				dataPrepRes  	<- regr_data_prep_score( 	table,
															obs_id 			= obsID,
															var_id 			= varID,
															value 			= value,
															primary_key   	= primaryKey,
															exclude      	= exclude,
															class_spec    	= classSpec,
															where_clause  	= whereClause,
															in_analysis_id 	= inAnalysisID)
				deepTableName        	<- dataPrepRes$deepTableName
				wideToDeepAnalysisID 	<- dataPrepRes$wideToDeepAnalysisID
				regrAnalysisID       	<- analysis@analysis_id
				connection         		<- table@odbc_connection

				#Generate name of the output table
				if(predict_table == "")
				{
					predict_table 		<- gen_out_table("NaiveBayes",regrAnalysisID)
				}

				sqlParameters <- list(  deepTableName  = deepTableName,
										obsID          = obsID,
										varID          = varID,
										value          = value,
										regrAnalysisID = regrAnalysisID,
										predictTable   = predictTable)

				#run NaiveBayesScore
				naiveBayesScoreRes <- run_sql(connection, "FLNaiveBayesPredict.sql", sqlParameters)

				sqlParameters <- list(  outTableName  = predict_table )
				scoreTable    <- run_sql(connection, "FLLinRegrScoreFetch.sql", sqlParameters)
				return(scoreTable)
			}
		)

# score function for FLDecisionTree
setMethod(	"FLPredict",
			signature(	analysis = "FLDecisionTree",
						table  = "FLTable"),
			function(	analysis,
						table,
						predict_table = "")
			{

				obsID 			<- "ObsID";
				varID 			<- "VarID";
				value 			<- "Num_Val";
				primaryKey   	<- analysis@primary_key;
				exclude      	<- analysis@exclude;
				classSpec    	<- analysis@class_spec;
				inAnalysisID 	<- analysis@wide_to_deep_analysis_id;
				whereClause  	<- "";
				#Convert wide table to deep format
				dataPrepRes 	<- regr_data_prep_score( 	table,
															obs_id 			= obsID,
															var_id 			= varID,
															value 			= value,
															primary_key   	= primaryKey,
															exclude      	= exclude,
															class_spec    	= classSpec,
															where_clause  	= whereClause,
															in_analysis_id 	= inAnalysisID);
				deepTableName        	<- dataPrepRes$deepTableName;
				wideToDeepAnalysisID 	<- dataPrepRes$wideToDeepAnalysisID;
				regrAnalysisID       	<- analysis@analysis_id;
				connection         		<- table@odbc_connection;

				#Generate name of the output table
				if(predict_table == "")
				{
					predict_table 		<- gen_out_table("NaiveBayes",regrAnalysisID);
				}
				sql 					<- "Call FLDecisionTreeMNScore('%s','%s','%s','%s','%s','%s','From RWrapper for DBLytix', OutAnalysisID)";
				sql 					<- sprintf(	sql,
													deepTableName,
													obsID,
													varID,
													value,
													regrAnalysisID,
													predict_table);

				#run DecisionTreeMNScore
				decisionTreeScoreRes 	<- sqlQuery(connection, sql);
				sql             		<- paste("SELECT * FROM", predict_table, "ORDER BY 1", sep = " ");
				scoreTable           	<- sqlQuery(connection, sql);
				return(scoreTable);
			}
		);
