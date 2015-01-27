#' @include utilities.R
#' @include data_prep.R
#' @include FLFetch.R
NULL

#' Stepwise Logistic Regression 
#'
#' Chooses a Logistic Regression model by eliminating predictors by PValue with
#' a Stepwise Algorithm 
#' 
#'@details Each of the algorithm works as follows:
#' \describe{
#' \item{BW}{In Backward Elimination variables are eliminated one at a time till 
#' all the p-Values are lower than value specified by the parameter \code{p_allow}} 
#' \item{FB}{In Fast Backward Elimination, all the variables that exceed the 
#' value specified by the parameter \code{p_allow_initial} are dropped in one go. 
#' Thereafter, only one variable is dropped at a time till all the p-Values are
#' below \code{p_allow_final} }
#' \item{UFB}{In Ultra Fast Backward Elimination, the \code{p_allow_initial} is
#' revised downward by \code{stepwise_decrease} after each step and all variables
#' that exceed \code{p_allow_initial} are dropped till all the p-Values are
#' below \code{p_allow_final} }
#' \item{SW}{Stepwise Logistic regression starts with 1-variable models built
#' individually with all the variables and then it shortlists the top N number
#' of models based on their respective Gini coefficient estimates. Then it adds
#' an additional variable to each of the top N models and filters the top N
#' models again based on their Gini coefficient estimates. This process continues
#' till no additional variables can be added because the p-Value of the model
#' exceeds \code{p_allow}.}
#' }
#'
#'@param table an object of class \code{FLTable}
#'@param primary_key name of primary key column of the table mapped to \code{table}
#'@param response name of the dependent variable column
#'@param type one of \{ "BW", "FB", "UFB", "SW" \} standing for Backward, 
#' Fast Backward, Ultra-Fast Backward and Stepwise algorithm respectively
#'@param max_iter Maximum number of iterations done before terminating the algorithm
#'@param threshold threshold for False positive value used to calculate the 
#' false positives and false negatives
#'@param p_allow (For "BW" and "SW") see details
#'@param p_allow_initial (For "FB" and "UFB") see details
#'@param p_allow_final (For "FB" and "UFB") see details
#'@param stepwise_decrease (For "UFB") see details
#'@param top_n (For "SW") see details
#'@param include vector of names of the columns which are not to be dropped
#'@param exclude vector of names of the columns which are to be excluded
#'@param class_spec list that identifies the value of the categorical variable
#' which is to be used as reference when converting to dummy binary variables
#'@param where_clause condition to filter out data from the table
#'@param note free form string that will be stored with the results, typically 
#' used to document the purpose of the analysis
#'
#'@return an object of class \code{FLLogRegr}. For details see \code{\link{FLLogRegr}}
#'
#'@examples
#'
#'
#' \dontrun{
#'
#' }
#'
#'
#'@export
FLLogRegrStep <- function( 	table,
							primary_key,
							response,
							type,
							max_iter,
							threshold,
							p_allow = 0.05,
							p_allow_initial = 0.3,
							p_allow_final = 0.05,
							stepwise_decrease = 0.05,
							top_n = 5,
							include      = c(),
							exclude      = c(),
							class_spec    = list(),
							where_clause  = "",
							note     = "From RWrapper For DBLytix")
{
	
	# Types of Stepwise Regression
	# BW  - Backward
	# FB  - Fast Backward
	# UFB - Ultra Fast Backward
	# SW  - Stepwise  
	types <- c("BW","FB","SW","UFB")
	if(type %in% types)
	{
		obsID  <- "ObsID";
		varID  <- "VarID";
		value  <- "Num_Val";
		
		dataPrepRes 			<- regr_data_prep( 	table,
													response,
													obs_id = obsID,
													var_id = varID,
													value = value,
													primary_key   = primary_key,
													exclude      = exclude,
													class_spec    = class_spec,
													where_clause  = where_clause);

		deepTableName        	<- dataPrepRes$deepTableName
		wideToDeepAnalysisID 	<- dataPrepRes$wideToDeepAnalysisID
		connection         		<- table@odbc_connection;
		specID 					<- "";
		
		if(length(include) > 0)
		{
			specID      <- gen_spec_ID(table@table_name);
			varMapQuery <- var_name_to_ID(wideToDeepAnalysisID,include);
			res         <- sqlQuery(connection,varMapQuery,stringsAsFactors = FALSE);		
			varIDs      <- unlist(res$VarID);
			queries     <- sapply(varIDs,function(col) paste("INSERT INTO fzzlLogRegrModelVarSpec VALUES ('",specID,"',",col,",'I')",sep="") );
			insertions  <- sapply(queries,function(query) sqlQuery(connection,query) );
		}

		if(type == "BW")
		{
			file          <- "FLLogRegrBW.sql"
			sqlParameters <- list(	deepTableName = deepTableName,
									obsID         = obsID,  
									varID         = varID, 
									value         = value,
									max_iter      = max_iter,
									threshold     = threshold,
									specID        = specID,
									pAllow        = p_allow, 							
									note          = note )
		}
		if(type == "FB")
		{
			file          <- "FLLogRegrFB.sql"
			sqlParameters <- list( 	deepTableName = deepTableName,
									obsID         = obsID,
									varID         = varID,
									value         = value,
									max_iter      = max_iter,
									threshold     = threshold,
									specID        = specID,
									pAllowInitial = p_allow_initial,
									pAllowFinal   = p_allow_final,
									note          = note )			
		}
		if(type == "UFB")
		{
			file          <- "FLLogRegrUFB.sql"
			sqlParameters <- list( 	deepTableName    = deepTableName,
									obsID            = obsID,
									varID            = varID,
									value            = value,
									max_iter      = max_iter,
									threshold     = threshold,
									specID           = specID,
									pAllowInitial    = p_allow_initial,
									pAllowFinal      = p_allow_final,
									stepwiseDecrease = stepwise_decrease,
									note             = note )			
		}
		if(type == "SW")
		{
			file          <- "FLLogRegrSW.sql"
			sqlParameters <- list(	deepTableName = deepTableName,
									obsID         = obsID,  
									varID         = varID, 
									value         = value,
									maxIter      = max_iter,
									threshold     = threshold,
									topN          = top_n,
									pAllow        = p_allow, 							
									note          = note )			
		}

		# if(type == "BW")
		# {
		# 	sql        		<- "CALL FLLogRegrBW('";
		# 	sqlParameters 	<- paste(	deepTableName,
		# 								obsID,  
		# 								varID, 
		# 								value,
		# 								max_iter,
		# 								threshold,
		# 								specID,
		# 								p_allow, 							
		# 								note, sep="','")
		# 	sql				<- paste(sql, sqlParameters,"', AnalysisID)", sep="")
		# }
		# if(type == "FB")
		# {
		# 	sql        		<- "CALL FLLogRegrFB('";
		# 	sqlParameters 	<- paste(	deepTableName,
		# 								obsID,  
		# 								varID, 
		# 								value,
		# 								max_iter,
		# 								threshold,
		# 								specID,
		# 								p_allow_initial, 							
		# 								p_allow_final,
		# 								note, sep="','")
		# 	sql				<- paste(sql, sqlParameters,"', AnalysisID)", sep="")
		# }
		# if(type == "UFB")
		# {
		# 	sql        		<- "CALL FLLogRegrUFB('";
		# 	sqlParameters 	<- paste(	deepTableName,
		# 								obsID,  
		# 								varID, 
		# 								value,
		# 								threshold,
		# 								max_iter,
		# 								specID,
		# 								p_allow_initial,
		# 								p_allow_final,
		# 								stepwise_decrease, 							
		# 								note, sep="','")
		# 	sql				<- paste(sql, sqlParameters,"', AnalysisID)", sep="")
		# }
		# if(type == "SW")
		# {
		# 	sql        		<- "CALL FLLogRegrSW('";
		# 	sqlParameters 	<- paste(	deepTableName,
		# 								obsID,  
		# 								varID, 
		# 								value,
		# 								max_iter,
		# 								threshold,
		# 								top_n,																	
		# 								p_allow, 							
		# 								note, sep="','")
		# 	sql				<- paste(sql, sqlParameters,"', AnalysisID)", sep="")
		# }
        		
		logRegrRes        	<- run_sql(connection, file, sqlParameters)	
		analysisID       	<- toString(logRegrRes[1,"AnalysisID"]);
		retData = new("FLLogRegr",analysis_id = analysisID, odbc_connection = connection, deep_table_name = deepTableName, wide_to_deep_analysis_id = wideToDeepAnalysisID);
	}
	else
	{
		stop("Incorrect value for type parameter. Type must be in {\"BW\",\"FB\",\"UFB\",\"SW\"} ")
	}
	
}
