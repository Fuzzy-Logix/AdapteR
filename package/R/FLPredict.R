# Scoring Functions 
setGeneric("FLPredict", function(	analysis, 
									table, 
									... ) {
  standardGeneric("FLPredict")
});

# score function for FLLinRegr
setMethod(	"FLPredict", 
			signature(	analysis = "FLLinRegr",
						table = "FLTable"),
			function(	analysis, 
						table)  
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
															exclude     	= exclude,
															class_spec    	= classSpec,
															where_clause  	= whereClause,
															in_analysis_id 	= inAnalysisID);
				deepTableName      		<- dataPrepRes$deepTableName;
				wideToDeepAnalysisID 	<- dataPrepRes$wideToDeepAnalysisID;
				regrAnalysisID 		 	<- analysis@analysis_id;
				connection         		<- table@odbc_connection;
				outTable 			 	<- "OutTable";
				sql              	 	<- "Call FLLinRegrScore('%s', '%s','%s','%s','%s','%s', 1,outTable"
				sql 				 	<- sprintf(	sql, 
													deepTableName,
													obsID,  
													varID, 
													value, 							
													whereClause, 
													regrAnalysisID)					
				sql            			<- paste(sql,")", sep="");
				
				#run LinRegrScore
				linRegrScoreRes 		<- sqlQuery(connection, sql)
				outTableName    		<- toString(linRegrScoreRes[[1]]);
				sql          			<- paste("SELECT * FROM", outTableName, "ORDER BY 1", sep = " ");
				scoreTable        		<- sqlQuery(connection, sql);
				return(scoreTable);
			}
		);

# score function for FLLogRegr
setMethod(	"FLPredict", 
			signature(	analysis = "FLLogRegr", 
						table = "FLTable"),
			function(	analysis, 
						table)  
			{
				obsID 		 	<- "ObsID";
				varID 		 	<- "VarID";
				value 		 	<- "Num_Val";
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
															primary_key 	= primaryKey,
															exclude     	= exclude,
															class_spec  	= classSpec,
															where_clause	= whereClause,
															in_analysis_id 	= inAnalysisID);					
				deepTableName        	<- dataPrepRes$deepTableName;
				wideToDeepAnalysisID 	<- dataPrepRes$wideToDeepAnalysisID;
				regrAnalysisID 		 	<- analysis@analysis_id;
				connection          	<- table@odbc_connection;
				outTable 			 	<- "OutTable";
				sql               		<- "Call FLLogRegrScore('%s', '%s','%s','%s','%s','%s', 1,outTable"
				sql 				 	<- sprintf(	sql, 
													deepTableName,
													obsID,  
													varID, 
													value, 							
													whereClause, 
													regrAnalysisID)					
				sql            			<- paste(sql,")", sep="");
				
				#run LogRegrScore
				logRegrScoreRes 		<- sqlQuery(connection, sql)
				outTableName    		<- toString(logRegrScoreRes[[1]]);
				sql          			<- paste("SELECT * FROM", outTableName, "ORDER BY 1", sep = " ");
				scoreTable      		<- sqlQuery(connection, sql);
				return(scoreTable);
			}
		);

#score function for FLNaiveBayes
setMethod(	"FLPredict", 
			signature(	analysis = "FLNaiveBayes", 
						table = "FLTable"),
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
				dataPrepRes  	<- regr_data_prep_score( 	table,
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
				sql 					<- "Call FLNaiveBayesPredict('%s','%s','%s','%s','%s','%s','From RWrapper for DBLytix', OutAnalysisID)";
				sql 					<- sprintf(	sql, 
													deepTableName,
													obsID,  
													varID, 
													value, 																		  
													regrAnalysisID,
													predict_table);											
				
				#run NaiveBayesScore
				naiveBayesScoreRes 		<- sqlQuery(connection, sql);							
				sql             		<- paste("SELECT * FROM", predict_table, "ORDER BY 1", sep = " ");
				scoreTable         		<- sqlQuery(connection, sql);							
				return(scoreTable);
			}
		);

# score function for FLDecisionTree
setMethod(	"FL.predict",
			signature(	analysis = "FLDecisionTree", 
						table  = "FLTable", 
						predict_table = ""),
			function(	analysis,
						table)
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