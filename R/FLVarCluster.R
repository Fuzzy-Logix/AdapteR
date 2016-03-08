#' @include utilities.R
#' @include FLTable.R
#' @include FLMatrix.R
#' @include FLVector.R
NULL

#' @export
FLVarCluster <- function (x, ...) {
  UseMethod("FLVarCluster", x)
}


#' Variable Clustering.
#'
#' \code{FLVarCluster} performs variable clustering on FLTable objects
#' using Principal Component Analysis
#'
#' @method FLVarCluster FLTable
#' @param x an object of class FLTable, wide or deep
#' @param contrib Level of contribution expected in the
#' output clusters. Value between 0 and 1
#' @param matrixType whether a correlation matrix or a
#' covariance matrix should be used for Eigenvalue decomposition.
#' Allowed values c("COVAR","CORREL")
#' @param groupBy Comma separated column names identifying
#' each data set. Currently not used and NULL always.
#' @param excludeCols the comma separated character string of columns to be excluded
#' @param classSpec list describing the categorical dummy variables
#' @param whereconditions takes the where_clause as a string 
#' @return \code{FLVarCluster} returns a R vector if data can be fetched
#' or a FLVector of cluster to which each variable or column is assigned.
#' @section Constraints:
#' If classSpec is not specified, the categorical variables are excluded
#' from analysis by default.
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' deeptable  <- FLTable( "FL_DEMO", "tblLogRegr", "ObsID","VarID","Num_Val")
#' clustervector <- FLVarCluster(deeptable,0.75,"COVAR",whereconditions=" VarID>0 ")
#' print(clustervector)
#' @export
FLVarCluster.FLTable<-function(x,
							contrib,
							matrixType = "COVAR",
							groupBy = "NULL",
							excludeCols = as.character(c()),
							classSpec = list(),
							whereconditions = ""
							)
{

	#Type validation
	if(!(base::toupper(matrixType) %in% c("COVAR","CORREL")))
	stop("matrixType should be in c(COVAR,CORREL)")
	if(!is.numeric(contrib)) stop("contrib should be between 0 and 1")
	if(contrib[1]>1 || contrib[1]<0) stop("contrib should be between 0 and 1")

	argList  <- as.list(environment())

	typeList <- list(
						excludeCols  = "character",
						classSpec   = "list",
						whereconditions = "character"
					)

	classList <- list(x = "FLTable")
	validate_args(argList, typeList, classList)

    connection <- getConnection(x)
    wideToDeepAnalysisId <- ""
    mapTable <- ""
	
	if(!x@isDeep){
		deepx <- wideToDeep(x,excludeCols=excludeCols,
							classSpec=classSpec,
							whereconditions=whereconditions)

		wideToDeepAnalysisId <- deepx[["AnalysisID"]]
		deepx <- deepx[["table"]]
		whereconditions <- ""
		mapTable <- getRemoteTableName(getOption("ResultDatabaseFL"),
					gen_wide_table_name("map"))

		sqlstr <- paste0(" CREATE TABLE ",mapTable," AS ( ",
			    	     " SELECT Final_VarID AS vectorIndexColumn,",
						 " CASE WHEN CatValue IS NOT NULL THEN ",
						 " CONCAT(COLUMN_NAME,CatValue) else COLUMN_NAME END AS columnName",
			    	     " FROM fzzlRegrDataPrepMap a ",
			    	     " WHERE a.AnalysisID = '",wideToDeepAnalysisId,"'",
			    	     " AND a.Final_VarID IS NOT NULL) WITH DATA")
		
		sqlSendUpdate(connection,sqlstr)
	}
	else if(class(x@select)=="FLTableFunctionQuery")
	{
		deeptablename <- genRandVarName()
		sqlstr <- paste0("CREATE VIEW ",getOption("ResultDatabaseFL"),".",deeptablename," AS ",constructSelect(x))
		sqlSendUpdate(connection,sqlstr)

		deeptablename1 <- gen_deep_table_name("New")
		sqlstr <- paste0("CREATE VIEW ",getOption("ResultDatabaseFL"),".",deeptablename1,
			" AS SELECT * FROM ",getOption("ResultDatabaseFL"),".",deeptablename,constructWhere(whereconditions))
		t <- sqlQuery(connection,sqlstr)
		if(length(t)>1) stop("Input Table and whereconditions mismatch,Error:",t)

		deepx <- FLTable(
                   getOption("ResultDatabaseFL"),
                   deeptablename1,
                   "obs_id_colname",
                   "var_id_colname",
                   "cell_val_colname"
                  )
		whereconditions <- ""
	}
	else
	{
		x@select@whereconditions <- c(x@select@whereconditions,whereconditions)
		deeptablename <- gen_deep_table_name("New")
		sqlstr <- paste0("CREATE VIEW ",getOption("ResultDatabaseFL"),".",deeptablename," AS ",constructSelect(x))
		t <- sqlQuery(connection,sqlstr)
		if(length(t)>1) stop("Input Table and whereconditions mismatch")
		deepx <- FLTable(
                   getOption("ResultDatabaseFL"),
                   deeptablename,
                   "obs_id_colname",
                   "var_id_colname",
                   "cell_val_colname"
                  )
		whereconditions <- ""
	}

	whereconditions <- whereconditions[whereconditions!=""]
	whereClause <- constructWhere(whereconditions)
	deeptable <- paste0(deepx@select@database,".",deepx@select@table_name)
	if(whereClause!="") whereClause <- paste0("' ",whereClause," '")
	else whereClause <- "NULL"

    sqlstr <- paste0("CALL FLVarCluster( '",deeptable,"',
			 					   '",getVariables(deepx)[["obs_id_colname"]],"',
			 					   '",getVariables(deepx)[["var_id_colname"]],"',
			 					   '",getVariables(deepx)[["cell_val_colname"]],"',",
			 					   whereClause,",",
			 					   "NULL,'",
			 					   matrixType,"',",
			 					   contrib,",1,ResultTable);")
	
	retobj <- sqlQuery(connection,sqlstr)
	outputTable <- as.character(retobj[1,1])

	sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
						getVariables(deepx)[["var_id_colname"]]," AS vectorIndexColumn,",
						" ClusterID AS vectorValueColumn",
					" FROM ",outputTable)

	tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = connection,
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

	clustervector <- new("FLVector",
						select = tblfunqueryobj,
						dimnames = list(deepx@dimnames[[2]],
										"vectorValueColumn"),
						isDeep = FALSE)

	clustervector <- tryCatch(as.vector(clustervector),
  						error=function(e){
  							cat("could not fetch data. Storing in Volatile table.\n")
  							if(!x@isDeep)
  							{
  								cat("The mapping table is ",mapTable)
  								cat("Use the mapping table for relation between\n",
	  								" column Names in input wide table and ",
	  								" variable IDs in vectorIndexColumn in FLVector\n")
  							}
  							return(clustervector)})

	if(is.numeric(clustervector) || is.vector(clustervector))
	{
		if(x@isDeep)
		names(clustervector) <- deepx@dimnames[[2]]
		else
		{
			sqlstr <- paste0(" SELECT columnName FROM ",mapTable," ORDER BY vectorIndexColumn")
			names(clustervector) <- sqlQuery(connection,sqlstr)[["columnName"]]
		}
		return(clustervector)
	}
	else
	return(clustervector)
}
