#' @include FLMatrix.R
NULL

## move to file FLVarCluster.R
#' Variable Clustering.
#'
#' \code{FLVarCluster} performs variable clustering on FLTable objects
#' using Principal Component Analysis
#'
#' The DB Lytix function called is FLVarCluster. Uses a principal component analysis for dimensionality 
#' reduction in order to cluster a given set of input variables into a smaller representative set.
#' The number of output clusters depend on the contribution level specified.
#'
#' @seealso \code{\link[ClustOfVar]{ClustOfVar}} package for R reference implementation.
#'
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
#' deeptable  <- FLTable(getTestTableName("tblLogRegr"), "ObsID","VarID",
#'                       "Num_Val", whereconditions= "ObsID<101")
#' clustervector <- FLVarCluster(deeptable,0.75,"COVAR",whereconditions=" VarID>0 ")
#' print(clustervector)
#' @export
FLVarCluster <- function (x, ...) {
  UseMethod("FLVarCluster", x)
}

## move to file FLVarCluster.R
#' @export
FLVarCluster.FLTable<-function(x,
							contrib,
							matrixType = "COVAR",
							groupBy = "NULL",
							excludeCols = as.character(c()),
							classSpec = list(),
							whereconditions = "",
                            ...
							)
{
	#Type validation
	if(!(base::toupper(matrixType) %in% c("COVAR","CORREL","TRUE","FALSE")))
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

    connection <- getFLConnection(x)
    wideToDeepAnalysisID <- ""
    mapTable <- ""
	
	if(!isDeep(x)){
		deepx <- wideToDeep(x,excludeCols=excludeCols,
							classSpec=classSpec,
							whereconditions=whereconditions)

		wideToDeepAnalysisID <- deepx@wideToDeepAnalysisID
		deepx <- setAlias(deepx,"")
		whereconditions <- ""

		sqlstr <- paste0(" SELECT Final_VarID AS vectorIndexColumn,",
						 " CASE WHEN CatValue IS NOT NULL THEN ",
						 " CONCAT(COLUMN_NAME,CatValue) else COLUMN_NAME END AS columnName",
			    	     " FROM ",getSystemTableMapping("fzzlRegrDataPrepMap"),
                                           "  a ",
			    	     " WHERE a.AnalysisID = '",wideToDeepAnalysisID,"'",
			    	     " AND a.Final_VarID IS NOT NULL ")
		mapTable <- createTable(pTableName=gen_wide_table_name("map"),
                                        pSelect=sqlstr)
	}
	else if(class(x@select)=="FLTableFunctionQuery")
	{
		deeptablename <- createView(pViewName=gen_view_name(""),
                                            pSelect=constructSelect(x))

		#sqlstr <- paste0("CREATE VIEW ",getOption("ResultDatabaseFL"),".",deeptablename1,
		#	" AS SELECT * FROM ",getOption("ResultDatabaseFL"),".",deeptablename,constructWhere(whereconditions))
		#t <- sqlSendUpdate(connection,sqlstr)
		deeptablename1 <- createView(pViewName=gen_view_name("New"),
                                             pSelect=paste0("SELECT * FROM ",deeptablename,constructWhere(whereconditions)))

        deepx <- FLTable(deeptablename1,
                        getObsIdSQLName(x),
                        getVarIdSQLName(x),
                        getValueSQLName(x))
		# deepx <- FLTable(
  #                  deeptablename1,
  #                  "obs_id_colname",
  #                  "var_id_colname",
  #                  "cell_val_colname"
  #                 )
		deepx <- setAlias(deepx,"")
		whereconditions <- ""
	}
	else
	{
		x@select@whereconditions <- c(x@select@whereconditions,whereconditions)
		deeptablename <- createView(pViewName=gen_view_name("New"),
                                            pSelect=constructSelect(x))

        deepx <- FLTable(deeptablename,
                        getObsIdSQLName(x),
                        getVarIdSQLName(x),
                        getValueSQLName(x))

		# deepx <- FLTable(
  #                  deeptablename,
  #                  "obs_id_colname",
  #                  "var_id_colname",
  #                  "cell_val_colname"
  #                 )
		deepx <- setAlias(deepx,"")
		whereconditions <- ""
	}

	whereconditions <- whereconditions[whereconditions!=""]
	whereClause <- constructWhere(whereconditions)
	deeptable <- getTableNameSlot(deepx)
	if(whereClause!="") whereClause <- paste0("' ",whereClause," '")
	else whereClause <- "NULL"
    
    if("TableOutput" %in% names(list(...)))
        vTableOutput <- list(...)[["TableOutput"]]
    else vTableOutput <- 1
    retobj <- sqlStoredProc(
        connection,
        "FLVarCluster",
        TableName=deeptable,
        ObsIDColName=getObsIdSQLExpression(deepx),
        VarIDColName=getVarIdSQLExpression(deepx),
        ValueColName=getValueSQLExpression(deepx),
        WhereClause= whereClause,
        GroupBy=groupBy,
        MatrixType=matrixType,
        Contrib=contrib,
        TableOutput=vTableOutput,
        Note="FLVarCluster from AdapteR",
        outputParameter=c(ResultTable="a")
        )
	outputTable <- as.character(retobj[1,1])

	# sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
	# 					getVarIdSQLExpression(deepx)," AS vectorIndexColumn,",
	# 					" ClusterID AS vectorValueColumn",
	# 				" FROM ",outputTable)
    
    sqlstr <- getFLVectorSQLFLVarCluster(deepx,outputTable)
	tblfunqueryobj <- new("FLTableFunctionQuery",
                        connectionName = attr(connection,"name"),
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

	clustervector <- newFLVector(
						select = tblfunqueryobj,
						Dimnames = list(deepx@Dimnames[[2]],
										"vectorValueColumn"),
						isDeep = FALSE)

	clustervector <- tryCatch(as.vector(clustervector),
  						error=function(e){
  							cat("could not fetch data. Storing in Volatile table.\n")
  							if(!isDeep(x))
  							{
  								cat("The mapping table is ",mapTable)
  								cat("Use the mapping table for relation between\n",
	  								" column Names in input wide table and ",
	  								" variable IDs in vectorIndexColumn in FLVector\n")
  							}
  							return(clustervector)})

	if(!isDeep(x))
		{
			sqlstr <- paste0(" SELECT a.columnName AS vcolnames \n ",
                            " FROM ",mapTable," a,",outputTable," b \n ",
                            " WHERE a.vectorIndexColumn=b.vectorIndexColumn ")
			names(clustervector) <- sqlQuery(getFLConnection(),sqlstr)[["vcolnames"]]
			t <- sqlSendUpdate(" DROP TABLE ",mapTable)
		}
	return(clustervector)
}

## move to file FLVarCluster.R
#' @export
FLVarCluster.FLMatrix <- function(x,
                            contrib,
                            matrixType = "COVAR",
                            groupBy = "NULL",
                            excludeCols = as.character(c()),
                            classSpec = list(),
                            whereconditions = ""
                        )
{
    x <- as.FLTable(x)
    return(FLVarCluster(x=x,
                contrib=contrib,
                matrixType = matrixType,
                groupBy = groupBy,
                excludeCols = excludeCols,
                classSpec = classSpec,
                whereconditions = whereconditions))
}

#' @export
FLVarCluster.FLTable.Hadoop <- function(x,
                                        contrib,
                                        matrixType = "COVAR",
                                        groupBy = "NULL",
                                        excludeCols = as.character(c()),
                                        classSpec = list(),
                                        whereconditions = ""
                                        ){
    vmap <- c(COVAR=FALSE,CORREL=TRUE)
    matrixType <- vmap[matrixType]
    if(is.na(matrixType))
        stop("matrixType must be COVAR or CORREL \n ")
    FLVarCluster.FLTable(x=x,
                        contrib=contrib,
                        matrixType=matrixType,
                        groupBy=groupBy,
                        excludeCols=excludeCols,
                        classSpec=classSpec,
                        whereconditions=whereconditions,
                        TableOutput="")
}

#' @export
FLVarCluster.FLTable.TDAster <- function(x,
                                        contrib,
                                        matrixType = "COVAR",
                                        groupBy = "NULL",
                                        excludeCols = as.character(c()),
                                        classSpec = list(),
                                        whereconditions = ""
                                        ){
    vOutTable <- gen_wide_table_name("VarClust")
    FLVarCluster.FLTable(x=x,
                        contrib=contrib,
                        matrixType=matrixType,
                        groupBy=groupBy,
                        excludeCols=excludeCols,
                        classSpec=classSpec,
                        whereconditions=whereconditions,
                        TableOutput=vOutTable)
}

#' @export
FLVarCluster.FLTableDeep.TDAster <- FLVarCluster.FLTable.TDAster

#' @export
FLVarCluster.FLTableDeep.Hadoop <- FLVarCluster.FLTable.Hadoop

getFLVectorSQLFLVarCluster <- function(object,outputTable){
    UseMethod("getFLVectorSQLFLVarCluster",object)
}
getFLVectorSQLFLVarCluster.FLTable.Hadoop <- function(object,outputTable){
    getFLVectorTableFunctionQuerySQL(indexColumn="varid",
                                    valueColumn="clusterid",
                                    FromTable=outputTable)
}

getFLVectorSQLFLVarCluster.FLTableDeep.Hadoop <- getFLVectorSQLFLVarCluster.FLTable.Hadoop

getFLVectorSQLFLVarCluster.default <- function(object,outputTable){
    getFLVectorTableFunctionQuerySQL(indexColumn=getVarIdSQLExpression(object),
                                    valueColumn="clusterid",
                                    FromTable=outputTable)
}
