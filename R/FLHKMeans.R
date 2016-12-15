#' @include FLMatrix.R
NULL

## move to file FLHKMeans.R
#' An S4 class to represent FLHKMeans
#'
#' @slot centers A numeric vector containing the number of clusters, say k which
#' should be greater than zero(centers > 0)
#' @slot AnalysisID A character output used to retrieve the results of analysis
#' @slot wideToDeepAnalysisId A character string denoting the intermediate identifier
#' during widetable to deeptable conversion.
#' @slot table FLTable object given as input on which analysis is performed
#' @slot results A list of all fetched components
#' @slot deeptable A character vector containing a deeptable(either conversion from a 
#' widetable or input deeptable)
#' @slot nstart the initial number of random sets (nstart > 0)
#' @slot mapTable A character string name for the mapping table in-database if input is wide-table.
#' @slot levels A numeric for the number of hierarchical levels which should be greater than zero.
#' @slot maxit maximal number of iterations for the hkmeans algorithm (maxit > 0).
#' @method cluster FLKMeans
#' @param object retrieves the cluster vector
#' @method centers FLKMeans
#' @param object retrieves the coordinates of the centroids
#' @method print FLKMeans
#' @param object overloads the print function
#' @method tot.withinss FLKMeans
#' @param object total within sum of squares
#' @method withinss FLKMeans
#' @param object within sum of squares
#' @method betweenss FLKMeans
#' @param object between sum of squares
#' @method totss FLKMeans
#' @param object total sum of squares
#' @method size FLKMeans
#' @param object size vector
#' @export
setClass(
	"FLHKMeans",
	contains=c("FLDataMining",
			   "FLClustering"),
	slots=list(
		nstart="numeric",
		levels="numeric"
	)
)


## move to file hkmeans.R
#' Hierarchial K-Means Clustering.
#'
#' \code{hkmeans} performs Hierarchial k-means clustering on FLTable objects.
#'
#' The DB Lytix function called is FLHKMeans.Hierarchical K-Means clusters the training data.  
#' The relationship of observations to clusters has hard edges. It re-clusters the training data in 
#' each cluster until the desired hierarchical level is reached.
#' @param x an object of class FLTable, wide or deep
#' @param centers the number of clusters
#' @param levels no.of.levels in the hierarchy
#' @param iter.max the maximum number of iterations allowed
#' @param nstart the initial number of random sets
#' @param excludeCols the comma separated character string of columns to be excluded
#' @param classSpec list describing the categorical dummy variables
#' @param whereconditions takes the where_clause as a string 
#' @return \code{hkmeans} returns a list which replicates equivalent R output
#' from \code{hkmeans} in stats package.The mapping table can be viewed
#' using \code{object$mapping} if input is wide table.
#' @section Constraints:
#' If classSpec is not specified, the categorical variables are excluded
#' from analysis by default.
#' @examples
#' widetable  <- FLTable("tblAbaloneWide", "ObsID")
#' hkmeansobject <- hkmeans(widetable,3,2,20,1,"Rings,SEX")
#' print(hkmeansobject)
#' plot(hkmeansobject)
#' One can specify ClassSpec and transform categorical variables 
#' before clustering. This increases the number of variables in the plot
#' because categorical variable is split into binary numerical variables.
#' The clusters may not be well-defined as is observed in the case below:-
#' hkmeansobjectnew <- hkmeans(widetable,3,2,20,1,"Rings,SEX",list("DummyCat(D)","SEX(M)"))
#' plot(hkmeansobjectnew)
#' @export
hkmeans <- function (x, ...) {
  UseMethod("hkmeans", x)
}

## move to file hkmeans.R
#' @export
hkmeans.FLTable<-function(x,
						centers,
						levels = 1,
						iter.max =10,
						nstart = 1,
						excludeCols = as.character(c()),
						classSpec = list(),
						whereconditions = "",  ## gk @ phani remove this, refactor to prior subsetting with x[...,]
						maxit = 500
						)
{

	#Type validation
	if(any(!(c(centers,iter.max,nstart,levels) >= 1)))
    stop("centers,iter.max,nstart should be atleast 1")
    else
    {
    	centers  <- as.integer(max(centers))
    	iter.max <- as.integer(max(iter.max))
    	nstart <- as.integer(max(nstart))
    	levels <- as.integer(max(levels))
    }

	argList  <- as.list(environment())

	typeList <- list(	centers      = "integer",
						levels = "integer",
						iter.max     = "integer",
						nstart       = "integer",
						excludeCols  = "character",
						classSpec   = "list",
						whereconditions = "character"
					)

	classList <- list(x = "FLTable")
	validate_args(argList, typeList, classList)

    connection <- getFLConnection(x)
    wideToDeepAnalysisId <- ""
    mapTable <- ""
	
	if(!x@isDeep){
		deepx <- wideToDeep(x,excludeCols=excludeCols,
							classSpec=classSpec,
							whereconditions=whereconditions)

		wideToDeepAnalysisId <- deepx[["AnalysisID"]]
		deepx <- deepx[["table"]]
		whereconditions <- ""
        deepx <- setAlias(deepx,"")
		sqlstr <- paste0(" SELECT a.Final_VarID AS VarID, \n ",
			    	     	" a.COLUMN_NAME AS ColumnName, \n ",
			    	     	"  a.FROM_TABLE AS MapName \n ",
			    	    " FROM fzzlRegrDataPrepMap a \n ",
			    	    " WHERE a.AnalysisID = '",wideToDeepAnalysisId,"' \n ",
			    	    " AND a.Final_VarID IS NOT NULL ")
		
		mapTable <- createTable(pTableName=gen_wide_table_name("map"),
                                pSelect=sqlstr)
	}
	else if(class(x@select)=="FLTableFunctionQuery")
	{
		#sqlstr <- paste0("CREATE VIEW ",getOption("ResultDatabaseFL"),
		#				".",deeptablename," AS \n ",
		#				constructSelect(x))
        deepx <- x
        deepx <- setAlias(deepx,"")
        whereconditions <- c(deepx@select@whereconditions,whereconditions)
        deepx@select@whereconditions <- whereconditions[whereconditions!=""]

		deeptablename <- createView(pViewName=gen_view_name(getTableNameSlot(deepx)),
                                    pSelect=constructSelect(deepx))

		deepx <- FLTable(deeptablename,
                        getIndexSQLName(deepx,margin=1),
                        getIndexSQLName(deepx,margin=2),
                        getIndexSQLName(deepx,margin=3)
                        )
        whereconditions <- ""
	}
	else
	{
		deepx <- x
		whereconditions <- c(deepx@select@whereconditions,whereconditions)
		deepx@select@whereconditions <- whereconditions[whereconditions!=""]
        if(length(setdiff(whereconditions,""))>0){
            deeptablename<-createView(pViewName=gen_view_name("New"),
                                    pSelect=constructSelect(deepx))

            deepx <- FLTable(deeptablename,
                            getIndexSQLName(deepx,margin=1),
                            getIndexSQLName(deepx,margin=2),
                            getIndexSQLName(deepx,margin=3)
                            )
        }
        whereconditions <- ""
        deepx <- setAlias(deepx,"")
	}

	whereconditions <- whereconditions[whereconditions!=""]
	whereClause <- constructWhere(whereconditions)
	deeptable <- getTableNameSlot(deepx)
	if(whereClause=="") whereClause <- "NULL"

	retobj <- sqlStoredProc(
                    connection,
                    "FLHKMeans",
                    TableName=deeptable,
                    ObsIDColName=getIndexSQLExpression(deepx,1),
                    VarIDColName=getIndexSQLExpression(deepx,2),
                    ValueColName=getIndexSQLExpression(deepx,3),
                    WhereClause= whereClause,
                    Levels=levels,
                    Clusters=centers,
                    Iterations=iter.max,
                    Hypothesis=nstart,
                    Note=genNote("hkmeans"),
                    outputParameter=c(AnalysisID="a")
                    )

	AnalysisID <- as.character(retobj[1,1])
    ## ######################################
 #    ## gk @ phani: optimize by delete this part, this should be done by 
	# sqlstr<-paste0("SELECT DISTINCT ObsID AS vectorIndexColumn \n ",
	# 				"	FROM fzzlKMeansClusterID \n ",
	# 				"	WHERE AnalysisID = '",AnalysisID,"' AND \n ",
	# 				"	HypothesisID = ",nstart,
	# 					" ORDER BY ObsID")

	# rows <- sqlQuery(connection,sqlstr)[["vectorIndexColumn"]]

	# sqlstr<-paste0("SELECT DISTINCT VarID AS vectorIndexColumn \n ",
	# 				"	FROM fzzlKMeansDendrogram \n ",
	# 				"	WHERE AnalysisID = '",AnalysisID,"' AND \n ",
	# 				"	HypothesisID = ",nstart,
	# 					" ORDER BY VarID")

	# cols <- sqlQuery(connection,sqlstr)[["vectorIndexColumn"]]

	# deepx@Dimnames <- list(rows,cols)
    ## ######################################
    
	if(levels>1)
	{
		# sqlstr <- paste0(" SELECT COUNT(DISTINCT ClusterID) FROM fzzlKMeansClusterID \n ",
		# 				" WHERE AnalysisID='",AnalysisID,"' \n ",
		# 				" AND HypothesisID = ",nstart)
        sqlstr <- constructSelectResult(object,result="levels")

		centers <- sqlQuery(connection,sqlstr)[1,1]
	}
	

	new("FLHKMeans",
		centers=centers,
		AnalysisID=AnalysisID,
		wideToDeepAnalysisId=wideToDeepAnalysisId,
		table=x,
		results=list(),
		deeptable=deepx,
		nstart = nstart,
		mapTable=mapTable,
		levels=levels
	)
}

## move to file FLHKMeans.R
#' @export
`$.FLHKMeans`<-function(object,property)
{
	#parentObject <- deparse(substitute(object))
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
                            "(",fixed=T))[2],",",fixed=T))[1]

	if(property=="cluster")
	{
		clustervector <- cluster.FLHKMeans(object)
		assign(parentObject,object,envir=parent.frame())
		return(clustervector)
	}
	else if(property=="centers")
	{
		centersmatrix <- centers.FLHKMeans(object)
		assign(parentObject,object,envir=parent.frame())
		return(centersmatrix)
	}
	else if(property=="tot.withinss")
	{
		tot_withinssvector <- tot.withinss.FLHKMeans(object)
		assign(parentObject,object,envir=parent.frame())
		return(tot_withinssvector)
	}
	else if(property=="betweenss")
	{
		betweenssvector <- betweenss.FLHKMeans(object)
		assign(parentObject,object,envir=parent.frame())
		return(betweenssvector)
	}
	else if(property=="totss")
	{
		totssvector <- totss.FLHKMeans(object)
		assign(parentObject,object,envir=parent.frame())
		return(totssvector)
	}
	else if(property=="withinss")
	{
		withinssvector <- withinss.FLHKMeans(object)
		assign(parentObject,object,envir=parent.frame())
		return(withinssvector)
	}
	else if(property=="size")
	{
		sizevector <- size.FLHKMeans(object)
		assign(parentObject,object,envir=parent.frame())
		return(sizevector)
	}
	else if(property=="mapping")
	{
		mapdataframe <- FLMapping.FLKMedoids(object)
		assign(parentObject,object,envir=parent.frame())
		return(mapdataframe)
	}
	else stop(property," is not a valid property")
}

## move to file FLHKMeans.R
cluster.FLHKMeans<-function(object)
{
	if(!is.null(object@results[["cluster"]]))
	return(object@results[["cluster"]])
	else
	{
		connection <- getFLConnection(object@table)
		## flag3Check(connection)

		tblfunqueryobj <- new("FLTableFunctionQuery",
                        connectionName = attr(connection,"name"),
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=constructSelectResult(object,result="clusters"))

		clustervector <- newFLVector(
							select = tblfunqueryobj,
							Dimnames = list(object@deeptable@Dimnames[[1]],
											"vectorValueColumn"),
							isDeep = FALSE)

		clustervector <- tryCatch(as.vector(clustervector),
      						error=function(e){clustervector})

		object@results <- c(object@results,list(cluster = clustervector))
		# parentObj <- deparse(substitute(object))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
                                "(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(clustervector)
	}
}

## move to file FLHKMeans.R
centers.FLHKMeans<-function(object)
{
	if(!is.null(object@results[["centers"]]))
	return(object@results[["centers"]])
	else
	{
		connection <- getFLConnection(object@table)
		## flag1Check(connection)
		# AnalysisID <- object@AnalysisID
		# sqlstr<-paste0("SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
		# 				    "  DENSE_RANK()OVER(ORDER BY ClusterID) AS rowIdColumn, \n ",
		# 				    "  VarID AS colIdColumn, \n ",
		# 				    "   Centroid AS valueColumn \n ",
		# 				" FROM fzzlKMeansDendrogram \n ",
		# 				" WHERE AnalysisID = '",AnalysisID,"' \n ",
		# 				" AND HypothesisID = ",object@nstart," \n ",
		# 				" AND Level = ",object@levels)

        ## Get column names from Mapping
        vColnames <- colnames(object@deeptable)
        ## gk: move this into colnames function.
        ## gk: create a test case for colnames of a deeptable
        if(object@mapTable!="")
        vColnames <- sqlQuery(connection,
                            paste0("SELECT ColumnName \n ",
                                " FROM ",object@mapTable," \n ",
                                " ORDER BY varID "))[[1]]

		tblfunqueryobj <- new("FLTableFunctionQuery",
                            connectionName = attr(connection,"name"),
                            variables=list(
                                rowIdColumn="rowIdColumn",
                                colIdColumn="colIdColumn",
                                valueColumn="valueColumn"),
                            whereconditions="",
                            order = "",
                            SQLquery=constructSelectResult(object,result="centers"))

	  	centersmatrix <- newFLMatrix(
                    select= tblfunqueryobj,
                    dims=as.integer(c(object@centers,
                                      length(vColnames))),
                    Dimnames=list(1:object@centers,
                                  vColnames))

	  	suppressWarnings(centersmatrix <- tryCatch(as.matrix(centersmatrix),
      						error=function(e){centersmatrix}))
		object@results <- c(object@results,list(centers = centersmatrix))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
                                "(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(centersmatrix)
	}
}

## move to file FLHKMeans.R
tot.withinss.FLHKMeans<-function(object){
	if(!is.null(object@results[["tot.withinss"]]))
	return(object@results[["tot.withinss"]])
	else
	{
		# connection <- getFLConnection(object@table)
		# deeptablename <- getTableNameSlot(object@deeptable)
		# obs_id_colname <- getVariables(object@deeptable)[["obs_id_colname"]]
		# var_id_colname <- getVariables(object@deeptable)[["var_id_colname"]]
		# cell_val_colname <- getVariables(object@deeptable)[["cell_val_colname"]]
		# whereconditions <- object@deeptable@select@whereconditions

		# sqlstr<-paste0("SELECT CAST(sum(power((",deeptablename,".",
		# 				cell_val_colname," - fzzlKMeansDendrogram.Centroid ),2)) AS NUMBER) \n ",
		# 				" FROM fzzlKMeansClusterID, \n ",deeptablename,", \n fzzlKMeansDendrogram \n ",
		# 				" WHERE fzzlKMeansDendrogram.AnalysisID = '",object@AnalysisID,"' \n ",
		# 				" AND fzzlKMeansClusterID.AnalysisID = '",object@AnalysisID,"' \n ", 
		# 				" AND ",deeptablename,".",var_id_colname,"=fzzlKMeansDendrogram.VarID \n ",
		# 				" AND fzzlKMeansClusterID.ClusterID = fzzlKMeansDendrogram.ClusterID \n ", 
		# 				" AND fzzlKMeansClusterID.ObsID = ",deeptablename,".",obs_id_colname," \n ",
		# 				" AND fzzlKMeansClusterID.HypothesisID = ",object@nstart," \n ",
		# 				" AND fzzlKMeansDendrogram.HypothesisID = ",object@nstart," \n ",
		# 				ifelse(length(whereconditions)>0, paste0(" AND ",whereconditions,collapse=" \n "),"")
		# 				)

		# tot_withinssvector <- sqlQuery(connection,sqlstr)[1,1]

        vWithinssVector <- object$withinss
		tot_withinssvector <- as.vector(sum(vWithinssVector))
		object@results <- c(object@results,list(tot.withinss = tot_withinssvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
                            "(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(tot_withinssvector)
	}
}


## move to file FLHKMeans.R
withinss.FLHKMeans<-function(object){
	if(!is.null(object@results[["withinss"]]))
	return(object@results[["withinss"]])
	else
	{
		connection <- getFLConnection(object@table)
		## flag3Check(connection)

		tblfunqueryobj <- new("FLTableFunctionQuery",
                        connectionName = attr(connection,"name"),
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=constructSelectResult(object,
                                                            result="withinss"))

		withinssvector <- newFLVector(
							select = tblfunqueryobj,
							Dimnames = list(1:object@centers,
											"vectorValueColumn"),
							isDeep = FALSE)

		withinssvector <- tryCatch(as.vector(withinssvector),
      						error=function(e){withinssvector})
		object@results <- c(object@results,list(withinss = withinssvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
						"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(withinssvector)
	}
	
}

## move to file FLHKMeans.R
betweenss.FLHKMeans<-function(object){
	if(!is.null(object@results[["betweenss"]]))
	return(object@results[["betweenss"]])
	else
	{
		# connection <- getFLConnection(object@table)
		# flag3Check(connection)
		# deeptablename <- getTableNameSlot(object@deeptable)
		# obs_id_colname <- getVariables(object@deeptable)[["obs_id_colname"]]
		# var_id_colname <- getVariables(object@deeptable)[["var_id_colname"]]
		# cell_val_colname <- getVariables(object@deeptable)[["cell_val_colname"]]
		# whereconditions <- object@deeptable@select@whereconditions

		# sqlstr<-paste0("SELECT CAST(sum(power((a.valavg - fzzlKMeansDendrogram.Centroid),2)) AS NUMBER) \n ",
		# 				" FROM (SELECT ",var_id_colname,",average(",cell_val_colname,") AS valavg \n ",
		# 					 " FROM ",deeptablename," \n ",
		# 					 " GROUP BY ",var_id_colname,") AS a, \n ",
		# 					 "fzzlKMeansClusterID, \n ",
		# 					 "fzzlKMeansDendrogram \n ",
		# 				" WHERE fzzlKMeansDendrogram.AnalysisID = '",object@AnalysisID,"' \n ",
		# 				" AND fzzlKMeansClusterID.AnalysisID = '",object@AnalysisID,"' \n ", 
		# 				" AND ",deeptablename,".",var_id_colname,"=fzzlKMeansDendrogram.VarID \n ", 
		# 				" AND fzzlKMeansClusterID.ClusterID = fzzlKMeansDendrogram.ClusterID \n ", 
		# 				" AND fzzlKMeansClusterID.ObsID = ",deeptablename,".",obs_id_colname," \n ",  
		# 				" AND a.",var_id_colname," = ",deeptablename,".",var_id_colname," \n ",
		# 				" AND fzzlKMeansClusterID.HypothesisID = ",object@nstart," \n ",
		# 				" AND fzzlKMeansDendrogram.HypothesisID = ",object@nstart," \n ",
		# 				ifelse(length(whereconditions)>0, paste0(" AND ",whereconditions,collapse=" \n "),"")
		# 				)

		# betweenssvector <- sqlQuery(connection,sqlstr)[1,1]

        betweenssvector <- object$totss-object$tot.withinss
		betweenssvector <- as.vector(betweenssvector)
		object@results <- c(object@results,list(betweenss = betweenssvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
						"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(betweenssvector)
	}
}

## move to file FLHKMeans.R
totss.FLHKMeans<-function(object){
	if(!is.null(object@results[["totss"]]))
	return(object@results[["totss"]])
	else
	{
		connection <- getFLConnection(object@table)

		sqlstr <- constructSelectResult(object,
                                            result="totss")
		
		totssvector <- sqlQuery(connection,sqlstr)[1,1]

		totssvector <- as.vector(totssvector)
		object@results <- c(object@results,list(totss = totssvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
							"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(totssvector)
	}
}

## move to file FLHKMeans.R
size.FLHKMeans<-function(object)
{
	if(!is.null(object@results[["size"]]))
	return(object@results[["size"]])
	else
	{
		connection <- getFLConnection(object@table)
		## flag3Check(connection)
		# sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
		# 				    "     DENSE_RANK()OVER(ORDER BY ClusterID) AS vectorIndexColumn, \n ",
		# 					"	 COUNT(ObsID) AS vectorValueColumn \n ",
		# 				"  FROM  fzzlKMeansClusterID \n ",
		# 				" WHERE AnalysisID = '",object@AnalysisID,"' \n ",   
	 #                      " AND HypothesisID = ",object@nstart," \n ",
	 #                      " GROUP BY ClusterID")

		tblfunqueryobj <- new("FLTableFunctionQuery",
                        connectionName = attr(connection,"name"),
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=constructSelectResult(object,
                                                        result="size"))

		sizevector <- newFLVector(
					select = tblfunqueryobj,
					Dimnames = list(1:object@centers,
									"vectorValueColumn"),
					isDeep = FALSE)

		sizevector <- tryCatch(as.vector(sizevector),
      						error=function(e){sizevector})
		object@results <- c(object@results,list(size = sizevector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
                            "(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(sizevector)
	}
}



# Prints the KMeans values

## move to file FLHKMeans.R
#' @export
print.FLHKMeans<-function(object)
{
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
                            "(",fixed=T))[2],")",fixed=T))[1]
	clustervector<-cluster.FLHKMeans(object)
	centermatrix<-centers.FLHKMeans(object)
	temp1 <- size.FLHKMeans(object)
	temp2 <- as.character(paste0(temp1,collapse=","))
	temp3 <- as.character(object@centers)

	cat(paste0("K-Means clustering with ",temp3," clusters of sizes ",temp2," 
				\n\nCluster Means:\n"))
	print(centermatrix)
	cat("\nClustering vector:\n")
	print(clustervector)
	cat("\nWithin cluster sum of squares by cluster\n")
	print(withinss.FLHKMeans(object))
	cat("(between_SS / total_SS = ")
	vtemp <- (100*betweenss.FLHKMeans(object))/totss.FLHKMeans(object)
	cat(paste0(as.character(vtemp)," %)\n"))
	cat("\nAvailable components\n")
	print(c("cluster","centers","totss","withinss","tot.withinss","betweenss","size"))

	assign(parentObject,object,envir=parent.frame())
}

## move to file FLHKMeans.R
#' @export
setMethod("show","FLHKMeans",
			function(object)
			{
				parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
                                    "(",fixed=T))[2],")",fixed=T))[1]
				print(object)
				assign(parentObject,object,envir=parent.frame())
			}
		 )

## move to file FLHKMeans.R
#' @export
plot.FLHKMeans <- function(object,...)
{
	deeptablename <- getTableNameSlot(object@deeptable)
	obs_id_colname <- getIndexSQLExpression(object@deeptable,1)
    var_id_colname <- getIndexSQLExpression(object@deeptable,2)
    cell_val_colname <- getIndexSQLExpression(object@deeptable,3)
	widetable <- gen_wide_table_name("new")

    ## If data is already available in R session, use it
    if("useData" %in% names(list(...)))
        x <- as.data.frame(list(...)$"useData")
    else{
        x <- object@deeptable
        x <- as.data.frame(x)
        x$obs_id_colname <- NULL
    }
	plot(x,col=as.vector(object$cluster),main="kmeans using DB-Lytix")
	points(as.matrix(object$centers),col=1:object@centers,pch=8,cex=2)
}

## move to file FLHKMeans.R
#' @export
fitted.FLHKMeans <- function(object,method="centers",...){
	AnalysisID <- object@AnalysisID
	
	if(!method %in% c("centers","classes"))
	stop(" method in fitted for kmeans should be c(centers,classes) \n ")
	if(method == "classes")
	return(object$cluster)

	# sqlstr<-paste0("SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
	# 				    " b.ObsID AS rowIdColumn, \n ",
	# 				    " a.VarID AS colIdColumn, \n ",
	# 				    " a.Centroid AS valueColumn \n ",
	# 				" FROM fzzlKMeansDendrogram a, \n ",
	# 						"fzzlkmeansclusterid b \n ",
	# 				" WHERE a.AnalysisID = '",AnalysisID,"' \n ",
	# 				" AND a.HypothesisID = ",object@nstart," \n ",
	# 				" AND a.Level = ",object@levels," \n ",
	# 				" AND a.HypothesisID = b.HypothesisID \n ",
	# 				" AND a.AnalysisID = b.AnalysisID \n ",
	# 				" AND a.ClusterID = b.ClusterID ")

	tblfunqueryobj <- new("FLTableFunctionQuery",
                    connectionName = attr(connection,"name"),
                    variables=list(
                        rowIdColumn="rowIdColumn",
                        colIdColumn="colIdColumn",
                        valueColumn="valueColumn"),
                    whereconditions="",
                    order = "",
                    SQLquery=constructSelectResult(object,
                                                    result="fitted"))

  	centersmatrix <- newFLMatrix(
			            select= tblfunqueryobj,
			            dims=as.integer(c(nrow(object@deeptable)),
			            	ncol(object@deeptable)),
			            Dimnames=list(object$cluster,
			            			object@deeptable@Dimnames[[2]]))
  	return(centersmatrix)
}

## move to file hkmeans.R
#' @export
hkmeans.FLMatrix <- function(x,
						centers,
						levels = 1,
						iter.max =10,
						nstart = 1,
						excludeCols = as.character(c()),
						classSpec = list(),
						whereconditions = ""
						)
{
	x <- as.FLTable(x)
	return(hkmeans(x=x,
				centers=centers,
				levels=levels,
				iter.max =iter.max,
				nstart = nstart,
				excludeCols = excludeCols,
				classSpec = classSpec,
				whereconditions = whereconditions))
}
