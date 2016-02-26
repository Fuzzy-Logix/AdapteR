#' @include utilities.R
#' @include FLTable.R
#' @include FLMatrix.R
#' @include FLVector.R
NULL
#' An S4 class to represent FLKMeans
#'
#' @slot centers A numeric vector containing the number of clusters, say k
#' @slot AnalysisID A character output used to retrieve the results of analysis
#' @slot connection ODBC connectivity for R
#' @slot table FLTable object given as input on which analysis is performed
#' @slot resultsfetched A logical vector describing what components are fetched
#' @slot results A list of all fetched components
#' @slot deeptablename A character vector containing a deeptable(either conversion from a widetable or input deeptable)
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
	"FLKMeans",
	slots=list(
		centers="numeric",
		AnalysisID="character",
		wideToDeepAnalysisId="character",
		table="FLTable",
		results ="list",
		deeptable="FLTable",
		nstart="numeric",
		mapTable="character"
	)
)
#' @export
kmeans <- function (x, ...) {
  UseMethod("kmeans", x)
}

#' @export
kmeans.data.frame<-stats::kmeans
#' @export
kmeans.matrix <- stats::kmeans
#' @export
kmeans.default <- stats::kmeans

#' K-Means Clustering.
#'
#' \code{kmeans} performs k-means clustering on FLTable objects.
#'
#' The wrapper overloads kmeans and implicitly calls FLKMeans.
#' @method kmeans FLTable
#' @param table an object of class FLTable
#' @param centers the number of clusters
#' @param iter.max the maximum number of iterations allowed
#' @param nstart the initial number of random sets
#' @param exclude the comma separated character string of columns to be excluded
#' @param class_spec list describing the categorical dummy variables
#' @param where_clause takes the where_clause as a string 
#' @section Constraints:
#' None
#' @return \code{kmeans} performs k-means clustering and replicates equivalent R output.
#' @examples
#' connection <- RODBC::odbcConnect("Gandalf")
#' widetable  <- FLTable(connection, "FL_TRAIN", "tblAbaloneWide", "ObsID")
#' kmeansobject <- kmeans(widetable,3,20,2,"Rings,SEX",list("DummyCat(D)","SEX(M)"))
#' print(kmeansobject)
#' @export
kmeans.FLTable<-function(x,
						centers,
						iter.max =10,
						nstart = 1,
						excludeCols = as.character(c()),
						classSpec = list(),
						whereconditions = ""
						)
{

	#Type validation
	if(any(!(c(centers,iter.max,nstart) >= 1)))
    stop("centers,iter.max,nstart should be atleast 1")
    else
    {
    	centers  <- as.integer(max(centers))
    	iter.max <- as.integer(max(iter.max))
    	nstart <- as.integer(max(nstart))
    }

	argList  <- as.list(environment())

	typeList <- list(	centers      = "integer",
						iter.max     = "integer",
						nstart       = "integer",
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

		sqlstr <- paste0(" CREATE TABLE ",mapTable," AS ( 
			    	     SELECT a.Final_VarID AS VarID,
			    	     	    a.COLUMN_NAME AS ColumnName,
			    	     	    a.FROM_TABLE AS MapName 
			    	     FROM fzzlRegrDataPrepMap a 
			    	     WHERE a.AnalysisID = '",wideToDeepAnalysisId,"' 
			    	     AND a.Final_VarID IS NOT NULL) WITH DATA")
		
		sqlSendUpdate(connection,sqlstr)
	}
	else if(class(x@select)=="FLTableFunctionQuery")
	{
		deeptablename <- gen_deep_table_name(x@select@table_name)
		sqlstr <- paste0("CREATE VIEW ",getOption("ResultDatabaseFL"),".",deeptablename," AS ",constructSelect(x))
		sqlSendUpdate(connection,sqlstr)
		deepx <- FLTable(connection,
                   getOption("ResultDatabaseFL"),
                   deeptablename,
                   "obs_id_colname",
                   "var_id_colname",
                   "cell_val_colname"
                  )
		whereconditions <- whereconditions
	}
	else
	{
		deepx <- x
		whereconditions <- c(x@select@whereconditions,whereconditions)
	}

	whereconditions <- whereconditions[whereconditions!=""]
	whereClause <- constructWhere(whereconditions)
	deeptable <- paste0(deepx@select@database,".",deepx@select@table_name)
	if(whereClause!="") whereClause <- paste0("'",whereClause,"'")
	else whereClause <- "NULL"

    sqlstr <- paste("CALL FLKMeans( '",deeptable,"',
			 					   '",getVariables(deepx)[["obs_id_colname"]],"',
			 					   '",getVariables(deepx)[["var_id_colname"]],"',
			 					   '",getVariables(deepx)[["cell_val_colname"]],"',",
			 					   whereClause,",",
			 					   centers,",",
			 					   iter.max,",",
			 					   nstart,",
			 					   'KMeans with clusters=",centers,"from AdapteR',
			 					   AnalysisID );")
	
	retobj <- sqlQuery(connection,sqlstr)
	AnalysisID <- as.character(retobj[1,1])
	sqlstr<-paste0("SELECT DISTINCT ObsID AS vectorIndexColumn 
						FROM fzzlKMeansClusterID 
						WHERE AnalysisID = '",AnalysisID,"' AND
						HypothesisID = ",nstart,
						" ORDER BY ObsID")

	rows <- sqlQuery(connection,sqlstr)[["vectorIndexColumn"]]

	sqlstr<-paste0("SELECT DISTINCT VarID AS vectorIndexColumn 
						FROM fzzlKMeansDendrogram 
						WHERE AnalysisID = '",AnalysisID,"' AND
						HypothesisID = ",nstart,
						" ORDER BY VarID")

	cols <- sqlQuery(connection,sqlstr)[["vectorIndexColumn"]]

	deepx@dimnames <- list(rows,cols)

	new("FLKMeans",
		centers=centers,
		AnalysisID=AnalysisID,
		wideToDeepAnalysisId=wideToDeepAnalysisId,
		table=x,
		results=list(),
		deeptable=deepx,
		nstart = nstart,
		mapTable=mapTable
	)
}

#' @export
`$.FLKMeans`<-function(object,property)
{
	#parentObject <- deparse(substitute(object))
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

	if(property=="cluster")
	{
		clustervector <- cluster(object)
		assign(parentObject,object,envir=parent.frame())
		return(clustervector)
	}
	else if(property=="centers")
	{
		centersmatrix <- centers(object)
		assign(parentObject,object,envir=parent.frame())
		return(centersmatrix)
	}
	else if(property=="tot.withinss")
	{
		tot_withinssvector <- tot.withinss(object)
		assign(parentObject,object,envir=parent.frame())
		return(tot_withinssvector)
	}
	else if(property=="betweenss")
	{
		betweenssvector <- betweenss(object)
		assign(parentObject,object,envir=parent.frame())
		return(betweenssvector)
	}
	else if(property=="totss")
	{
		totssvector <- totss(object)
		assign(parentObject,object,envir=parent.frame())
		return(totssvector)
	}
	else if(property=="withinss")
	{
		withinssvector <- withinss(object)
		assign(parentObject,object,envir=parent.frame())
		return(withinssvector)
	}
	else if(property=="size")
	{
		sizevector <- size(object)
		assign(parentObject,object,envir=parent.frame())
		return(sizevector)
	}
	else stop(property," is not a valid property")
}

#' @export
cluster <- function (x, ...) {
   UseMethod("cluster", x)
 }

#' @export
cluster.FLKMeans<-function(object)
{
	if(!is.null(object@results[["cluster"]]))
	return(object@results[["cluster"]])
	else
	{
		connection <- getConnection(object@table)
		flag3Check(connection)
		AnalysisID <- object@AnalysisID
		sqlstr<-paste0("SELECT '%insertIDhere%' AS vectorIdColumn, 
						         ObsID AS vectorIndexColumn,
						         CAST(SUBSTRING(ClusterID FROM POSITION('-' IN ClusterID )+1 ) AS INTEGER) AS vectorValueColumn 
						FROM fzzlKMeansClusterID 
						WHERE AnalysisID = '",AnalysisID,"' AND
						HypothesisID = ",object@nstart,
						" ORDER BY ObsID")

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
							dimnames = list(object@deeptable@dimnames[[1]],
											"vectorValueColumn"),
							isDeep = FALSE)

		clustervector <- tryCatch(as.vector(clustervector),
      						error=function(e){clustervector})

		object@results <- c(object@results,list(cluster = clustervector))
		# parentObj <- deparse(substitute(object))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(clustervector)
	}
}

#' @export
centers <- function (x, ...) 
{
   UseMethod("centers", x)
}
#' @export
centers.FLKMeans<-function(object)
{
	if(!is.null(object@results[["centers"]]))
	return(object@results[["centers"]])
	else
	{
		connection <- getConnection(object@table)
		flag1Check(connection)
		AnalysisID <- object@AnalysisID
		sqlstr<-paste0("SELECT '%insertIDhere%' AS MATRIX_ID,
						       CAST(SUBSTRING(ClusterID FROM POSITION('-' IN ClusterID )+1 ) AS INTEGER) AS rowIdColumn,
						       VarID AS colIdColumn,
						       Centroid AS valueColumn 
						FROM fzzlKMeansDendrogram 
						WHERE AnalysisID = '",AnalysisID,"' 
						AND HypothesisID = ",object@nstart)

		tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = connection,
                        variables=list(
                            rowIdColumn="rowIdColumn",
                            colIdColumn="colIdColumn",
                            valueColumn="valueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

	  	centersmatrix <- new("FLMatrix",
				            select= tblfunqueryobj,
				            dim=c(object@centers,
				            	length(object@deeptable@dimnames[[2]])),
				            dimnames=list(1:object@centers,
				            			object@deeptable@dimnames[[2]]))

	  	centersmatrix <- tryCatch(as.matrix(centersmatrix),
      						error=function(e){centersmatrix})
		object@results <- c(object@results,list(centers = centersmatrix))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(centersmatrix)
	}
}

#' @export
tot.withinss<-function(object,...){
	UseMethod("tot.withinss",object)
}

#' @export
tot.withinss.FLKMeans<-function(object){
	if(!is.null(object@results[["tot.withinss"]]))
	return(object@results[["tot.withinss"]])
	else
	{
		connection <- getConnection(object@table)
		flag3Check(connection)
		deeptablename <- paste0(object@deeptable@select@database,".",object@deeptable@select@table_name)
		obs_id_colname <- getVariables(object@deeptable)[["obs_id_colname"]]
		var_id_colname <- getVariables(object@deeptable)[["var_id_colname"]]
		cell_val_colname <- getVariables(object@deeptable)[["cell_val_colname"]]
		sqlstr<-paste0("SELECT CAST(sum(power((",deeptablename,".",cell_val_colname," - fzzlKMeansDendrogram.Centroid ),2)) AS NUMBER)  
						FROM fzzlKMeansClusterID,",deeptablename,",fzzlKMeansDendrogram 
						WHERE fzzlKMeansDendrogram.AnalysisID = '",object@AnalysisID,"' 
						AND fzzlKMeansClusterID.AnalysisID = '",object@AnalysisID,"' 
						AND ",deeptablename,".",var_id_colname,"=fzzlKMeansDendrogram.VarID 
						AND fzzlKMeansClusterID.ClusterID = fzzlKMeansDendrogram.ClusterID 
						AND fzzlKMeansClusterID.ObsID = ",deeptablename,".",obs_id_colname)

		tot_withinssvector <- sqlQuery(connection,sqlstr)[1,1]

		tot_withinssvector <- as.vector(tot_withinssvector)
		object@results <- c(object@results,list(tot.withinss = tot_withinssvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(tot_withinssvector)
	}
}

#' @export
withinss<-function(object){
	UseMethod("withinss",object)
}

#' @export
withinss.FLKMeans<-function(object){
	if(!is.null(object@results[["withinss"]]))
	return(object@results[["withinss"]])
	else
	{
		connection <- getConnection(object@table)
		flag3Check(connection)
		deeptablename <- paste0(object@deeptable@select@database,".",object@deeptable@select@table_name)
		obs_id_colname <- getVariables(object@deeptable)[["obs_id_colname"]]
		var_id_colname <- getVariables(object@deeptable)[["var_id_colname"]]
		cell_val_colname <- getVariables(object@deeptable)[["cell_val_colname"]]
		sqlstr<-paste0("SELECT '%insertIDhere%' AS vectorIdColumn,
								CAST(SUBSTRING(fzzlKMeansClusterID.ClusterID FROM POSITION('-' IN fzzlKMeansClusterID.ClusterID )+1 ) AS INTEGER) AS vectorIndexColumn,
								CAST(sum(power((",deeptablename,".",cell_val_colname," - fzzlKMeansDendrogram.Centroid ),2)) AS NUMBER) AS vectorValueColumn
						FROM fzzlKMeansClusterID,",deeptablename,",fzzlKMeansDendrogram 
						WHERE fzzlKMeansDendrogram.AnalysisID = '",object@AnalysisID,"' 
						AND fzzlKMeansClusterID.AnalysisID = '",object@AnalysisID,"' 
						AND ",deeptablename,".",var_id_colname,"=fzzlKMeansDendrogram.VarID 
						AND fzzlKMeansClusterID.ClusterID = fzzlKMeansDendrogram.ClusterID 
						AND fzzlKMeansClusterID.ObsID = ",deeptablename,".",obs_id_colname,"  
						GROUP BY fzzlKMeansClusterID.ClusterID")

		tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = connection,
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

		withinssvector <- new("FLVector",
							select = tblfunqueryobj,
							dimnames = list(1:object@centers,
											"vectorValueColumn"),
							isDeep = FALSE)

		withinssvector <- tryCatch(as.vector(withinssvector),
      						error=function(e){withinssvector})
		object@results <- c(object@results,list(withinss = withinssvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(withinssvector)
	}
	
}

#' @export
betweenss<-function(object){
	UseMethod("betweenss",object)
}

#' @export
betweenss.FLKMeans<-function(object){
	if(!is.null(object@results[["betweenss"]]))
	return(object@results[["betweenss"]])
	else
	{
		connection <- getConnection(object@table)
		flag3Check(connection)
		deeptablename <- paste0(object@deeptable@select@database,".",object@deeptable@select@table_name)
		obs_id_colname <- getVariables(object@deeptable)[["obs_id_colname"]]
		var_id_colname <- getVariables(object@deeptable)[["var_id_colname"]]
		cell_val_colname <- getVariables(object@deeptable)[["cell_val_colname"]]

		sqlstr<-paste0("SELECT CAST(sum(power((a.valavg - fzzlKMeansDendrogram.Centroid),2)) AS NUMBER) 
						FROM (SELECT ",var_id_colname,",average(",cell_val_colname,") AS valavg 
							  FROM ",deeptablename," 
							  GROUP BY ",var_id_colname,") AS a, 
							 fzzlKMeansClusterID, 
							 fzzlKMeansDendrogram 
						WHERE fzzlKMeansDendrogram.AnalysisID = '",object@AnalysisID,"' 
						AND fzzlKMeansClusterID.AnalysisID = '",object@AnalysisID,"' 
						AND ",deeptablename,".",var_id_colname,"=fzzlKMeansDendrogram.VarID 
						AND fzzlKMeansClusterID.ClusterID = fzzlKMeansDendrogram.ClusterID 
						AND fzzlKMeansClusterID.ObsID = ",deeptablename,".",obs_id_colname,"  
						AND a.",var_id_colname," = ",deeptablename,".",var_id_colname)

		betweenssvector <- sqlQuery(connection,sqlstr)[1,1]

		betweenssvector <- as.vector(betweenssvector)
		object@results <- c(object@results,list(betweenss = betweenssvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(betweenssvector)
	}
}

#' @export
totss<-function(object){
	UseMethod("totss",object)
}

#' @export
totss.FLKMeans<-function(object){
	if(!is.null(object@results[["totss"]]))
	return(object@results[["totss"]])
	else
	{
		connection <- getConnection(object@table)
		flag3Check(connection)
		deeptablename <- paste0(object@deeptable@select@database,".",object@deeptable@select@table_name)
		obs_id_colname <- getVariables(object@deeptable)[["obs_id_colname"]]
		var_id_colname <- getVariables(object@deeptable)[["var_id_colname"]]
		cell_val_colname <- getVariables(object@deeptable)[["cell_val_colname"]]

		sqlstr<-paste0("SELECT CAST(sum(power((",deeptablename,".",cell_val_colname," - a.valavg),2)) AS NUMBER) 
						FROM (SELECT ",var_id_colname,",average(",cell_val_colname,") AS valavg 
							  FROM ",deeptablename," GROUP BY ",var_id_colname,") AS a, ",
							 deeptablename," 
						WHERE a.",var_id_colname," = ",deeptablename,".",var_id_colname)
		
		totssvector <- sqlQuery(connection,sqlstr)[1,1]

		totssvector <- as.vector(totssvector)
		object@results <- c(object@results,list(totss = totssvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(totssvector)
	}
}

#' @export
size<-function(object){
	UseMethod("size",object)
}

#' @export
size.FLKMeans<-function(object)
{
	if(!is.null(object@results[["size"]]))
	return(object@results[["size"]])
	else
	{
		connection <- getConnection(object@table)
		flag3Check(connection)
		sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,
						         CAST(SUBSTRING(clusterid FROM POSITION('-' IN clusterid )+1 ) AS INTEGER) AS vectorIndexColumn,
								 COUNT(ObsID) AS vectorValueColumn 
						  FROM  fzzlKMeansClusterID 
						  WHERE AnalysisID = '",object@AnalysisID,"'   
	                      AND HypothesisID = ",object@nstart," 
	                      GROUP BY ClusterID")

		tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = connection,
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

		sizevector <- new("FLVector",
					select = tblfunqueryobj,
					dimnames = list(1:object@centers,
									"vectorValueColumn"),
					isDeep = FALSE)

		sizevector <- tryCatch(as.vector(sizevector),
      						error=function(e){sizevector})
		object@results <- c(object@results,list(size = sizevector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(sizevector)
	}
}

# Prints the KMeans values
#' @export
print.FLKMeans<-function(object)
{
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	clustervector<-cluster(object)
	centermatrix<-centers(object)
	temp1 <- size(object)
	temp2 <- as.character(paste0(temp1,collapse=","))
	temp3 <- as.character(object@centers)

	cat(paste0("K-Means clustering with ",temp3," clusters of sizes ",temp2," 
				\n\nCluster Means:\n"))
	print(centermatrix)
	cat("\nClustering vector:\n")
	print(clustervector)
	cat("\nWithin cluster sum of squares by cluster\n")
	print(withinss(object))
	cat("(between_SS / total_SS = ")
	vtemp <- (100*betweenss(object))/totss(object)
	cat(paste0(as.character(vtemp)," %)\n"))
	cat("\nAvailable components\n")
	print(c("cluster","centers","totss","withinss","tot.withinss","betweenss","size"))

	assign(parentObject,object,envir=parent.frame())
}

#' @export
setMethod("show","FLKMeans",
			function(object)
			{
				parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
				print(object)
				assign(parentObject,object,envir=parent.frame())
			}
		 )

#' @export
plot.FLKMeans <- function(object)
{
	deeptablename <- paste0(object@deeptable@select@database,".",object@deeptable@select@table_name)
	obs_id_colname <- getVariables(object@deeptable)[["obs_id_colname"]]
	var_id_colname <- getVariables(object@deeptable)[["var_id_colname"]]
	cell_val_colname <- getVariables(object@deeptable)[["cell_val_colname"]]
	widetable <- gen_wide_table_name("new")
	#widetable <- "tempuniquewide12345678"
	if(!object@table@isDeep)
	{
		widex <- deepToWide(object@deeptable,
							whereconditions="",
							mapTable= object@mapTable,
							mapName = paste0(object@table@select@database,".",object@table@select@table_name),
							outWideTableDatabase=getOption("ResultDatabaseFL"),
                    		outWideTableName=widetable)
		x <- widex$table
	}
	else
	x <- object@deeptable
	x <- as.data.frame(x)
	x$obs_id_colname <- NULL
	#print(x[1:20,])
	plot(x,col=as.vector(object$cluster))
	points(as.matrix(object$centers),col=1:object@centers,pch=8,cex=2)
}
