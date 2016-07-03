#' @include FLMatrix.R
NULL


#' An S4 class to represent FLHKMeans
setClass(
	"FLHKMeans",
	slots=list(
		centers="numeric",
		AnalysisID="character",
		wideToDeepAnalysisId="character",
		table="FLTable",
		results ="list",
		deeptable="FLTable",
		nstart="numeric",
		mapTable="character",
		levels="numeric"
	)
)
#' @export
hkmeans <- function (x, ...) {
  UseMethod("hkmeans", x)
}


#' Hierarchial K-Means Clustering.
#'
#' \code{hkmeans} performs Hierarchial k-means clustering on FLTable objects.
#'
#' @method hkmeans FLTable
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
#' connection <- flConnect(odbcSource="Gandalf")
#' widetable  <- FLTable( "FL_DEMO", "tblAbaloneWide", "ObsID")
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
hkmeans.FLTable<-function(x,
						centers,
						levels = 1,
						iter.max =10,
						nstart = 1,
						excludeCols = as.character(c()),
						classSpec = list(),
						whereconditions = ""
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

    connection <- getConnection(x)
    wideToDeepAnalysisId <- ""
    mapTable <- ""
	
	if(!x@isDeep){
		deepx <- wideToDeep(x,excludeCols=excludeCols,
							classSpec=classSpec,
							whereconditions=whereconditions)

		wideToDeepAnalysisId <- deepx[["AnalysisID"]]
		deepx <- deepx[["table"]]
		deepx <- setAlias(deepx,"")
		whereconditions <- ""
		mapTable <- getRemoteTableName(getOption("ResultDatabaseFL"),
					gen_wide_table_name("map"))

		sqlstr <- paste0(" CREATE TABLE ",mapTable," AS ( \n ",
			    	    " SELECT a.Final_VarID AS VarID, \n ",
			    	     	" a.COLUMN_NAME AS ColumnName, \n ",
			    	     	"  a.FROM_TABLE AS MapName \n ",
			    	    " FROM fzzlRegrDataPrepMap a \n ",
			    	    " WHERE a.AnalysisID = '",wideToDeepAnalysisId,"' \n ",
			    	    " AND a.Final_VarID IS NOT NULL) WITH DATA ")
		
		sqlSendUpdate(connection,sqlstr)
	}
	else if(class(x@select)=="FLTableFunctionQuery")
	{
		deeptablename <- gen_view_name(x@select@table_name)
		sqlstr <- paste0("CREATE VIEW ",getOption("ResultDatabaseFL"),
						".",deeptablename," AS \n ",
						constructSelect(x))
		sqlSendUpdate(connection,sqlstr)
		deepx <- FLTable(
                   getOption("ResultDatabaseFL"),
                   deeptablename,
                   "obs_id_colname",
                   "var_id_colname",
                   "cell_val_colname"
                  )
		deepx <- setAlias(deepx,"")
		deepx@select@whereconditions <- whereconditions[whereconditions!=""]
	}
	else
	{
		deepx <- x
		deepx <- setAlias(deepx,"")
		whereconditions <- c(deepx@select@whereconditions,whereconditions)
		deepx@select@whereconditions <- whereconditions[whereconditions!=""]
	}

	whereconditions <- whereconditions[whereconditions!=""]
	whereClause <- constructWhere(whereconditions)
	deeptable <- paste0(deepx@select@database,".",deepx@select@table_name)
	if(whereClause!="") whereClause <- paste0("'",whereClause,"'")
	else whereClause <- "NULL"

    sqlstr <- paste0("CALL FLHKMeans( '",deeptable,"',
			 					   '",getVariables(deepx)[["obs_id_colname"]],"',
			 					   '",getVariables(deepx)[["var_id_colname"]],"',
			 					   '",getVariables(deepx)[["cell_val_colname"]],"',",
			 					   whereClause,",",
			 					   levels,",",
			 					   centers,",",
			 					   iter.max,",",
			 					   nstart,",",
			 					   fquote(genNote("hkmeans")),
			 					   ",AnalysisID );")
	
	retobj <- sqlQuery(connection,sqlstr,AnalysisIDQuery=
						genAnalysisIDQuery("fzzlKMeansInfo",genNote("hkmeans")))
	AnalysisID <- as.character(retobj[1,1])
	sqlstr<-paste0("SELECT DISTINCT ObsID AS vectorIndexColumn \n ",
					"	FROM fzzlKMeansClusterID \n ",
					"	WHERE AnalysisID = '",AnalysisID,"' AND \n ",
					"	HypothesisID = ",nstart,
						" ORDER BY ObsID")

	rows <- sqlQuery(connection,sqlstr)[["vectorIndexColumn"]]

	sqlstr<-paste0("SELECT DISTINCT VarID AS vectorIndexColumn \n ",
					"	FROM fzzlKMeansDendrogram \n ",
					"	WHERE AnalysisID = '",AnalysisID,"' AND \n ",
					"	HypothesisID = ",nstart,
						" ORDER BY VarID")

	cols <- sqlQuery(connection,sqlstr)[["vectorIndexColumn"]]

	deepx@dimnames <- list(rows,cols)

	if(levels>1)
	{
		sqlstr <- paste0(" SELECT COUNT(DISTINCT ClusterID) FROM fzzlKMeansClusterID \n ",
						" WHERE AnalysisID='",AnalysisID,"' \n ",
						" AND HypothesisID = ",nstart)

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

#' @export
`$.FLHKMeans`<-function(object,property)
{
	#parentObject <- deparse(substitute(object))
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

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


cluster.FLHKMeans<-function(object)
{
	if(!is.null(object@results[["cluster"]]))
	return(object@results[["cluster"]])
	else
	{
		connection <- getConnection(object@table)
		flag3Check(connection)
		AnalysisID <- object@AnalysisID
		sqlstr<-paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
						    "     ObsID AS vectorIndexColumn, \n ",
						    "    DENSE_RANK()OVER(ORDER BY ClusterID) AS vectorValueColumn \n ",
						" FROM fzzlKMeansClusterID \n ",
						" WHERE AnalysisID = '",AnalysisID,"' \n AND ",
						" HypothesisID = ",object@nstart," \n ",
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


centers.FLHKMeans<-function(object)
{
	if(!is.null(object@results[["centers"]]))
	return(object@results[["centers"]])
	else
	{
		connection <- getConnection(object@table)
		flag1Check(connection)
		AnalysisID <- object@AnalysisID
		sqlstr<-paste0("SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
						    "  DENSE_RANK()OVER(ORDER BY ClusterID) AS rowIdColumn, \n ",
						    "  VarID AS colIdColumn, \n ",
						    "   Centroid AS valueColumn \n ",
						" FROM fzzlKMeansDendrogram \n ",
						" WHERE AnalysisID = '",AnalysisID,"' \n ",
						" AND HypothesisID = ",object@nstart," \n ",
						" AND Level = ",object@levels)

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


tot.withinss.FLHKMeans<-function(object){
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
		whereconditions <- object@deeptable@select@whereconditions

		sqlstr<-paste0("SELECT CAST(sum(power((",deeptablename,".",
						cell_val_colname," - fzzlKMeansDendrogram.Centroid ),2)) AS NUMBER) \n ",
						" FROM fzzlKMeansClusterID, \n ",deeptablename,", \n fzzlKMeansDendrogram \n ",
						" WHERE fzzlKMeansDendrogram.AnalysisID = '",object@AnalysisID,"' \n ",
						" AND fzzlKMeansClusterID.AnalysisID = '",object@AnalysisID,"' \n ", 
						" AND ",deeptablename,".",var_id_colname,"=fzzlKMeansDendrogram.VarID \n ",
						" AND fzzlKMeansClusterID.ClusterID = fzzlKMeansDendrogram.ClusterID \n ", 
						" AND fzzlKMeansClusterID.ObsID = ",deeptablename,".",obs_id_colname," \n ",
						" AND fzzlKMeansClusterID.HypothesisID = ",object@nstart," \n ",
						" AND fzzlKMeansDendrogram.HypothesisID = ",object@nstart," \n ",
						ifelse(length(whereconditions)>0, paste0(" AND ",whereconditions,collapse=" \n "),"")
						)

		tot_withinssvector <- sqlQuery(connection,sqlstr)[1,1]

		tot_withinssvector <- as.vector(tot_withinssvector)
		object@results <- c(object@results,list(tot.withinss = tot_withinssvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(tot_withinssvector)
	}
}


withinss.FLHKMeans<-function(object){
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
		whereconditions <- object@deeptable@select@whereconditions

		sqlstr<-paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
								" DENSE_RANK()OVER(ORDER BY fzzlKMeansClusterID.ClusterID) AS vectorIndexColumn, \n ",
								" CAST(sum(power((",deeptablename,".",cell_val_colname,
									" - fzzlKMeansDendrogram.Centroid ),2)) AS NUMBER) AS vectorValueColumn \n ",
						" FROM fzzlKMeansClusterID, \n ",deeptablename,", \n fzzlKMeansDendrogram \n ",
						" WHERE fzzlKMeansDendrogram.AnalysisID = '",object@AnalysisID,"' \n ",
						" AND fzzlKMeansClusterID.AnalysisID = '",object@AnalysisID,"' \n ",
						" AND ",deeptablename,".",var_id_colname,"=fzzlKMeansDendrogram.VarID \n ",
						" AND fzzlKMeansClusterID.ClusterID = fzzlKMeansDendrogram.ClusterID \n ",
						" AND fzzlKMeansClusterID.ObsID = ",deeptablename,".",obs_id_colname," \n ",
						" AND fzzlKMeansClusterID.HypothesisID = ",object@nstart," \n ",
						" AND fzzlKMeansDendrogram.HypothesisID = ",object@nstart," \n ",
						ifelse(length(whereconditions)>0, paste0(" AND ",whereconditions,collapse=" \n "),""), 
						" GROUP BY fzzlKMeansClusterID.ClusterID ")

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
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
						"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(withinssvector)
	}
	
}


betweenss.FLHKMeans<-function(object){
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
		whereconditions <- object@deeptable@select@whereconditions

		sqlstr<-paste0("SELECT CAST(sum(power((a.valavg - fzzlKMeansDendrogram.Centroid),2)) AS NUMBER) \n ",
						" FROM (SELECT ",var_id_colname,",average(",cell_val_colname,") AS valavg \n ",
							 " FROM ",deeptablename," \n ",
							 " GROUP BY ",var_id_colname,") AS a, \n ",
							 "fzzlKMeansClusterID, \n ",
							 "fzzlKMeansDendrogram \n ",
						" WHERE fzzlKMeansDendrogram.AnalysisID = '",object@AnalysisID,"' \n ",
						" AND fzzlKMeansClusterID.AnalysisID = '",object@AnalysisID,"' \n ", 
						" AND ",deeptablename,".",var_id_colname,"=fzzlKMeansDendrogram.VarID \n ", 
						" AND fzzlKMeansClusterID.ClusterID = fzzlKMeansDendrogram.ClusterID \n ", 
						" AND fzzlKMeansClusterID.ObsID = ",deeptablename,".",obs_id_colname," \n ",  
						" AND a.",var_id_colname," = ",deeptablename,".",var_id_colname," \n ",
						" AND fzzlKMeansClusterID.HypothesisID = ",object@nstart," \n ",
						" AND fzzlKMeansDendrogram.HypothesisID = ",object@nstart," \n ",
						ifelse(length(whereconditions)>0, paste0(" AND ",whereconditions,collapse=" \n "),"")
						)

		betweenssvector <- sqlQuery(connection,sqlstr)[1,1]

		betweenssvector <- as.vector(betweenssvector)
		object@results <- c(object@results,list(betweenss = betweenssvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
						"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(betweenssvector)
	}
}


totss.FLHKMeans<-function(object){
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
		whereconditions <- object@deeptable@select@whereconditions

		sqlstr<-paste0("SELECT CAST(sum(power((",deeptablename,".",
						cell_val_colname," - a.valavg),2)) AS NUMBER) \n ",
						" FROM (SELECT ",var_id_colname,",average(",cell_val_colname,") AS valavg \n ",
							"  FROM ",deeptablename,
							  constructWhere(whereconditions)," \n ",
							  " GROUP BY ",var_id_colname,") AS a, \n ",
							 deeptablename," \n ",
						" WHERE a.",var_id_colname," = ",deeptablename,".",var_id_colname,
						ifelse(length(whereconditions)>0, paste0(" AND ",whereconditions,collapse=" \n "),"")
						)
		
		totssvector <- sqlQuery(connection,sqlstr)[1,1]

		totssvector <- as.vector(totssvector)
		object@results <- c(object@results,list(totss = totssvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
							"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(totssvector)
	}
}


size.FLHKMeans<-function(object)
{
	if(!is.null(object@results[["size"]]))
	return(object@results[["size"]])
	else
	{
		connection <- getConnection(object@table)
		flag3Check(connection)
		sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
						    "     DENSE_RANK()OVER(ORDER BY ClusterID) AS vectorIndexColumn, \n ",
							"	 COUNT(ObsID) AS vectorValueColumn \n ",
						"  FROM  fzzlKMeansClusterID \n ",
						" WHERE AnalysisID = '",object@AnalysisID,"' \n ",   
	                      " AND HypothesisID = ",object@nstart," \n ",
	                      " GROUP BY ClusterID")

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
print.FLHKMeans<-function(object)
{
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
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

#' @export
setMethod("show","FLHKMeans",
			function(object)
			{
				parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
				print(object)
				assign(parentObject,object,envir=parent.frame())
			}
		 )

#' @export
plot.FLHKMeans <- function(object)
{
	deeptablename <- paste0(object@deeptable@select@database,".",object@deeptable@select@table_name)
	obs_id_colname <- getVariables(object@deeptable)[["obs_id_colname"]]
	var_id_colname <- getVariables(object@deeptable)[["var_id_colname"]]
	cell_val_colname <- getVariables(object@deeptable)[["cell_val_colname"]]
	widetable <- gen_wide_table_name("new")
	x <- object@deeptable
	x <- as.data.frame(x)
	x$obs_id_colname <- NULL
	plot(x,col=as.vector(object$cluster),main="kmeans using DB-Lytix")
	points(as.matrix(object$centers),col=1:object@centers,pch=8,cex=2)
}

#' @export
fitted.FLHKMeans <- function(object,method="centers",...){
	AnalysisID <- object@AnalysisID
	
	if(!method %in% c("centers","classes"))
	stop(" method in fitted for kmeans should be c(centers,classes) \n ")
	if(method == "classes")
	return(object$cluster)

	sqlstr<-paste0("SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
					    " b.ObsID AS rowIdColumn, \n ",
					    " a.VarID AS colIdColumn, \n ",
					    " a.Centroid AS valueColumn \n ",
					" FROM fzzlKMeansDendrogram a, \n ",
							"fzzlkmeansclusterid b \n ",
					" WHERE a.AnalysisID = '",AnalysisID,"' \n ",
					" AND a.HypothesisID = ",object@nstart," \n ",
					" AND a.Level = ",object@levels," \n ",
					" AND a.HypothesisID = b.HypothesisID \n ",
					" AND a.AnalysisID = b.AnalysisID \n ",
					" AND a.ClusterID = b.ClusterID ")

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
			            dim=c(nrow(object@deeptable),
			            	ncol(object@deeptable)),
			            dimnames=list(object$cluster,
			            			object@deeptable@dimnames[[2]]))
  	return(centersmatrix)
}

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
