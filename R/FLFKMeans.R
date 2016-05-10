#' @include utilities.R
#' @include FLTable.R
#' @include FLMatrix.R
#' @include FLVector.R
NULL
#' An S4 class to represent FLFKMeans
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
setClass(
	"FLFKMeans",
	slots=list(
		centers="numeric",
		AnalysisID="character",
		wideToDeepAnalysisId="character",
		table="FLTable",
		diss="logical",
		results ="list",
		deeptable="FLTable",
		temptables="list",
		mapTable="character",
		memb.exp="numeric",
		maxit="numeric"
	)
)

#' @export
fanny <- function (x,k,...) {
  UseMethod("fanny", x)
}
#' @export
fanny.data.frame<-cluster::fanny
#' @export
fanny.matrix <- cluster::fanny
#' @export
fanny.default <- cluster::fanny

#' FuzzyKMeans Clustering.
#'
#' \code{fanny} performs fuzzy analysis on FLTable objects.
#'
#' @method fanny FLTable
#' @param x an object of class FLTable, can be wide or deep table
#' @param k the number of clusters
#' @param diss logical if \code{x} is dissimilarity matrix.
#' currently not used.
#' @param memb.exp degree of fuzziness or membership coefficient
#' @param metric only "euclidean" distance supported currently
#' @param Stand logical indicating if standardization
#' should be done before calculating diss matrix
#' @param iniMem.p inital membership matrix. Currently not used
#' @param cluster.only logical if only clustering vector is 
#' needed as output
#' @param keep.diss logicals indicating if the 
#' dissimilarities and/or input data x should be kept in the result
#' @param keep.data logicals indicating if the 
#' dissimilarities and/or input data x should be kept in the result
#' @param maxit maximum number of iterations
#' @param tol tolerance used for convergence. Currently 0.000001
#' @param trace.lev integer specifying a trace level for 
#' printing diagnostics during the build and swap phase of the algorithm.
#' currently always 0
#' @param excludeCols the comma separated character string of columns to be excluded
#' @param classSpec list describing the categorical dummy variables
#' @param whereconditions takes the where_clause as a string
#' @param distTable name of the in-database table having dissimilarity
#' matrix or distance table
#' @section Constraints:
#' Plotting for large datasets takes longer time to fetch data.
#' If classSpec is not specified, the categorical variables are excluded
#' from analysis by default.
#' @return \code{fanny} returns a list and replicates equivalent R output
#' from \code{fanny} in cluster package.The mapping table can be viewed
#' using \code{object$mapping} if input is wide table.
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' widetable  <- FLTable("FL_DEMO", "iris", "rownames")
#' fkmeansobject <- fanny(widetable,2,memb.exp=2)
#' print(fkmeansobject)
#' plot(fkmeansobject)
#' One can specify ClassSpec and transform categorical variables 
#' before clustering. This increases the number of variables in the plot
#' because categorical variable is split into binary numerical variables.
#' The clusters may not be well-defined as is observed in the case below:-
#' widetable  <- FLTable( "FL_DEMO", "iris", "rownames")
#' fannyobjectnew <- fanny(widetable,3,classSpec=list("Species(setosa)"))
#' plot(fannyobjectnew)
#' @export
fanny.FLTable <- function(x,
						k,
						diss=FALSE,
						memb.exp= 2,
						metric="euclidean",##notUsed
						Stand=FALSE,##notUsed
						iniMem.p=NULL,##notUsed
						cluster.only = FALSE,
    					keep.diss = (!diss && !cluster.only),
    					keep.data = (!diss && !cluster.only),
    					maxit = 500,
    					tol = 0.000001,##From dbLytix
    					trace.lev = 0,##notUsed
						excludeCols = "",
						classSpec = list(),
						whereconditions = "",
						distTable=""
						)
{

	#Type validation
	if(any(!(c(k,maxit,memb.exp) >= 1)))
    stop("k,maxit,memb.exp should be atleast 1")
    else
    {
    	k  <- as.integer(max(k))
    	maxit <- as.integer(max(maxit))
    }

	argList  <- as.list(environment())

	typeList <- list(	k      = "integer",
						maxit     = "integer",
						memb.exp = "double",
						excludeCols  = "character",
						classSpec   = "list",
						whereconditions = "character",
						diss = "logical",
						metric = "character",
						Stand = "logical",
						cluster.only = "logical",
						keep.diss = "logical",
						keep.data = "logical",
						distTable = "character",
						tol = "double"
					)

	classList <- list(x = "FLTable")
	validate_args(argList, typeList, classList)

    connection <- getConnection(x)
    wideToDeepAnalysisId <- ""
    mapTable <- ""
	
	vcall <- match.call()
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
		deeptablename <- gen_view_name("")
		sqlstr <- paste0("CREATE VIEW ",getOption("ResultDatabaseFL"),".",deeptablename," AS ",constructSelect(x))
		sqlSendUpdate(connection,sqlstr)

		deeptablename1 <- gen_view_name("New")
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
		deeptablename <- gen_view_name("New")
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

	if(diss)
	{
		cat(" diss is not supported currently. Please input data table instead.")
		diss <- FALSE
	}

    sqlstr <- paste0("CALL FLFKMeans( '",deeptable,"',
			 					   '",getVariables(deepx)[["obs_id_colname"]],"',
			 					   '",getVariables(deepx)[["var_id_colname"]],"',
			 					   '",getVariables(deepx)[["cell_val_colname"]],"',",
			 					   whereClause,",",
			 					   k,",",
			 					   maxit,",",
			 					   memb.exp,",1,",
			 					   fquote(genNote("fkmeans")),
			 					   ",AnalysisID );")
	
	retobj <- sqlQuery(connection,sqlstr,AnalysisIDQuery=
						genAnalysisIDQuery("fzzlKMeansInfo",genNote("fkmeans")))
	retobj <- checkSqlQueryOutput(retobj)
	AnalysisID <- as.character(retobj[1,1])
	
	FLFKMeansobject <- new("FLFKMeans",
						centers=k,
						AnalysisID=AnalysisID,
						wideToDeepAnalysisId=wideToDeepAnalysisId,
						table=x,
						results=list(call=vcall),
						deeptable=deepx,
						diss=diss,
						temptables=list(),
						mapTable=mapTable,
						memb.exp=memb.exp,
						maxit=maxit)
	if(cluster.only)
	return(FLFKMeansobject$clustering)
	else return(FLFKMeansobject)
}

#' @export
`$.FLFKMeans`<-function(object,property)
{
	#parentObject <- deparse(substitute(object))
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

	if(property=="membership")
	{
		membershipmatrix <- membership.FLFKMeans(object)
		assign(parentObject,object,envir=parent.frame())
		return(membershipmatrix)
	}
	else if(property=="memb.exp")
	{
		memb.expvector <- object@memb.exp
		assign(parentObject,object,envir=parent.frame())
		return(memb.expvector)
	}
	else if(property=="coeff")
	{
		coeffvector <- coeff.FLFKMeans(object)
		assign(parentObject,object,envir=parent.frame())
		return(coeffvector)
	}
	else if(property=="clustering")
	{
		clusteringvector <- clustering.FLFKMeans(object)
		assign(parentObject,object,envir=parent.frame())
		return(clusteringvector)
	}
	else if(property=="k.crisp") ##What are crisp clusters?
	{
		k.crispvector <- object@centers
		assign(parentObject,object,envir=parent.frame())
		return(k.crispvector)
	}
	else if(property=="objective")
	{
		objectivevector <- objective.FLFKMeans(object)
		assign(parentObject,object,envir=parent.frame())
		return(objectivevector)
	}
	else if(property=="convergence")
	{
		convergencevector <- convergence.FLFKMeans(object)
		assign(parentObject,object,envir=parent.frame())
		return(convergencevector)
	}
	else if(property=="silinfo")
	{
		silinfolist <- silinfo.FLFKMeans(object)
		assign(parentObject,object,envir=parent.frame())
		return(silinfolist)
	}
	else if(property=="diss")
	{
		dissmatrix <- diss.FLKMedoids(object)
		assign(parentObject,object,envir=parent.frame())
		return(dissmatrix)
	}
	else if(property=="call")
	{
		callobject <- call.FLFKMeans(object)
		assign(parentObject,object,envir=parent.frame())
		return(callobject)
	}
	else if(property=="data")
	{
		dataframe <- data.FLKMedoids(object)
		assign(parentObject,object,envir=parent.frame())
		return(dataframe)
	}
	else if(property=="mapping")
	{
		mapdataframe <- FLMapping.FLKMedoids(object)
		assign(parentObject,object,envir=parent.frame())
		return(mapdataframe)
	}
	else stop(property," is not a valid property")
}


clustering.FLFKMeans <- function(object)
{
	if(!is.null(object@results[["clustering"]]))
	return(object@results[["clustering"]])
	else
	{
		connection <- getConnection(object@table)
		flag3Check(connection)
		AnalysisID <- object@AnalysisID
		sqlstr<-paste0("SELECT '%insertIDhere%' AS vectorIdColumn, 
						        ObsID AS vectorIndexColumn,
						        CAST(ClusterID AS INTEGER) AS vectorValueColumn 
						FROM fzzlKMeansClusterID 
						WHERE AnalysisID = '",AnalysisID,"' AND
						HypothesisID = 1 ")

		tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = connection,
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

		clusteringvector <- new("FLVector",
							select = tblfunqueryobj,
							dimnames = list(object@deeptable@dimnames[[1]],
											"vectorValueColumn"),
							isDeep = FALSE)
		clusteringvector <- tryCatch(as.vector(clusteringvector),
      						error=function(e){clusteringvector})

		object@results <- c(object@results,list(clustering = clusteringvector))
		# parentObj <- deparse(substitute(object))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(clusteringvector)
	}
}


membership.FLFKMeans<-function(object)
{
	if(!is.null(object@results[["membership"]]))
	return(object@results[["membership"]])
	else
	{
		connection <- getConnection(object@table)
		flag1Check(connection)
		AnalysisID <- object@AnalysisID

		sqlstr<-paste0("SELECT '%insertIDhere%' AS MATRIX_ID, 
					         a.obsID AS rowIdColumn,
					         a.ClusterID AS colIdColumn,
					         a.Weight AS valueColumn 
						FROM fzzlKMeansMembership AS a 
						WHERE a.AnalysisID = '",AnalysisID,"'",
						" AND a.HypothesisID=1")

		tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = connection,
                        variables=list(
                            rowIdColumn="rowIdColumn",
                            colIdColumn="colIdColumn",
                            valueColumn="valueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

	  	membershipmatrix <- new("FLMatrix",
				            select= tblfunqueryobj,
				            dim=c(length(object@deeptable@dimnames[[1]]),
				            	object@centers),
				            dimnames=list(
				            			object@deeptable@dimnames[[1]],
				            			1:object@centers))

		membershipmatrix <- tryCatch(as.matrix(membershipmatrix),
      						error=function(e){membershipmatrix})
		
		object@results <- c(object@results,list(membership = membershipmatrix))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(membershipmatrix)
	}
}


coeff.FLFKMeans<-function(object){
	if(!is.null(object@results[["coeff"]]))
	return(object@results[["coeff"]])
	else
	{
		a <- genRandVarName()
		connection <- getConnection(object@table)
		flag3Check(connection)
		k<-1/object@centers

		sqlstr <- paste0("SELECT FLSum(a.Weight*a.Weight)/COUNT(DISTINCT a.ObsID) AS dunn_coeff,",
						 "(dunn_coeff-",k,")/",k," AS normalized ",
						" FROM fzzlKMeansMembership a ",
						" WHERE a.AnalysisID= '",object@AnalysisID,"'",
						" AND a.HypothesisID=1")

		coeffvector <- sqlQuery(connection,sqlstr)
		coeffvector <- c(coeffvector[["dunn_coeff"]],coeffvector[["normalized"]])
		names(coeffvector) <- c("dunn_coeff","normalized")

		object@results <- c(object@results,list(coeff = coeffvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(coeffvector)
	}
}


objective.FLFKMeans <- function(object){
	if(!is.null(object@results[["objective"]]))
	return(object@results[["objective"]])
	else
	{
		##Phani-- Query needs to be optimized.
		a <- genRandVarName()
		connection <- getConnection(object@table)
		flag3Check(connection)
		deeptablename <- paste0(object@deeptable@select@database,".",object@deeptable@select@table_name)
		obs_id_colname <- getVariables(object@deeptable)[["obs_id_colname"]]
		var_id_colname <- getVariables(object@deeptable)[["var_id_colname"]]
		cell_val_colname <- getVariables(object@deeptable)[["cell_val_colname"]]

		sqlstr <- paste0(" SELECT FLSum(a.dist/b.den) AS objective FROM ",
						"(SELECT b.ClusterID, FLSum(b.Weight**",object@memb.exp," * c.Weight**",object@memb.exp," * a.Dist) AS dist FROM ",
						"(SELECT a.",obs_id_colname," AS ObsIDX,",
								"c.",obs_id_colname," AS ObsIDY,",
								"FLEuclideanDist(a.",cell_val_colname,", c.",cell_val_colname,") AS Dist ",
						" FROM ",deeptablename," a,",deeptablename," c ",
						" WHERE a.",var_id_colname," = c.",var_id_colname,
						" GROUP BY 1,2) as a, fzzlKMeansMembership b, fzzlKMeansMembership c ",
						" WHERE b.ObsID=a.ObsIDX and b.ClusterID=c.ClusterID and c.ObsID=a.ObsIDY and b.AnalysisID='",object@AnalysisID,"'",
						" AND c.AnalysisID='",object@AnalysisID,"' and b.HypothesisID=1 and c.HypothesisID=1 ",
						" GROUP BY 1) AS a,",
						" (SELECT clusterID,FLSum(weight*weight*2) AS den FROM fzzlKMeansMembership ",
						" WHERE AnalysisID='",object@AnalysisID,"' and HypothesisID=1 ",
						" GROUP BY clusterID) as b ",
						" WHERE a.ClusterID=b.ClusterID")

		objectivevector <- c(as.numeric(sqlQuery(connection,sqlstr)[[1]]),0.000001)
		names(objectivevector) <- c("objective","tolerance")

		object@results <- c(object@results,list(objective = objectivevector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(objectivevector)
	}
	
}


k.crisp.FLFKMeans<-function(object){
	if(!is.null(object@results[["k.crisp"]]))
	return(object@results[["k.crisp"]])
	else
	{
		connection <- getConnection(object@table)
		flag3Check(connection)
		k<-1/object@centers

		sqlstr <- paste0("SELECT COUNT(DISTINCT a.ObsID) as nonCrispClusters FROM ",
						"(SELECT a.ObsID, COUNT(DISTINCT a.ClusterID) AS clusters",
						" FROM fzzlKMeansMembership a ",
						" WHERE a.AnalysisID= '",object@AnalysisID,"'",
						" AND a.HypothesisID=1 ",
						" AND a.Weight=",k,
						" GROUP BY a.ObsID) as a",
						" WHERE a.clusters=",object@centers)

		n <- sqlQuery(connection,sqlstr)[["nonCrispClusters"]]
		if(n!=0) k.crispvector<- object@centers-1
		else k.crispvector <- object@centers

		object@results <- c(object@results,list(k.crisp = k.crispvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(k.crispvector)
	}
}
## Number of iterations to convergence is not avalilable from DBLytix

convergence.FLFKMeans <- function(object){
	if(!is.null(object@results[["convergence"]]))
	return(object@results[["convergence"]])
	else
	{
		convergencevector <- c(NA,1,object@maxit)
		names(convergencevector) <- c("iterations","converged","maxit")
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(convergencevector)
	}
}


silinfo.FLFKMeans <- function(object){
	if(!is.null(object@results[["silinfo"]]))
	return(object@results[["silinfo"]])
	else
	{
		connection <- getConnection(object@table)
		flag3Check(connection)
		deeptablename <- paste0(object@deeptable@select@database,".",object@deeptable@select@table_name)
		obs_id_colname <- getVariables(object@deeptable)[["obs_id_colname"]]
		var_id_colname <- getVariables(object@deeptable)[["var_id_colname"]]
		cell_val_colname <- getVariables(object@deeptable)[["cell_val_colname"]]
		a <- paste0(genRandVarName(),"1")
		b <- paste0(genRandVarName(),"2")
		c <- paste0(getOption("ResultDatabaseFL"),".",gen_unique_table_name("3"))
		d <- paste0(getOption("ResultDatabaseFL"),".",gen_unique_table_name("4"))
		e <- paste0(getOption("ResultDatabaseFL"),".",gen_unique_table_name("5"))

		##Ensure required temptables exist
		if(is.null(object@temptables[["temptbl4"]]))
		{
			t <- sqlSendUpdate(connection,paste0(" create table ",e," as (SELECT a.",obs_id_colname," AS ObsID,a.",var_id_colname," AS VarID,a.",cell_val_colname," AS Num_Val,b.ClusterID 
									  FROM ",deeptablename," a,fzzlKMeansClusterID b WHERE a.",
									  obs_id_colname,"=b.ObsID and b.AnalysisID='",object@AnalysisID,"' AND b.HypothesisID=1)WITH DATA"))
			if(length(t)>1) stop(t)
			object@temptables <- c(object@temptables,list(temptbl4=e))
		}
		if(is.null(object@temptables[["temptbl2"]]))
		{
			t <- sqlSendUpdate(connection,paste0(" create table ",d," as (SELECT a.ClusterID as ClusterIDX,b.ClusterID as ClusterIDY,a.ObsID AS ObsIDX,
										b.ObsID AS ObsIDY,FLEuclideanDist(a.Num_Val, b.Num_Val) AS Dist
								  FROM ",object@temptables[["temptbl4"]]," a,",object@temptables[["temptbl4"]]," b 
								  WHERE a.VarID = b.VarID and a.ClusterID <> b.ClusterID
								  GROUP BY 1,2,3,4) with data"))
			if(length(t)>1) stop(t)
			object@temptables <- c(object@temptables,list(temptbl2=d))
		}
		if(is.null(object@temptables[["temptbl3"]]))
		{
			t <- sqlSendUpdate(connection,paste0(" create table ",c," as (SELECT a.ClusterID as ClusterIDX,a.ObsID AS ObsIDX,
											b.ObsID AS ObsIDY,FLEuclideanDist(a.Num_Val, b.Num_Val) AS Dist
									FROM ",object@temptables[["temptbl4"]]," a,",object@temptables[["temptbl4"]]," b 
									WHERE a.VarID = b.VarID and a.ClusterID = b.ClusterID
									GROUP BY 1,2,3) with data"))
			if(length(t)>1) stop(t)
			object@temptables <- c(object@temptables,list(temptbl3=c))
		}
		

		temptbl2 <- object@temptables[["temptbl2"]]
		temptbl3 <- object@temptables[["temptbl3"]]

		sqlstr<-paste0("select a.ObsIDX as obs_id_colname,a.ClusterIDX AS ClusterID,a.ClusterIDY AS neighbor,(a.num/a.den) as sil_width 
						from(select a.ClusterIDX ,b.ClusterIDY,a.ObsIDX ,((b.bi-a.ai)) as num, 
									Case when a.ai>b.bi then a.ai else  b.bi end as den 
							from(select a.ClusterIDX , a.ObsIDX, FLMean(a.Dist) as ai 
								from ",temptbl3," a
								group by 1,2) as a,
								(select b.ObsIDX,b.ClusterIDX,c.ClusterIDY,b.bi  
								from(select a.ClusterIDX, a.ObsIDX,min(a.di) as bi 
									from(select a.ClusterIDX , a.ClusterIDY,a.ObsIDX, cast(FLMean(a.Dist) as decimal(38,7)) as di  
										from ",temptbl2," a
										group by 1,2,3) as a
									group by 1,2) as b,
								(select a.ClusterIDX , a.ClusterIDY,a.ObsIDX, cast(FLMean(a.Dist)as decimal(38,7)) as di  
								from ",temptbl2," a
								group by 1,2,3) as c
							where c.ObsIDX=b.ObsIDX and c.ClusterIDX=b.ClusterIDX and b.bi=c.di) as b
						where a.ObsIDX=b.ObsIDX and a.ClusterIDX=b.ClusterIDX) as a")
		
		tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = connection,
                        variables = list(
			                obs_id_colname = "obs_id_colname"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

		widthsFLTable <- new("FLTable",
							select = tblfunqueryobj,
							dimnames = list(object@deeptable@dimnames[[1]],
											c("obs_id_colname","ClusterID","neighbor","sil_width")),
							isDeep = FALSE)

		widthsDataFrame <- tryCatch(as.data.frame(widthsFLTable),
      						error=function(e){store(widthsFLTable)})

		if(is.data.frame(widthsDataFrame))
		{
			ObsID <- widthsDataFrame[["obs_id_colname"]]
			if(is.null(ObsID) || length(ObsID)==0)
			ObsID <- rownames(widthsDataFrame)
			widthsDataFrame$obs_id_colname <- NULL
			widthsmatrix <- as.matrix(widthsDataFrame)
			rownames(widthsmatrix) <- ObsID
			colnames(widthsmatrix) <- c("cluster","neighbor","sil_width")
		}
		else widthsmatrix <- widthsFLTable

		sqlstr <- paste0("select '%insertIDhere%' AS vectorIdColumn, ROW_NUMBER() OVER(ORDER BY a.ClusterID) as vectorIndexColumn, FLMean(a.sil_width) as vectorValueColumn  
						from(select a.ClusterIDX as ClusterID, a.ObsIDX as ObsID,(a.num/a.den) as sil_width 
							from(select a.ClusterIDX ,a.ObsIDX ,((b.bi-a.ai)) as num, Case when a.ai>b.bi then a.ai else  b.bi end as den 
								from(select a.ClusterIDX , a.ObsIDX, FLMean(a.Dist) as ai 
									from ",temptbl3," a
									group by 1,2) as a,
								(select a.ClusterIDX, a.ObsIDX,FLMin(a.di) as bi 
								from(select a.ClusterIDX , a.ClusterIDY,a.ObsIDX, FLMean(a.Dist) as di  
									from ",temptbl2," a
									group by 1,2,3) as a
								group by 1,2) as b
							where a.ObsIDX=b.ObsIDX and a.ClusterIDX=b.ClusterIDX) as a) as a
							group by a.ClusterID")
		
		clus.avg.widthsvector <- tryCatch(sqlQuery(connection,sqlstr)[["vectorValueColumn"]],
										 error=function(e){
										 	tblfunqueryobj <- new("FLTableFunctionQuery",
										                        connection = connection,
										                        variables = list(
													                obs_id_colname = "vectorIndexColumn",
													                cell_val_colname = "vectorValueColumn"),
										                        whereconditions="",
										                        order = "",
										                        SQLquery=sqlstr)

											t <- new("FLVector",
													select = tblfunqueryobj,
													dimnames = list(1:object@centers,
																	"vectorValueColumn"),
													isDeep = FALSE)
											store(t)
										 })

		if(class(widthsmatrix)=="FLTable")
		{
			sqlstr <- paste0("select FLMean(a.sil_width) as avg_sil_width 
							from(select a.ClusterIDX as ClusterID, a.ObsIDX as ObsID,(a.num/a.den) as sil_width 
								from(select a.ClusterIDX ,a.ObsIDX ,((b.bi-a.ai)) as num, Case when a.ai>b.bi then a.ai else  b.bi end as den 
									from(select a.ClusterIDX , a.ObsIDX, FLMean(a.Dist) as ai 
										from ",temptbl3," a
										group by 1,2) as a,
										(select a.ClusterIDX, a.ObsIDX,FLMin(a.di) as bi 
										from(select a.ClusterIDX , a.ClusterIDY,a.ObsIDX, FLMean(a.Dist) as di  
											from ",temptbl2," a
											group by 1,2,3) as a
										group by 1,2) as b
									where a.ObsIDX=b.ObsIDX and a.ClusterIDX=b.ClusterIDX) as a) as a")
			
			avg.widthvector <- sqlQuery(connection,sqlstr)[["avg_sil_width"]]
		}
		else
		avg.widthvector <- tryCatch(base::mean(widthsmatrix[,"sil_width"]),
									error=function(e) base::mean(widthsmatrix[,"SIL_WIDTH"]))

		silinfolist <- list(widths=widthsmatrix,
							clus.avg.widths=clus.avg.widthsvector,
							avg.width=avg.widthvector)

		
		object@results <- c(object@results,list(silinfo = silinfolist))
		
		if((!(class(widthsmatrix)=="FLTable")) && (!(class(clus.avg.widthsvector)=="FLVector")))
		{
			t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["temptbl2"]]))
			object@temptables[["temptbl2"]] <- NULL
			t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["temptbl4"]]))
			object@temptables[["temptbl4"]] <- NULL
			t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["temptbl3"]]))
			object@temptables[["temptbl3"]] <- NULL
		}

		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(silinfolist)
	}
}


call.FLFKMeans<-function(object)
{
	if(!is.null(object@results[["call"]]))
	return(object@results[["call"]])
	else
	{
		callobject <- base::call("fanny",x=object@table,k=object@centers)
		
		object@results <- c(object@results,list(call = callobject))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(callobject)
	}
}


#' @export
print.FLFKMeans <- function(object)
{
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	results <- list()
	results <- c(results,list(membership=membership.FLFKMeans(object)),
						list(coeff=coeff.FLFKMeans(object)),
						list(clustering=clustering.FLFKMeans(object)),
						list(objective=objective.FLFKMeans(object)),
						list(k.crisp=k.crisp.FLFKMeans(object)),
						list(convergence=convergence.FLFKMeans(object)),
						list(silinfo=""),
						list(diss=""),
						list(call=call.FLFKMeans(object)),
						list(data=""),
						list(memb.exp=object@memb.exp))
	class(results) <- c("fanny","partition","silhouette")

	assign(parentObject,object,envir=parent.frame())
	print(results)
}

#' @export
setMethod("show","FLFKMeans",
			function(object)
			{
				parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
				print(object)
				assign(parentObject,object,envir=parent.frame())
			}
		 )

#' @export
plot.FLFKMeans <- function(object)
{
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	results <- list()
	dataframe <- data.FLKMedoids(object)
	if(is.null(dataframe) || length(dataframe)==0)
	l <- list(diss=as.matrix(diss.FLKMedoids(object)))
	else
	l <- list(data=dataframe)
	results <- c(results,
						list(clustering=clustering.FLFKMeans(object)),
						list(objective=""),
						list(silinfo=silinfo.FLFKMeans(object)),
						list(call=call.FLFKMeans(object)),
						l
						)
	class(results) <- c("fanny","partition","silhouette")

	assign(parentObject,object,envir=parent.frame())
	plot(results)
}
