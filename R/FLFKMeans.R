#' @include FLMatrix.R
NULL

## move to file FLFKMeans.R
#' An S4 class to represent FLFKMeans
#'
#' @slot centers A numeric vector containing the number of clusters, say k
#' @slot AnalysisID A character output used to retrieve the results of analysis
#' @slot wideToDeepAnalysisId A character string denoting the intermediate identifier
#' during widetable to deeptable conversion.
#' @slot diss logical TRUE if dissimilarity matrix is supplied to \code{fanny}
#' @slot table FLTable object given as input on which analysis is performed
#' @slot results A list of all fetched components
#' @slot deeptable A character vector containing a deeptable(either conversion from a 
#' widetable or input deeptable)
#' @slot temptables A list of temporary table names used across the results
#' @slot mapTable A character string name for the mapping table in-database if input is wide-table.
#' @slot memb.exp A number r strictly larger than 1 specifying the membership exponent 
#' used in the fit criterion.Default: 2 which is hardwired inside FANNY.
#' @slot maxit maximal number of iterations for the FANNY algorithm.
#' @method clustering FLFKMeans
#' @param object returns the clustering vector of the nearest crisp clustering.
#' @method membership FLFKMeans
#' @param object returns matrix containing the memberships for each pair consisting of an observation and a cluster.
#' @method coeff FLFKMeans
#' @param object returns vector with Dunn's partition coefficient F(k) of the clustering, where k is the number of clusters. F(k) is
#' the sum of all squared membership coefficients, divided by the number of observations. Its value is between 1/k and 1.
#' The normalized form of the coefficient is also given. It is defined as (F(k) - 1/k) / (1 - 1/k), and ranges between 0 and 1.
#' A low value of Dunn's coefficient indicates a very fuzzy clustering, whereas a value close to 1 indicates a near-crisp clustering.
#' @method objective FLFKMeans
#' @param object returns named vector containing the minimal value of the objective function reached by the FANNY algorithm and 
#' the relative convergence tolerance tol used.
#' @method k.crisp FLFKMeans
#' @param object returns integer (<= k) giving the number of crisp clusters; can be less than k, where it's 
#' recommended to decrease memb.exp.
#' @method convergence FLFKMeans
#' @param object returns named vector with iterations, the number of iterations needed and converged indicating if the algorithm 
#' converged (in maxit iterations within convergence tolerance tol).
#' @method silinfo FLFKMeans
#' @param object returns list with silhouette information of the nearest crisp clustering.
#' @method call FLFKMeans
#' @param object generating call
#' @method print FLFKMeans
#' @param object prints results of agglomerative clustering.
#' @method plot FLFKMeans
#' @param object plots results of agglomerative clustering.
setClass(
	"FLFKMeans",
	contains="FLDataMining",
	slots=list(
		centers="numeric",
		diss="logical",
		temptables="list",
		memb.exp="numeric",
		maxit="numeric"
	)
)

## move to file fanny.R
#' FuzzyKMeans Clustering.
#'
#' \code{fanny} performs fuzzy analysis on FLTable objects.
#'
#' The DB Lytix function called is FLFKMeans.Fuzzy K-Means clusters the training data. 
#' The relationship of observations to clusters are weighted by the
#' membership matrix, which is controlled by the degree of fuzziness.
#'
#' @seealso \code{\link[cluster]{fanny}} for R function reference implementation.
#' @param x an object of class FLTable, can be wide or deep table
#' @param k the number of clusters. t is required that 0 < k < n/2 where n is
#' the number of observations.
#' @param diss logical TRUE if \code{x} is dissimilarity matrix.
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
#' widetable  <- FLTable("iris", "rownames")
#' fkmeansobject <- fanny(widetable,2,memb.exp=2)
#' print(fkmeansobject)
#' plot(fkmeansobject)
#' One can specify ClassSpec and transform categorical variables 
#' before clustering. This increases the number of variables in the plot
#' because categorical variable is split into binary numerical variables.
#' The clusters may not be well-defined as is observed in the case below:-
#' widetable  <- FLTable("iris", "rownames")
#' fannyobjectnew <- fanny(widetable,3,classSpec=list("Species(setosa)"))
#' plot(fannyobjectnew)
#' @export
fanny <- function (x,k,...) {
  UseMethod("fanny", x)
}

#' @export
fanny.default <- function(x,k,...){
    if (!requireNamespace("cluster", quietly = TRUE)){
            stop("cluster package needed for fanny. Please install it.",
            call. = FALSE)
        }
    else return(cluster::fanny(x,k,...))
}

## move to file fanny.R
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

    connection <- getFLConnection(x)
    wideToDeepAnalysisId <- ""
    mapTable <- ""
	
	vcall <- match.call()
	if(!x@isDeep){
		deepx <- wideToDeep(x,excludeCols=excludeCols,
							classSpec=classSpec,
							whereconditions=whereconditions)

		wideToDeepAnalysisId <- deepx[["AnalysisID"]]
		deepx <- deepx[["table"]]
		deepx <- setAlias(deepx,"")
		whereconditions <- ""

		sqlstr <- paste0(" SELECT a.Final_VarID AS VarID,
			    	     	    a.COLUMN_NAME AS ColumnName,
			    	     	    a.FROM_TABLE AS MapName 
			    	     FROM fzzlRegrDataPrepMap a 
			    	     WHERE a.AnalysisID = '",wideToDeepAnalysisId,"' 
			    	     AND a.Final_VarID IS NOT NULL ")
		
		mapTable <- createTable(pTableName=gen_wide_table_name("map"),
                                pSelect=sqlstr)
	}
	else if(class(x@select)=="FLTableFunctionQuery")
	{
		#sqlstr <- paste0("CREATE VIEW ",getOption("ResultDatabaseFL"),".",
		#				deeptablename," AS \n ",constructSelect(x))
		#sqlSendUpdate(connection,sqlstr)
		deeptablename <- createView(pViewName=gen_view_name(""),
                                    pSelect=constructSelect(x))

		
		#sqlstr <- paste0("CREATE VIEW ",getOption("ResultDatabaseFL"),".",deeptablename1,
		#				" AS  \n SELECT * FROM ",getOption("ResultDatabaseFL"),
		#				".",deeptablename,constructWhere(whereconditions))
		#t <- sqlSendUpdate(connection,sqlstr)
		
		deeptablename1<-createView(pViewName=gen_view_name("New"),
					pSelect=paste0("SELECT * FROM ", deeptablename,constructWhere(whereconditions))
					)

		deepx <- FLTable(
                   deeptablename1,
                   "obs_id_colname",
                   "var_id_colname",
                   "cell_val_colname"
                  )
		deepx <- setAlias(deepx,"")
		whereconditions <- ""
	}
	else
	{
		x@select@whereconditions <- c(x@select@whereconditions,whereconditions)
		#sqlstr <- paste0("CREATE VIEW ",getOption("ResultDatabaseFL"),".",
		#				deeptablename," AS  \n ",constructSelect(x))
		#t <- sqlSendUpdate(connection,sqlstr)
		deeptablename<-createView(pViewName=gen_view_name("New"),
                                  pSelect=constructSelect(x))

		deepx <- FLTable(
                   deeptablename,
                   "obs_id_colname",
                   "var_id_colname",
                   "cell_val_colname"
                  )
		deepx <- setAlias(deepx,"")
		whereconditions <- ""
	}

	whereconditions <- whereconditions[whereconditions!=""]
	whereClause <- constructWhere(whereconditions)
	deeptable <- deepx@select@table_name
	if(whereClause=="") whereClause <- "NULL"

	if(diss)
	{
		cat(" diss is not supported currently. Please input data table instead.")
		diss <- FALSE
	}

	retobj <- sqlStoredProc(
        connection,
        "FLFKMeans",
        TableName=deeptable,
        ObsIDColName=getVariables(deepx)[["obs_id_colname"]],
        VarIDColName=getVariables(deepx)[["var_id_colname"]],
        ValueColName=getVariables(deepx)[["cell_val_colname"]],
        WhereClause= whereClause,
        Clusters=k,
        Iterations=maxit,
        Fuzzy=memb.exp,
        Hypothesis=1,
        Note=genNote("fkmeans"),
        outputParameter=c(AnalysisID="a")
        )

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

## move to file FLFKMeans.R
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

## move to file FLFKMeans.R
clustering.FLFKMeans <- function(object)
{
	if(!is.null(object@results[["clustering"]]))
	return(object@results[["clustering"]])
	else
	{
		connection <- getFLConnection(object@table)
		## flag3Check(connection)
		AnalysisID <- object@AnalysisID
		sqlstr<-paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
						    "    ObsID AS vectorIndexColumn, \n ",
						    "    CAST(ClusterID AS INTEGER) AS vectorValueColumn \n ",
						"FROM fzzlKMeansClusterID \n ",
						"WHERE AnalysisID = ",fquote(AnalysisID)," AND \n ",
						"HypothesisID = 1 ")

		tblfunqueryobj <- new("FLTableFunctionQuery",
                        connectionName = attr(connection,"name"),
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

		clusteringvector <- newFLVector(
							select = tblfunqueryobj,
							Dimnames = list(object@deeptable@Dimnames[[1]],
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

## move to file FLFKMeans.R
membership.FLFKMeans<-function(object)
{
	if(!is.null(object@results[["membership"]]))
	return(object@results[["membership"]])
	else
	{
		connection <- getFLConnection(object@table)
		## flag1Check(connection)
		AnalysisID <- object@AnalysisID

		sqlstr<-paste0("SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
					        " a.obsID AS rowIdColumn, \n ",
					        " a.ClusterID AS colIdColumn, \n ",
					        " a.Weight AS valueColumn \n ",
						"FROM fzzlKMeansMembership AS a \n ",
						"WHERE a.AnalysisID = ",fquote(AnalysisID)," \n ",
						" AND a.HypothesisID=1")

		tblfunqueryobj <- new("FLTableFunctionQuery",
                        connectionName = attr(connection,"name"),
                        variables=list(
                            rowIdColumn="rowIdColumn",
                            colIdColumn="colIdColumn",
                            valueColumn="valueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

	  	membershipmatrix <- newFLMatrix(
				            select= tblfunqueryobj,
				            dims=as.integer(c(nrow(object@deeptable),
                                                              object@centers)),
				            Dimnames=list(
                                                rownames(object@deeptable),
                                                1:object@centers))

		membershipmatrix <- tryCatch(as.matrix(membershipmatrix),
      						error=function(e){membershipmatrix})
		
		object@results <- c(object@results,list(membership = membershipmatrix))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(membershipmatrix)
	}
}

## move to file FLFKMeans.R
coeff.FLFKMeans<-function(object){
	if(!is.null(object@results[["coeff"]]))
	return(object@results[["coeff"]])
	else
	{
		a <- genRandVarName()
		connection <- getFLConnection(object@table)
		## flag3Check(connection)
		k<-1/object@centers

		sqlstr <- paste0("SELECT a.dunn_coeff, (a.dunn_coeff-",k,")/",k," AS normalized ",
                        " FROM ( \n ",
                        " SELECT FLSum(a.Weight*a.Weight)/COUNT(DISTINCT a.ObsID) AS dunn_coeff \n ",
						" FROM fzzlKMeansMembership a \n ",
						" WHERE a.AnalysisID= '",object@AnalysisID,"' \n ",
						" AND a.HypothesisID=1 \n ) a ")

		coeffvector <- sqlQuery(connection,sqlstr)
		coeffvector <- c(coeffvector[["dunn_coeff"]],coeffvector[["normalized"]])
		names(coeffvector) <- c("dunn_coeff","normalized")

		object@results <- c(object@results,list(coeff = coeffvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(coeffvector)
	}
}

## move to file FLFKMeans.R
objective.FLFKMeans <- function(object){
	if(!is.null(object@results[["objective"]]))
	return(object@results[["objective"]])
	else
	{
		##Phani-- Query needs to be optimized.
		a <- genRandVarName()
		connection <- getFLConnection(object@table)
            ## flag3Check(connection)
		deeptablename <- object@deeptable@select@table_name
		obs_id_colname <- getVariables(object@deeptable)[["obs_id_colname"]]
		var_id_colname <- getVariables(object@deeptable)[["var_id_colname"]]
		cell_val_colname <- getVariables(object@deeptable)[["cell_val_colname"]]

		sqlstr <- paste0(" SELECT FLSum(a.dist/b.den) AS objective  \n FROM \n ",
						"(SELECT b.ClusterID, FLSum(power(b.Weight,",object@memb.exp,") * power(c.Weight,",
								object@memb.exp,") * a.Dist) AS dist  \n FROM  \n ",
						"(SELECT a.",obs_id_colname," AS ObsIDX, \n ",
								"c.",obs_id_colname," AS ObsIDY, \n ",
								"FLEuclideanDist(a.",cell_val_colname,", c.",cell_val_colname,") AS Dist  \n ",
						" FROM ",deeptablename," a, \n ",deeptablename," c  \n ",
						" WHERE a.",var_id_colname," = c.",var_id_colname,
						"  \n GROUP BY 1,2) as a, fzzlKMeansMembership b, fzzlKMeansMembership c ",
						" WHERE b.ObsID=a.ObsIDX and b.ClusterID=c.ClusterID \n  and c.ObsID=a.ObsIDY and b.AnalysisID='",object@AnalysisID,"'",
						"  \n AND c.AnalysisID='",object@AnalysisID,"' and \n  b.HypothesisID=1 and c.HypothesisID=1 ",
						" GROUP BY 1) AS a, \n ",
						" (SELECT clusterID,FLSum(weight*weight*2) AS den  \n FROM fzzlKMeansMembership  \n ",
						" WHERE AnalysisID='",object@AnalysisID,"' and HypothesisID=1 \n ",
						" GROUP BY clusterID) as b \n ",
						" WHERE a.ClusterID=b.ClusterID")

		objectivevector <- c(as.numeric(sqlQuery(connection,sqlstr)[[1]]),0.000001)
		names(objectivevector) <- c("objective","tolerance")

		object@results <- c(object@results,list(objective = objectivevector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(objectivevector)
	}
	
}

## move to file FLFKMeans.R
k.crisp.FLFKMeans<-function(object){
	if(!is.null(object@results[["k.crisp"]]))
	return(object@results[["k.crisp"]])
	else
	{
		connection <- getFLConnection(object@table)
		## flag3Check(connection)
		k<-1/object@centers

		sqlstr <- paste0("SELECT COUNT(DISTINCT a.ObsID) as noncrispclusters \n FROM \n ",
						"(SELECT a.ObsID, COUNT(DISTINCT a.ClusterID) AS clusters \n ",
						" FROM fzzlKMeansMembership a \n ",
						" WHERE a.AnalysisID= '",object@AnalysisID,"' \n ",
						" AND a.HypothesisID=1  \n ",
						" AND a.Weight=",k,
						"  \n GROUP BY a.ObsID) as a \n ",
						" WHERE a.clusters=",object@centers)

		n <- sqlQuery(connection,sqlstr)[["noncrispclusters"]]
		if(n!=0) k.crispvector<- object@centers-1
		else k.crispvector <- object@centers

		object@results <- c(object@results,list(k.crisp = k.crispvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(k.crispvector)
	}
}
## Number of iterations to convergence is not avalilable from DBLytix
## move to file FLFKMeans.R
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

## move to file FLFKMeans.R
silinfo.FLFKMeans <- function(object){
	if(!is.null(object@results[["silinfo"]]))
	return(object@results[["silinfo"]])
	else
	{
		connection <- getFLConnection(object@table)
		## flag3Check(connection)
		deeptablename <- object@deeptable@select@table_name
		obs_id_colname <- getVariables(object@deeptable)[["obs_id_colname"]]
		var_id_colname <- getVariables(object@deeptable)[["var_id_colname"]]
		cell_val_colname <- getVariables(object@deeptable)[["cell_val_colname"]]
		b <- gen_unique_table_name("3") ## gk: refactor!

		##Ensure required temptables exist
		t <- createTable(pTableName=b,
                        pSelect=paste0("SELECT  a.",obs_id_colname," AS ObsIDX, b.",
                                                obs_id_colname," AS ObsIDY, \n p.clusterid AS ClusIDX , ",
                                                " q.Clusterid AS ClusIDY , \n ",
                                                " FLEuclideanDist(a.",cell_val_colname,
                                                                ", b.",cell_val_colname,") AS Dist \n ",
                                        " FROM ",deeptablename," a, ",
                                                deeptablename," b, fzzlkmeansclusterid p, fzzlkmeansclusterid q \n ",
                                        " WHERE a.",var_id_colname," = b.",var_id_colname,
                                                " AND a.",obs_id_colname," < b.",obs_id_colname,
                                                " \n  AND p.obsid = a.",obs_id_colname,
                                                " \n AND q.Obsid = b.",obs_id_colname, 
                                                " \n AND q.AnalysisID = '",object@AnalysisID,
                                                "' AND q.HypothesisID = 1  AND p.AnalysisID = '",
                                                object@AnalysisID,"'AND p.HypothesisID = 1 \n ",
                                        " GROUP BY a.",obs_id_colname,", b.",
                                                        obs_id_colname,", p.clusterid, q.clusterid "))
		# u <- sqlSendUpdate(connection, paste0("INSERT INTO ",b,
		# 								" SELECT ObsIDY, ObsIDX, ClusIDY, ClusIDX, Dist FROM ",b))
        u <- insertIntotbl(pTableName=b,
                           pSelect=paste0(" SELECT ObsIDY, ObsIDX, ClusIDY, ClusIDX, Dist FROM ",b))


				
		sili_table <- sqlQuery(connection, paste0("SELECT a.ObsIDX AS obsid, \n ",
                                                    " CAST(a.ClusIDX as INT) AS clusid, \n ",
                                                    " CAST(B2_i.Neighbour AS INT) AS neighbour, \n ",
													" CASE WHEN FLMean(a.Dist) >  B2_i.val \n ",
													" THEN (B2_i.val - FLMean(a.Dist) )/ FLMean(a.Dist) ",
													" ELSE  (B2_i.val - FLMean(a.Dist) )/B2_i.val ",
													" END AS siliwidth \n ",
													" FROM ",b," AS a \n ,",
													" (SELECT Bi.ObsID AS ObsIDX , FLMin(Bi.Val) AS q \n ",
													" FROM \n ",
													" (SELECT b.ObsIDX AS ObsID, FLMean(b.Dist) AS val \n ",
													" FROM ",b," AS b \n ",
													" WHERE b.ClusIDX <> b.ClusIDY \n ",
													" GROUP BY b.ObsIDX, b.ClusIDY ",
													" ) AS Bi \n ",
													" GROUP BY Bi.ObsID) ",
													" AS  B1_i, \n ",
													" (SELECT b.ObsIDX AS ObsID, b.ClusIDY AS Neighbour, FLMean(b.Dist) AS val \n ",
													" FROM ",b," AS b \n ",
													" WHERE b.ClusIDX <> b.ClusIDY \n ",
													" GROUP BY b.ObsIDX, b.ClusIDY \n ",
													" ) AS B2_i \n ",
													" WHERE B1_i.q= B2_i.val AND B1_i.ObsIDX = B2_i.ObsID \n ",
                                                    " AND a.ClusIDX = a.ClusIDY AND a.ObsIDX = B2_i.ObsID \n ",
													" GROUP BY a.obsIDX, a.ClusIDX, B2_i.Neighbour, B2_i.val \n ",
													" ORDER BY 1,2"))
	}	
    sili_matrix <- as.matrix(sili_table[,c("clusid","neighbour","siliwidth")])
    colnames(sili_matrix) <- c("cluster","neighbor","sil_width")
    rownames(sili_matrix) <- sili_table$obsid
    
    clus.avg.width <- as.numeric(lapply(1:object@centers, function(i){
        mean(sili_table$siliwidth[sili_table$clusid == i])
    }))
    silinfolist <- list(widths = sili_matrix, clus.avg.widths = clus.avg.width)
    return(silinfolist)																	
}

## move to file FLFKMeans.R
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

## move to file FLFKMeans.R
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

## move to file FLFKMeans.R
#' @export
setMethod("show","FLFKMeans",
			function(object)
			{
				parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
				print(object)
				assign(parentObject,object,envir=parent.frame())
			}
		 )

## move to file FLFKMeans.R
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

## move to file fanny.R
#' @export
fanny.FLMatrix <- function(x,
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
	x <- as.FLTable(x)
	vcall <- match.call()
	fannyobj <- (fanny(x=x,
				k=k,
				diss=diss,
				memb.exp= memb.exp,
				metric=metric,##notUsed
				Stand=Stand,##notUsed
				iniMem.p=iniMem.p,##notUsed
				cluster.only = cluster.only,
				keep.diss = keep.diss,
				keep.data = keep.data,
				maxit = maxit,
				tol = tol,##From dbLytix
				trace.lev = trace.lev,##notUsed
				excludeCols = excludeCols,
				classSpec = classSpec,
				whereconditions = whereconditions,
				distTable=distTable
			))
	fannyobj@results$call <- vcall
	return(fannyobj)
}
