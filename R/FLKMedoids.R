#' @include FLMatrix.R
NULL

## move to file FLKMedoids.R
#' An S4 class to represent FLKMedoids
#'
#' @slot centers A numeric vector containing the number of clusters, say k
#' @slot AnalysisID A character output used to retrieve the results of analysis
#' @slot wideToDeepAnalysisId A character string denoting the intermediate identifier
#' during widetable to deeptable conversion.
#' @slot diss logical TRUE if dissimilarity matrix is supplied to \code{pam}
#' @slot table FLTable object given as input on which analysis is performed
#' @slot results A list of all fetched components
#' @slot deeptable A character vector containing a deeptable(either conversion from a 
#' widetable or input deeptable)
#' @slot temptables A list of temporary table names used across the results
#' @slot mapTable A character string name for the mapping table in-database if input is wide-table.
#' @slot distTable Name of the distance matrix. DistTable should
#' contain a N x N distance matrix between all ObsID in an input FLTable.
#' @slot maxit maximal number of iterations for the FANNY algorithm.
#' @method clustering FLKMedoids
#' @param object retrieves the clustering vector
#' @method medoids FLKMedoids
#' @param object returns matrix of the medoids or representative objects of the clusters. If a dissimilarity matrix was given as 
#' input to pam, then a vector of numbers or labels of observations is given, else medoids is a matrix with in 
#' each row the coordinates of one medoid.
#' @method id.med FLKMedoids
#' @param object returns integer vector of indices giving the medoid observation numbers.
#' @method objective FLKMedoids
#' @param object the objective function after the first and second step of the pam algorithm.
#' @method isolation FLKMedoids
#' @param object returns vector with length equal to the number of clusters, specifying which clusters are isolated clusters 
#' (L- or L*-clusters) and which clusters are not isolated.
#' @method clusinfo FLKMedoids
#' @param object returns matrix, each row gives numerical information for one cluster. These are the cardinality of the cluster
#' (number of observations), the maximal and average dissimilarity between the observations in the cluster and the cluster's
#' medoid, the diameter of the cluster (maximal dissimilarity between two observations of the cluster), and the separation 
#' of the cluster (minimal dissimilarity between an observation of the cluster and an observation of another cluster).
#' @method silinfo FLKMedoids
#' @param object returns list with silhouette width information.
#' @method diss FLKMedoids
#' @param object dissimilarity (maybe NULL).
#' @method call FLKMedoids
#' @param object function generating call.
#' @method data FLKMedoids
#' @param object returns a matrix containing the original or standardized data. This might be missing to save memory or when a 
#' dissimilarity matrix was given as input structure to the clustering method.
#' @method print FLKMedoids
#' @param object prints the results of pam on FL objects.
#' @method plot FLKMedoids
#' @param object plots the results of pam on FL objects.
#' @method FLMapping FLKMedoids
#' @param object gives the mapping data.frame which is used in execution.
#' @export

setClass(
	"FLClustering",
	slots=list(
		centers="numeric",
		maxit="numeric"
	)
)

setClass(
	"FLKMedoids",
	contains=c("FLDataMining",
				"FLClustering"),
	slots=list(
		diss="logical",
		temptables="list",
		distTable="character"
	)
)
## move to file pam.R
#' K-Medoids Clustering.
#'
#' \code{pam} performs k-medoids clustering on FLTable objects.
#'
#' The DB Lytix function called is FLKMedoids. K-Medoids clusters the training data. 
#' The algorithm used is PAM (Partitioning Around Medoids).
#'
#' @seealso \code{\link[cluster]{pam}} for R function reference implementation.
#' @param x an object of class FLTable, can be wide or deep table
#' @param k the number of clusters
#' @param diss logical if \code{x} is dissimilarity matrix
#' @param metric only "euclidean" distance supported currently
#' @param medoids initial medoids. Not used currently.
#' @param Stand logical indicating if standardization
#' should be done before calculating diss matrix.
#' @param cluster.only logical if only clustering vector is 
#' needed as output
#' @param do.swap logical indicating if swap phase is needed.
#' currently always TRUE
#' @param keep.diss logicals indicating if the 
#' dissimilarities and/or input data x should be kept in the result
#' @param keep.data logicals indicating if the 
#' dissimilarities and/or input data x should be kept in the result
#' @param pomance logical or integer in 0:2 specifying algorithmic short cuts.
#' currently always FALSE
#' @param trace.lev integer specifying a trace level for 
#' printing diagnostics during the build and swap phase of the algorithm.
#' currently always 0
#' @param iter.max the maximum number of iterations allowed
#' @param excludeCols the comma separated character string of columns to be excluded
#' @param classSpec list describing the categorical dummy variables
#' @param whereconditions takes the where_clause as a string
#' @param distTable name of the in-database table having dissimilarity
#' matrix or distance table
#' @section Constraints:
#' Plotting time increases with size of data and even fail for large
#' datasets because data is fetched to R session for plotting.
#' If classSpec is not specified, the categorical variables are excluded
#' from analysis by default.
#' @return \code{pam} gives a list which replicates equivalent R output
#' from \code{pam} in cluster package. The mapping table can be viewed
#' using \code{object$mapping} if input is wide table.
#' @examples
#' widetable  <- FLTable("iris", "rownames")
#' kmedoidsobject <- pam(widetable,3)
#' print(kmedoidsobject)
#' plot(kmedoidsobject)
#' One can specify ClassSpec and transform categorical variables 
#' before clustering. This increases the number of variables in the plot
#' because categorical variable is split into binary numerical variables.
#' The clusters may not be well-defined as is observed in the case below:-
#' widetable  <- FLTable("iris", "rownames")
#' pamobjectnew <- pam(widetable,3,classSpec=list("Species(setosa)"))
#' plot(pamobjectnew)
#' @export
pam <- function (x,k,...) {
  UseMethod("pam", x)
}

#' @export
pam.default <- function(x,k,...){
    if (!requireNamespace("cluster", quietly = TRUE)){
            stop("cluster package needed for pam. Please install it.",
            call. = FALSE)
        }
    else return(cluster::pam(x,k,...))
}

## move to file pam.R
#' @export
pam.FLTable <- function(x,
						k,
						diss=FALSE,
						metric="euclidean",##notUsed
						medoids=NULL,##notUsed
						Stand=FALSE,##notUsed
						cluster.only = FALSE,
    					do.swap = TRUE,##notUsed
    					keep.diss = (!diss && !cluster.only),
    					keep.data = (!diss && !cluster.only),
    					pamonce = FALSE,##notUsed
    					trace.lev = 0,##notUsed
    					iter.max =10,
						excludeCols = "",
						classSpec = list(),
						whereconditions = "",
						distTable=""
						)
{

	#Type validation
	if(any(!(c(k,iter.max) >= 1)))
    stop("k,iter.max should be atleast 1")
    else
    {
    	k  <- as.integer(max(k))
    	iter.max <- as.integer(max(iter.max))
    }

	argList  <- as.list(environment())

	typeList <- list(	k      = "integer",
						iter.max     = "integer",
						excludeCols  = "character",
						classSpec   = "list",
						whereconditions = "character",
						diss = "logical",
						metric = "character",
						Stand = "logical",
						cluster.only = "logical",
						do.swap = "logical",
						keep.diss = "logical",
						keep.data = "logical",
						pamonce = "logical",
						distTable = "character"
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

		sqlstr <- paste0(" SELECT a.*  
			    	     FROM fzzlRegrDataPrepMap a 
			    	     WHERE a.AnalysisID = '",wideToDeepAnalysisId,"' ")
		
		mapTable <- createTable(pTableName=gen_wide_table_name("map"),
                                pSelect=sqlstr)
	}
	else if(class(x@select)=="FLTableFunctionQuery")
	{
		
		#sqlstr <- paste0("CREATE VIEW ",getOption("ResultDatabaseFL"),
		#				".",deeptablename," AS  \n ",constructSelect(x))
		#sqlSendUpdate(connection,sqlstr)
        x <- setIndexSQLName(x,1,"obsid")
		deeptablename <- createView(pViewName=gen_view_name(""),
					pSelect=constructSelect(x))

		#sqlstr <- paste0("CREATE VIEW ",getOption("ResultDatabaseFL"),
		#				".",deeptablename1,
		#				" AS  \n SELECT * FROM ",getOption("ResultDatabaseFL"),
		#				".",deeptablename,constructWhere(whereconditions))
		#t <- sqlSendUpdate(connection,sqlstr)
		deeptablename1 <- createView(pViewName=gen_view_name("New"),
                                     pSelect=paste0("SELECT * FROM ", deeptablename,constructWhere(whereconditions)))	


		deepx <- FLTable(deeptablename1,
                        getIndexSQLName(x,margin=1),
                        getIndexSQLName(x,margin=2),
                        getIndexSQLName(x,margin=3)
                        )
		deepx <- setAlias(deepx,"")
		whereconditions <- ""
	}
	else
	{
        x <- setIndexSQLName(x,1,"obsid")
        whereconditions <- c(x@select@whereconditions,whereconditions)
        x@select@whereconditions <- whereconditions
        if(length(setdiff(whereconditions,""))>0){
            # x@select@whereconditions <- c(x@select@whereconditions,whereconditions)
        
            #sqlstr <- paste0("CREATE VIEW ",getOption("ResultDatabaseFL"),
            #               ".",deeptablename," AS \n  ",constructSelect(x))
            #t <- sqlSendUpdate(connection,sqlstr)
            deeptablename<-createView(pViewName=gen_view_name("New"),
                        pSelect=constructSelect(x))

            deepx <- FLTable(deeptablename,
                            getIndexSQLName(x,margin=1),
                            getIndexSQLName(x,margin=2),
                            getIndexSQLName(x,margin=3)
                            )
        }
        else deepx <- x
		deepx <- setAlias(deepx,"")
		whereconditions <- ""
	}

	whereconditions <- whereconditions[whereconditions!=""]
	whereClause <- constructWhere(whereconditions)
	deeptable <- getTableNameSlot(deepx)
	if(whereClause=="") whereClause <- "NULL"

	if(diss)
	{
		if(distTable=="") 
		stop("Since diss=TRUE, provide distTable input in the form of database.table")
		distTableCopy <- distTable
	}
	else distTableCopy <- "NULL"

	retobj <- sqlStoredProc(
        connection,
        "FLKMedoids",
        outputParameter=c(AnalysisID="a"),
        TableName=deeptable,
        ObsIDColName=getIndexSQLExpression(deepx,1),
        VarIDColName=getIndexSQLExpression(deepx,2),
        ValueColName=getIndexSQLExpression(deepx,3),
        WhereClause= whereClause,
        Medoids=k,
        Iterations=iter.max,
        DistTable=distTableCopy,
        Note=genNote("kmedoids")
        )

	retobj <- checkSqlQueryOutput(retobj)
	AnalysisID <- as.character(retobj[1,1])

	## medoid points are necessary for calculating some results
	sqlstr0 <- paste0(" SELECT AnalysisID,MedoidID,MedoidID \n ", 
                        " FROM fzzlKMedoidsMedoidIDs \n ",
                        " WHERE AnalysisID='",AnalysisID,"' ")
    # sqlSendUpdate(connection,sqlstr0)
    insertIntotbl(pTableName="fzzlKMedoidsCluster",
                pSelect=sqlstr0)
	
	FLKMedoidsobject <- new("FLKMedoids",
						centers=k,
						AnalysisID=AnalysisID,
						wideToDeepAnalysisId=wideToDeepAnalysisId,
						table=x,
						results=list(call=vcall),
						deeptable=deepx,
						diss=diss,
						temptables=list(),
						mapTable=mapTable,
						distTable=distTable)
	if(cluster.only)
	return(FLKMedoidsobject$clustering)
	else return(FLKMedoidsobject)
}

## move to file FLKMedoids.R
#' @export
`$.FLKMedoids`<-function(object,property)
{
	#parentObject <- deparse(substitute(object))
	parentObject <- unlist(strsplit(unlist(strsplit(
					as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

	if(property=="medoids")
	{
		medoidsmatrix <- medoids.FLKMedoids(object)
		assign(parentObject,object,envir=parent.frame())
		return(medoidsmatrix)
	}
	else if(property=="id.med")
	{
		id.medvector <- id.med.FLKMedoids(object)
		assign(parentObject,object,envir=parent.frame())
		return(id.medvector)
	}
	else if(property=="clustering")
	{
		clusteringvector <- clustering.FLKMedoids(object)
		assign(parentObject,object,envir=parent.frame())
		return(clusteringvector)
	}
	else if(property=="objective")
	{
		objectivevector <- objective.FLKMedoids(object)
		assign(parentObject,object,envir=parent.frame())
		return(objectivevector)
	}
	else if(property=="isolation")
	{
		isolationvector <- isolation.FLKMedoids(object)
		assign(parentObject,object,envir=parent.frame())
		return(isolationvector)
	}
	else if(property=="clusinfo")
	{
		clusinfomatrix <- clusinfo.FLKMedoids(object)
		assign(parentObject,object,envir=parent.frame())
		return(clusinfomatrix)
	}
	else if(property=="silinfo")
	{
		silinfolist <- silinfo.FLKMedoids(object)
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
		callobject <- call.FLKMedoids(object)
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

## move to file FLKMedoids.R
clustering.FLKMedoids <- function(object)
{
	if(!is.null(object@results[["clustering"]]))
	return(object@results[["clustering"]])
	else
	{
		connection <- getFLConnection(object@table)
		## flag3Check(connection)
		AnalysisID <- object@AnalysisID
		sqlstr<-paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
								" a.ObsID AS vectorIndexColumn, \n ", 
						        " b.newMedoidID AS vectorValueColumn \n ",  
						"FROM fzzlKMedoidsCluster a, \n ",
						"(SELECT z.MedoidID as MedoidID, \n ",
							" ROW_NUMBER() OVER(ORDER BY z.MedoidID) AS newMedoidID \n ",
						" FROM fzzlKMedoidsMedoidIDs z \n ", 
						"WHERE z.AnalysisID = ",fquote(AnalysisID),") AS b \n ", 
						"WHERE a.AnalysisID = ",fquote(AnalysisID),
						" \n  AND a.MedoidID=b.MedoidID")

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

## move to file FLKMedoids.R
medoids.FLKMedoids<-function(object)
{
	if(!is.null(object@results[["medoids"]]))
	return(object@results[["medoids"]])
	else
	{
		if(object@diss)
		medoidsmatrix <- id.med.FLKMedoids(object)
		else
		{
			connection <- getFLConnection(object@table)
			## flag1Check(connection)
			AnalysisID <- object@AnalysisID
			deeptablename <- getTableNameSlot(object@deeptable)
			obs_id_colname <- getIndexSQLExpression(object@deeptable,1)
			var_id_colname <- getIndexSQLExpression(object@deeptable,2)
			cell_val_colname <- getIndexSQLExpression(object@deeptable,3)

			sqlstr<-paste0("SELECT '%insertIDhere%' AS MATRIX_ID, \n ", 
						        " b.newMedoidID AS rowIdColumn, \n ",
						        " a.",var_id_colname," AS colIdColumn, \n ",
						        " a.",cell_val_colname," AS valueColumn \n ",
							" FROM ",deeptablename," AS a, \n ",
							"(SELECT z.MedoidID as MedoidID, \n ",
								" ROW_NUMBER() OVER(ORDER BY z.MedoidID) AS newMedoidID \n ",
							" FROM fzzlKMedoidsMedoidIDs z \n ", 
							" WHERE z.AnalysisID = '",AnalysisID,"') AS b \n ", 
							" WHERE a.",obs_id_colname," = b.MedoidID")

			tblfunqueryobj <- new("FLTableFunctionQuery",
	                        connectionName = attr(connection,"name"),
	                        variables=list(
	                            rowIdColumn="rowIdColumn",
	                            colIdColumn="colIdColumn",
	                            valueColumn="valueColumn"),
	                        whereconditions="",
	                        order = "",
	                        SQLquery=sqlstr)

		  	medoidsmatrix <- newFLMatrix(
					            select= tblfunqueryobj,
					            dims=as.integer(c(object@centers,
                                                length(object@deeptable@Dimnames[[2]]))),
					            Dimnames=list(1:object@centers,
					            			object@deeptable@Dimnames[[2]]))
		}
		if(is.FLMatrix(medoidsmatrix))
		medoidsmatrix <- tryCatch(as.matrix(medoidsmatrix),
      						error=function(e){medoidsmatrix})
		else
		medoidsmatrix <- tryCatch(as.vector(medoidsmatrix),
      						error=function(e){medoidsmatrix})
		
		object@results <- c(object@results,list(medoids = medoidsmatrix))
		parentObject <- unlist(strsplit(unlist(strsplit(
						as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(medoidsmatrix)
	}
}

## move to file FLKMedoids.R
id.med.FLKMedoids<-function(object){
	if(!is.null(object@results[["id.med"]]))
	return(object@results[["id.med"]])
	else
	{
		a <- genRandVarName()
		connection <- getFLConnection(object@table)
		## flag3Check(connection)
		sqlstr<-paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
						        " ROW_NUMBER() OVER(ORDER BY ",a,".MedoidID) AS vectorIndexColumn, \n ",
						         a,".MedoidID AS vectorValueColumn \n ", 
						" FROM fzzlKMedoidsMedoidIDs ",a," \n ",
						" WHERE ",a,".AnalysisID = '",object@AnalysisID,"'")

		tblfunqueryobj <- new("FLTableFunctionQuery",
                        connectionName = attr(connection,"name"),
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

		id.medvector <- newFLVector(
							select = tblfunqueryobj,
							Dimnames = list(1:object@centers,
											"vectorValueColumn"),
							isDeep = FALSE)
		id.medvector <- tryCatch(as.vector(id.medvector),
      						error=function(e){id.medvector})

		object@results <- c(object@results,list(id.med = id.medvector))
		parentObject <- unlist(strsplit(unlist(strsplit(
						as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(id.medvector)
	}
}

## move to file FLKMedoids.R
objective.FLKMedoids <- function(object){
	if(!is.null(object@results[["objective"]]))
	return(object@results[["objective"]])
	else
	{
		## DBLytix TotalCost is the average;R cost is absolute.
		## The idea is the same,i.e to see improvement from build to swap
		a <- genRandVarName()
		connection <- getFLConnection(object@table)
            ## flag3Check(connection)
		n <- nrow(object@deeptable)

		sqlstr<-paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
						         a,".Iteration AS vectorIndexColumn, \n ",
						         a,".TotalCost AS vectorValueColumn \n ", 
						" FROM fzzlKMedoidsTotalCost ",a," \n ",
						" WHERE ",a,".AnalysisID = '",object@AnalysisID,"' \n ", 
						" AND ",a,".Iteration < 3 \n ",
						" ORDER BY ",a,".Iteration")

		objectivevector <- as.numeric(sqlQuery(connection,sqlstr)[[3]])
		names(objectivevector) <- c("build","swap")

		object@results <- c(object@results,list(objective = objectivevector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
                            "(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(objectivevector)
	}
	
}

## move to file FLKMedoids.R
isolation.FLKMedoids <- function(object){
	if(!is.null(object@results[["isolation"]]))
	return(object@results[["isolation"]])
	else
	{
		connection <- getFLConnection(object@table)
		## flag3Check(connection)
		deeptablename <- getTableNameSlot(object@deeptable)
		obs_id_colname <- getIndexSQLExpression(object@deeptable,1)
		var_id_colname <- getIndexSQLExpression(object@deeptable,2)
		cell_val_colname <- getIndexSQLExpression(object@deeptable,3)
		a <- paste0(genRandVarName(),"1")
		b <- paste0(genRandVarName(),"2")
		c <- gen_unique_table_name("3")
		d <- gen_unique_table_name("4")
		e <- gen_unique_table_name("5")

		##Ensure required temptables exist
		if(is.null(object@temptables[["temptbl4"]]))
		{
            t <- createTable(e,pSelect=paste0("SELECT a.",obs_id_colname," AS ObsID, \n a.",var_id_colname,
                                                        " AS VarID, \n a.",cell_val_colname," AS Num_Val, \n b.MedoidID  \n ",
                                                " FROM  \n ",deeptablename," a, \n ",
                                                        "fzzlKMedoidsCluster b  \n ",
                                                " WHERE a.",obs_id_colname,"=b.ObsID and  \n b.AnalysisID='",
                                                        object@AnalysisID,"' "))
			# t <- sqlSendUpdate(connection,paste0(" create table ",e,
			# 					" as  \n (SELECT a.",obs_id_colname," AS ObsID, \n a.",var_id_colname,
			# 						" AS VarID, \n a.",cell_val_colname," AS Num_Val, \n b.MedoidID  \n ",
			# 						" FROM  \n ",deeptablename," a, \n ",
			# 								"fzzlKMedoidsCluster b  \n ",
			# 						" WHERE a.",obs_id_colname,"=b.ObsID and  \n b.AnalysisID='",
			# 						object@AnalysisID,"' )WITH DATA"))
			# if(length(t)>1) stop(t)
			object@temptables <- c(object@temptables,list(temptbl4=e))
		}
		if(is.null(object@temptables[["temptbl1"]]))
		{
			# t <- sqlSendUpdate(connection,paste0(" create table ",c,
			# 					" as \n (SELECT a.MedoidID as MedoidIDX, \n b.MedoidID as MedoidIDY,",
			# 								"a.ObsID AS ObsIDX, \n ",
			# 								" b.ObsID AS ObsIDY, \n FLEuclideanDist(a.Num_Val, b.Num_Val) AS Dist \n ",
			# 					  " FROM ",object@temptables[["temptbl4"]]," a, \n ",
			# 					  			object@temptables[["temptbl4"]]," b \n ", 
			# 					 " WHERE a.VarID = b.VarID and a.MedoidID = b.MedoidID \n ",
			# 					 " GROUP BY 1,2,3,4) with data"))
			# if(length(t)>1) stop(t)
            t <- createTable(c,pSelect=paste0("SELECT a.MedoidID as MedoidIDX, \n b.MedoidID as MedoidIDY,",
                                                "a.ObsID AS ObsIDX, \n ",
                                                " b.ObsID AS ObsIDY, \n ",
                                                " FLEuclideanDist(a.Num_Val, b.Num_Val) AS Dist \n ",
                                            " FROM ",object@temptables[["temptbl4"]]," a, \n ",
                                                    object@temptables[["temptbl4"]]," b \n ", 
                                            " WHERE a.VarID = b.VarID and a.MedoidID = b.MedoidID \n ",
                                            " GROUP BY a.MedoidID,b.MedoidID,a.ObsID,b.ObsID"))
			object@temptables <- c(object@temptables,list(temptbl1=c))
		}
		if(is.null(object@temptables[["temptbl2"]]))
		{
			# t <- sqlSendUpdate(connection,paste0(" create table ",d,
			# 						" as \n (SELECT a.MedoidID as MedoidIDX, \n ",
			# 							" b.MedoidID as MedoidIDY,a.ObsID AS ObsIDX, \n ",
			# 							" b.ObsID AS ObsIDY, \n FLEuclideanDist(a.Num_Val, b.Num_Val) AS Dist \n ",
			# 					  " FROM ",object@temptables[["temptbl4"]]," a, \n ",
			# 					  			object@temptables[["temptbl4"]]," b \n ",
			# 					  " WHERE a.VarID = b.VarID and a.MedoidID <> b.MedoidID \n ",
			# 					  " GROUP BY 1,2,3,4) with data"))
			# if(length(t)>1) stop(t)
            t <- createTable(d,pSelect=paste0("SELECT a.MedoidID as MedoidIDX, \n ",
                                                    " b.MedoidID as MedoidIDY,a.ObsID AS ObsIDX, \n ",
                                                    " b.ObsID AS ObsIDY, \n FLEuclideanDist(a.Num_Val, b.Num_Val) AS Dist \n ",
                                                " FROM ",object@temptables[["temptbl4"]]," a, \n ",
                                                    object@temptables[["temptbl4"]]," b \n ",
                                                " WHERE a.VarID = b.VarID and a.MedoidID <> b.MedoidID \n ",
                                                " GROUP BY a.MedoidID,b.MedoidID,a.ObsID,b.ObsID"))
			object@temptables <- c(object@temptables,list(temptbl2=d))
		}

		temptbl1 <- object@temptables[["temptbl1"]]
		temptbl2 <- object@temptables[["temptbl2"]]

		sqlstr<-paste0("SELECT ROW_NUMBER() OVER(ORDER BY a.MedoidIDX) as MedoidID, \n ", 
							"  CASE WHEN (SUM(CASE WHEN a.maxim < b.minim THEN 0 ELSE 1 END)>0) THEN 0 ELSE 1 END AS isl \n ",
						" FROM(SELECT a.MedoidIDX,a.ObsIDX, FLMax(a.Dist) AS maxim \n ",
							" FROM ",temptbl1," a \n ",
							" group by a.MedoidIDX,a.ObsIDX) as a, \n ",
						 " (select a.MedoidIDX , a.ObsIDX, FLMin(a.Dist) as minim \n ", 
						 " FROM ",temptbl2," a \n ",
						 " GROUP BY a.MedoidIDX,a.ObsIDX) as b \n ",
						" WHERE a.ObsIDX=b.ObsIDX  \n AND a.MedoidIDX=b.MedoidIDX \n ",
						" GROUP BY a.MedoidIDX")

		isL <- sqlQuery(connection,sqlstr)[["isl"]]

		sqlstr <- paste0("SELECT ROW_NUMBER() OVER(ORDER BY a.MedoidIDX) AS MedoidID, \n ", 
							"	CASE WHEN (SUM(CASE WHEN a.diameter<b.separation",
							" \n  THEN 0 ELSE 1 END)>0) THEN 0 ELSE 1 END AS islstar  \n ",
						" FROM(SELECT a.MedoidIDX , FLMax(a.Dist) AS diameter \n ", 
							" FROM ",temptbl1," a \n ",
							" GROUP BY a.MedoidIDX) as a, \n ",
							" (SELECT a.MedoidIDX , FLMin(a.Dist) AS separation \n ",  
							" FROM ",temptbl2," a \n ",
						" GROUP BY a.MedoidIDX) as b \n ",
						" WHERE  a.MedoidIDX=b.MedoidIDX \n ",
						" GROUP BY a.MedoidIDX")

		isLstar <- sqlQuery(connection,sqlstr)[["islstar"]]

		isolationvector <- isL + isLstar
		isolationvector <- factor(isolationvector,levels=c(0,1,2),labels=c("no","L","L*"))
		names(isolationvector) <- 1:object@centers
		object@results <- c(object@results,list(isolation = isolationvector))

		##Drop temptables created if all components have already used them
		if(!is.null(object@results[["clusinfo"]]))
		{
            t <- dropTable(object@temptables[["temptbl1"]])
			# t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["temptbl1"]]))
			object@temptables[["temptbl1"]] <- NULL
		}
		if(!is.null(object@results[["clusinfo"]]) 
			&& is.matrix(object@results[["silinfo"]][["widths"]])
			&& is.numeric(object@results[["silinfo"]][["clus.avg.widths"]]))
		{
            t <- dropTable(object@temptables[["temptbl2"]])
			# t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["temptbl2"]]))
			object@temptables[["temptbl2"]] <- NULL
            t <- object@temptables[["temptbl4"]]
			# t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["temptbl4"]]))
			object@temptables[["temptbl4"]] <- NULL
		}

		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
                                "(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(isolationvector)
	}
}

## move to file FLKMedoids.R
clusinfo.FLKMedoids <- function(object){
	if(!is.null(object@results[["clusinfo"]]))
	return(object@results[["clusinfo"]])
	else
	{
		connection <- getFLConnection(object@table)
		## flag3Check(connection)
		deeptablename <- getTableNameSlot(object@deeptable)
		obs_id_colname <- getIndexSQLExpression(object@deeptable,1)
		var_id_colname <- getIndexSQLExpression(object@deeptable,2)
		cell_val_colname <- getIndexSQLExpression(object@deeptable,3)
		a <- paste0(genRandVarName(),"1")
		b <- paste0(genRandVarName(),"2")
		c <- gen_unique_table_name("3")
		d <- gen_unique_table_name("4")
		e <- gen_unique_table_name("5")

		##Ensure required temptables exist
		if(is.null(object@temptables[["temptbl4"]]))
		{
			# t <- sqlSendUpdate(connection,paste0(" create table ",e,
			# 						" \n  as (SELECT a.",obs_id_colname," AS ObsID, \n a.",
			# 							var_id_colname," AS VarID,a.",cell_val_colname,
			# 							" \n  AS Num_Val,b.MedoidID  \n ",
			# 						"  FROM ",deeptablename," a, \n fzzlKMedoidsCluster b  \n ",
			# 						" WHERE a.",obs_id_colname,"=b.ObsID and b.AnalysisID='",
			# 						object@AnalysisID,"' )WITH DATA"))
			# if(length(t)>1) stop(t)
            t <- createTable(e,pSelect=paste0("SELECT a.",obs_id_colname," AS ObsID, \n a.",
                                                        var_id_colname," AS VarID,a.",cell_val_colname,
                                                    " \n  AS Num_Val,b.MedoidID  \n ",
                                            "  FROM ",deeptablename," a, \n fzzlKMedoidsCluster b  \n ",
                                            " WHERE a.",obs_id_colname,"=b.ObsID and b.AnalysisID='",
                                                        object@AnalysisID,"' "))
			object@temptables <- c(object@temptables,list(temptbl4=e))
		}
		if(is.null(object@temptables[["temptbl1"]]))
		{
			# t <- sqlSendUpdate(connection,paste0(" create table ",c,
			# 					" \n  as (SELECT a.MedoidID as MedoidIDX, \n ",
			# 						" b.MedoidID as MedoidIDY,a.ObsID AS ObsIDX, \n ",
			# 						" b.ObsID AS ObsIDY,FLEuclideanDist(a.Num_Val, b.Num_Val) AS Dist \n ",
			# 					" FROM ",object@temptables[["temptbl4"]]," a, \n ",
			# 							object@temptables[["temptbl4"]]," b \n ",
			# 					" WHERE a.VarID = b.VarID and  \n ",
			# 						" a.MedoidID = b.MedoidID \n ",
			# 					" GROUP BY 1,2,3,4) with data"))
			# if(length(t)>1) stop(t)
            t <- createTable(c,pSelect=paste0("SELECT a.MedoidID as MedoidIDX, \n ",
                                                    " b.MedoidID as MedoidIDY,a.ObsID AS ObsIDX, \n ",
                                                    " b.ObsID AS ObsIDY,FLEuclideanDist(a.Num_Val, b.Num_Val) AS Dist \n ",
                                                " FROM ",object@temptables[["temptbl4"]]," a, \n ",
                                                        object@temptables[["temptbl4"]]," b \n ",
                                                " WHERE a.VarID = b.VarID and  \n ",
                                                        " a.MedoidID = b.MedoidID \n ",
                                                " GROUP BY a.MedoidID,b.MedoidID,b.ObsID,a.ObsID"))
			object@temptables <- c(object@temptables,list(temptbl1=c))
		}
		if(is.null(object@temptables[["temptbl2"]]))
		{
			# t <- sqlSendUpdate(connection,paste0(" create table ",d,
			# 					" \n as (SELECT a.MedoidID as MedoidIDX, \n ",
			# 								" b.MedoidID as MedoidIDY,a.ObsID AS ObsIDX, \n ",
			# 							" b.ObsID AS ObsIDY, \n FLEuclideanDist(a.Num_Val, b.Num_Val) AS Dist \n ",
			# 					" FROM ",object@temptables[["temptbl4"]]," a, \n ",
			# 							object@temptables[["temptbl4"]]," b \n ",
			# 					" WHERE a.VarID = b.VarID  \n and a.MedoidID <> b.MedoidID \n ",
			# 					" GROUP BY 1,2,3,4) with data"))
			# if(length(t)>1) stop(t)
            t <- createTable(d,pSelect=paste0("SELECT a.MedoidID as MedoidIDX, \n ",
                                                    " b.MedoidID as MedoidIDY,a.ObsID AS ObsIDX, \n ",
                                                    " b.ObsID AS ObsIDY, \n FLEuclideanDist(a.Num_Val, b.Num_Val) AS Dist \n ",
                                            " FROM ",object@temptables[["temptbl4"]]," a, \n ",
                                                    object@temptables[["temptbl4"]]," b \n ",
                                            " WHERE a.VarID = b.VarID  \n and a.MedoidID <> b.MedoidID \n ",
                                            " GROUP BY a.MedoidID,b.MedoidID,a.ObsID,b.ObsID"))
			object@temptables <- c(object@temptables,list(temptbl2=d))
		}

		temptbl1 <- object@temptables[["temptbl1"]]
		temptbl2 <- object@temptables[["temptbl2"]]

		sqlstr<-paste0("SELECT ROW_NUMBER() OVER(ORDER BY a.MedoidIDX) as medoidid, \n c.sizes as sizes, \n ", 
							" d.maxDiss as max_Diss , \n  d.avgDiss as avg_Diss, \n ",
							" a.diameter as diameter, \n b.separation as separation \n ", 
						" FROM (SELECT a.MedoidIDX , FLMax(a.Dist) as diameter \n ", 
							" FROM ",temptbl1," a \n ",
							" GROUP BY a.MedoidIDX) as a, \n ",
							" (SELECT a.MedoidIDX , FLMin(a.Dist) as separation \n ",  
							" FROM ",temptbl2," a \n ",
							" GROUP BY a.MedoidIDX) as b, \n ",
							" (SELECT a.MedoidID as MedoidIDX, \n ",
								" COUNT(a.ObsID) as sizes from fzzlKMedoidsCluster a \n ", 
							" WHERE a.AnalysisID='",object@AnalysisID,"' \n GROUP BY a.MedoidID) as c, \n ",
							" (SELECT a.MedoidID as MedoidIDX, \n FLMax(a.Dist) as maxDiss, \n ",
							" FLMean(a.Dist) as avgDiss  \n ",
							" FROM(SELECT b.MedoidID, \n a.",obs_id_colname," AS ObsIDX, \n c.",
										obs_id_colname," AS ObsIDY, \n FLEuclideanDist(a.",cell_val_colname,
											", \n  c.",cell_val_colname,") AS Dist \n ",
									" FROM ",deeptablename," a, fzzlKMedoidsCluster b,",deeptablename," c \n ",
									" WHERE a.",var_id_colname," = c.",var_id_colname,
											" \n  and  b.MedoidID=c.",obs_id_colname,
									  		" \n  and b.ObsID=a.",obs_id_colname,
									  		" \n  and b.AnalysisID='",object@AnalysisID,"' \n ", 
							         " GROUP BY b.MedoidID,a.",obs_id_colname,",c.",obs_id_colname,") AS a \n ",
							" GROUP BY a.MedoidID) as d \n ",
						" WHERE  a.MedoidIDX=b.MedoidIDX and \n ",
								" a.MedoidIDX=c.MedoidIDX and  \n ",
								" a.MedoidIDX=d.MedoidIDX")

		clusinfoDataFrame <- sqlQuery(connection,sqlstr)
		clusinfoDataFrame$medoidid <- NULL
		clusinfomatrix <- as.matrix(clusinfoDataFrame)

		
		object@results <- c(object@results,list(clusinfo = clusinfomatrix))
		##Drop temptables created if all components have already used them
		if(!is.null(object@results[["isolation"]]))
		{
            t <- dropTable(object@temptables[["temptbl1"]])
			# t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["temptbl1"]]))
			object@temptables[["temptbl1"]] <- NULL
		}
		if(!is.null(object@results[["isolation"]]) 
			&& is.matrix(object@results[["silinfo"]][["widths"]])
			&& is.numeric(object@results[["silinfo"]][["clus.avg.widths"]]))
		{
            t <- dropTable(object@temptables[["temptbl2"]])
			# t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["temptbl2"]]))
			object@temptables[["temptbl2"]] <- NULL
            t <- dropTable(object@temptables[["temptbl4"]])
			# t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["temptbl4"]]))
			object@temptables[["temptbl4"]] <- NULL
		}
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(clusinfomatrix)
	}
}

## move to file FLKMedoids.R
silinfo.FLKMedoids <- function(object){
	if(!is.null(object@results[["silinfo"]]))
	return(object@results[["silinfo"]])
	else
	{
		connection <- getFLConnection(object@table)
		## flag3Check(connection)
		deeptablename <- getTableNameSlot(object@deeptable)
		obs_id_colname <- getIndexSQLExpression(object@deeptable,1)
		var_id_colname <- getIndexSQLExpression(object@deeptable,2)
		cell_val_colname <- getIndexSQLExpression(object@deeptable,3)
		a <- paste0(genRandVarName(),"1")
		b <- paste0(genRandVarName(),"2")
		c <- gen_unique_table_name("3")
		d <- gen_unique_table_name("4")
		e <- gen_unique_table_name("5")

		##Ensure required temptables exist
		if(is.null(object@temptables[["temptbl4"]]))
		{
			# t <- sqlSendUpdate(connection,paste0(" create table ",e,
			# 						" as  \n (SELECT a.",obs_id_colname," AS ObsID, \n a.",
			# 							var_id_colname," AS VarID, \n a.",cell_val_colname,
			# 							" AS Num_Val, \n b.MedoidID \n ", 
			# 						  " FROM ",deeptablename," a,fzzlKMedoidsCluster b  \n ",
			# 						  " WHERE a.",obs_id_colname,"=b.ObsID  \n and b.AnalysisID='",
			# 						  object@AnalysisID,"' )WITH DATA"))
			# if(length(t)>1) stop(t)
            t <- createTable(e,pSelect=paste0("SELECT a.",obs_id_colname," AS ObsID, \n a.",
                                                        var_id_colname," AS VarID,a.",cell_val_colname,
                                                    " \n  AS Num_Val,b.MedoidID  \n ",
                                            "  FROM ",deeptablename," a, \n fzzlKMedoidsCluster b  \n ",
                                            " WHERE a.",obs_id_colname,"=b.ObsID and b.AnalysisID='",
                                                        object@AnalysisID,"' "))
			object@temptables <- c(object@temptables,list(temptbl4=e))
		}
		if(is.null(object@temptables[["temptbl3"]]))
		{
			# t <- sqlSendUpdate(connection,paste0(" create table ",c,
			# 								" as (SELECT a.MedoidID as MedoidIDX, \n a.ObsID AS ObsIDX, \n ",
			# 								" b.ObsID AS ObsIDY, \n FLEuclideanDist(a.Num_Val, b.Num_Val) AS Dist \n ",
			# 						" FROM ",object@temptables[["temptbl4"]]," a, \n ",
			# 								object@temptables[["temptbl4"]]," b \n ",
			# 						" WHERE a.VarID = b.VarID  \n and a.MedoidID = b.MedoidID \n ",
			# 						" GROUP BY 1,2,3) with data"))
			# if(length(t)>1) stop(t)
            t <- createTable(c,pSelect=paste0("SELECT a.MedoidID as MedoidIDX, \n a.ObsID AS ObsIDX, \n ",
                                                    " b.ObsID AS ObsIDY, \n FLEuclideanDist(a.Num_Val, b.Num_Val) AS Dist \n ",
                                            " FROM ",object@temptables[["temptbl4"]]," a, \n ",
                                                    object@temptables[["temptbl4"]]," b \n ",
                                            " WHERE a.VarID = b.VarID  \n and a.MedoidID = b.MedoidID \n ",
                                            " GROUP BY a.MedoidID,a.ObsID,b.ObsID"))
			object@temptables <- c(object@temptables,list(temptbl3=c))
		}
		if(is.null(object@temptables[["temptbl2"]]))
		{
			# t <- sqlSendUpdate(connection,paste0(" create table ",d,
			# 						" as  \n (SELECT a.MedoidID as MedoidIDX, \n ",
			# 									" b.MedoidID as MedoidIDY, \n a.ObsID AS ObsIDX, \n ",
			# 							" b.ObsID AS ObsIDY, \n FLEuclideanDist(a.Num_Val, b.Num_Val) AS Dist \n ",
			# 					  " FROM ",object@temptables[["temptbl4"]]," a, \n ",
			# 					  			object@temptables[["temptbl4"]]," b \n ", 
			# 					  " WHERE a.VarID = b.VarID  \n and a.MedoidID <> b.MedoidID \n ",
			# 					  " GROUP BY 1,2,3,4) with data"))
			# if(length(t)>1) stop(t)
            t <- createTable(d,pSelect=paste0("SELECT a.MedoidID as MedoidIDX, \n ",
                                                    " b.MedoidID as MedoidIDY,a.ObsID AS ObsIDX, \n ",
                                                    " b.ObsID AS ObsIDY, \n FLEuclideanDist(a.Num_Val, b.Num_Val) AS Dist \n ",
                                                " FROM ",object@temptables[["temptbl4"]]," a, \n ",
                                                    object@temptables[["temptbl4"]]," b \n ",
                                                " WHERE a.VarID = b.VarID and a.MedoidID <> b.MedoidID \n ",
                                                " GROUP BY a.MedoidID,b.MedoidID,a.ObsID,b.ObsID"))
			object@temptables <- c(object@temptables,list(temptbl2=d))
		}

		temptbl2 <- object@temptables[["temptbl2"]]
		temptbl3 <- object@temptables[["temptbl3"]]

		sqlstr<-paste0("select a.ObsIDX as obs_id_colname, \n a.MedoidIDX AS MedoidID, \n ",
							" a.MedoidIDY AS neighbor, \n (a.num/a.den) as sil_width \n ", 
						" from(select a.MedoidIDX , \n b.MedoidIDY,a.ObsIDX ,((b.bi-a.ai)) as num, \n ",
								"	Case when a.ai>b.bi then a.ai else  b.bi end as den \n ", 
							" from(select a.MedoidIDX , a.ObsIDX, FLMean(a.Dist) as ai \n ", 
								" from ",temptbl3," a \n ",
								" group by a.MedoidIDX,a.ObsIDX) as a, \n ",
								" (select b.ObsIDX,b.MedoidIDX,c.MedoidIDY,b.bi \n ",  
								" from(select a.MedoidIDX, a.ObsIDX,min(a.di) as bi  \n ",
									" from(select a.MedoidIDX , a.MedoidIDY, \n ",
										"a.ObsIDX, cast(FLMean(a.Dist) as decimal(38,7)) as di \n ",  
										" from ",temptbl2," a \n ",
										" group by a.MedoidIDX,a.MedoidIDY,a.ObsIDX) as a \n ",
									" group by a.MedoidIDX,a.ObsIDX) as b, \n ",
								" (select a.MedoidIDX ,  \n a.MedoidIDY, \n a.ObsIDX, \n ",
									" cast(FLMean(a.Dist)as decimal(38,7)) as di \n ",  
								" from ",temptbl2," a \n ",
								" group by a.MedoidIDX,a.MedoidIDY,a.ObsIDX) as c \n ",
							" where c.ObsIDX=b.ObsIDX  \n ",
									" and c.MedoidIDX=b.MedoidIDX  \n ",
									" and b.bi=c.di) as b \n ",
						" where a.ObsIDX=b.ObsIDX \n  and a.MedoidIDX=b.MedoidIDX) as a")
		
		tblfunqueryobj <- new("FLTableFunctionQuery",
                        connectionName = attr(connection,"name"),
                        variables = list(
			                obs_id_colname = "obs_id_colname"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

		widthsFLTable <- newFLTable(
                             select = tblfunqueryobj,
                             dims=as.integer(c(nrow(object@deeptable), 4)),
							Dimnames = list(object@deeptable@Dimnames[[1]],
											c("obs_id_colname","MedoidID","neighbor","sil_width")),
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

		sqlstr <- paste0("select '%insertIDhere%' AS vectorIdColumn, \n ",
						" ROW_NUMBER() OVER(ORDER BY a.MedoidID) as vectorIndexColumn, \n ",
						" FLMean(a.sil_width) as vectorValueColumn \n ",
						" from(select a.MedoidIDX as MedoidID, \n ",
						" a.ObsIDX as ObsID,(a.num/a.den) as sil_width \n ", 
							"from(select a.MedoidIDX , \n a.ObsIDX , \n ((b.bi-a.ai)) as num, \n ",
							" Case when a.ai>b.bi then a.ai else  b.bi end as den \n ", 
								"from(select a.MedoidIDX , a.ObsIDX, FLMean(a.Dist) as ai \n ", 
									" from ",temptbl3," a \n ",
									" group by a.MedoidIDX,a.ObsIDX) as a, \n ",
								"(select a.MedoidIDX, a.ObsIDX,FLMin(a.di) as bi \n ", 
								"from(select a.MedoidIDX , a.MedoidIDY,a.ObsIDX, FLMean(a.Dist) as di \n ",  
									"from ",temptbl2," a \n ",
									" group by a.MedoidIDX,a.MedoidIDY,a.ObsIDX) as a \n ",
								" group by a.MedoidIDX,a.ObsIDX) as b \n ",
						" where a.ObsIDX=b.ObsIDX  \n and a.MedoidIDX=b.MedoidIDX) as a) as a \n ",
						" group by a.MedoidID")
		
		clus.avg.widthsvector <- tryCatch({
                                        vdf <- sqlQuery(connection,sqlstr)
                                        colnames(vdf) <- tolower(colnames(vdf))
                                        vdf[["vectorvaluecolumn"]]
                                        },
										 error=function(e){
										 	tblfunqueryobj <- new("FLTableFunctionQuery",
										                        connectionName = attr(connection,"name"),
										                        variables = list(
													                obs_id_colname = "vectorIndexColumn",
													                cell_val_colname = "vectorValueColumn"),
										                        whereconditions="",
										                        order = "",
										                        SQLquery=sqlstr)

											t <- newFLVector(
													select = tblfunqueryobj,
													Dimnames = list(1:object@centers,
																	"vectorValueColumn"),
													isDeep = FALSE)
											store(t)
										 })

		if(is.FLTable(widthsmatrix))
		{
			sqlstr <- paste0("select FLMean(a.sil_width) as avg_sil_width \n ",
							" from(select a.MedoidIDX as MedoidID, \n ",
									" a.ObsIDX as ObsID,(a.num/a.den) as sil_width \n ",
								"from(select a.MedoidIDX , \n ",
									"a.ObsIDX ,((b.bi-a.ai)) as num, \n ",
									" Case when a.ai>b.bi then a.ai else  b.bi end as den \n ", 
									" from(select a.MedoidIDX , a.ObsIDX, FLMean(a.Dist) as ai \n ", 
										" from ",temptbl3," a \n ",
										" group by a.MedoidIDX,a.ObsIDX) as a, \n ",
										" (select a.MedoidIDX, a.ObsIDX,FLMin(a.di) as bi \n ",
										" from(select a.MedoidIDX , a.MedoidIDY,a.ObsIDX, FLMean(a.Dist) as di \n ",  
											" from ",temptbl2," a \n ",
											" group by a.MedoidIDX,a.MedoidIDY,a.ObsIDX) as a \n ",
										" group by a.MedoidIDX,a.ObsIDX) as b \n ",
							" where a.ObsIDX=b.ObsIDX  \n and a.MedoidIDX=b.MedoidIDX) as a) as a")
			
			avg.widthvector <- sqlQuery(connection,sqlstr)[["avg_sil_width"]]
		}
		else
		avg.widthvector <- tryCatch(base::mean(widthsmatrix[,"sil_width"]),
									error=function(e) base::mean(widthsmatrix[,"SIL_WIDTH"]))
		
		silinfolist <- list(widths=widthsmatrix,
							clus.avg.widths=clus.avg.widthsvector,
							avg.width=avg.widthvector)

		
		object@results <- c(object@results,list(silinfo = silinfolist))
		##Drop temptables created if all components have already used them
		if((!(class(widthsmatrix)=="FLTable")) && (!(class(clus.avg.widthsvector)=="FLVector")))
		{
			if(!is.null(object@results[["clusinfo"]]) && !is.null(object@results[["isolation"]]))
			{
                t <- dropTable(object@temptables[["temptbl2"]])
				# t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["temptbl2"]]))
				object@temptables[["temptbl2"]] <- NULL
                t <- dropTable(object@temptables[["temptbl4"]])
				# t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["temptbl4"]]))
				object@temptables[["temptbl4"]] <- NULL
			}
            t <- dropTable(object@temptables[["temptbl3"]])
			# t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["temptbl3"]]))
			object@temptables[["temptbl3"]] <- NULL
		}

		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
                            "(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(silinfolist)
	}
}

## move to file FLKMedoids.R
diss.FLKMedoids<-function(object)
{
	if(!is.null(object@results[["diss"]]))
	return(object@results[["diss"]])
	else
	{
		connection <- getFLConnection(object@table)
		## flag1Check(connection)
		AnalysisID <- object@AnalysisID
		deeptablename <- getTableNameSlot(object@deeptable)
		obs_id_colname <- getIndexSQLExpression(object@deeptable,1)
		var_id_colname <- getIndexSQLExpression(object@deeptable,2)
		cell_val_colname <- getIndexSQLExpression(object@deeptable,3)
		if(object@diss && object@distTable!="NULL")
		{
			sqlstr<-paste0("SELECT '%insertIDhere%' as MATRIX_ID, \n ",
									"a.ObsIDX AS rowIdColumn, \n ",
									"a.ObsIDY AS colIdColumn, \n ",
									"a.Dist AS valueColumn \n ",
							" FROM ",object@distTable," a")
		}
		else
		{

			sqlstr<-paste0("SELECT '%insertIDhere%' as MATRIX_ID, \n ", 
									"a.",obs_id_colname," AS rowIdColumn, \n ",
									"b.",obs_id_colname," AS colIdColumn, \n ",
									"FLEuclideanDist(a.",cell_val_colname,
												", b.",cell_val_colname,") AS valueColumn \n ",
							" FROM ",deeptablename," a, \n ",deeptablename," b \n ",
							" WHERE a.",var_id_colname," = b.",var_id_colname,
								" \n  and a.",obs_id_colname,">b.",obs_id_colname, 
							"  \n GROUP BY a.",obs_id_colname,",b.",obs_id_colname)

		}

			tblfunqueryobj <- new("FLTableFunctionQuery",
	                        connectionName = attr(connection,"name"),
	                        variables=list(
	                            rowIdColumn="rowIdColumn",
	                            colIdColumn="colIdColumn",
	                            valueColumn="valueColumn"),
	                        whereconditions="",
	                        order = "",
	                        SQLquery=sqlstr)

		  	dissmatrix <- newFLMatrix(
					            select= tblfunqueryobj,
					            dims=as.integer(c(length(object@deeptable@Dimnames[[1]]),
					            	length(object@deeptable@Dimnames[[1]]))),
					            Dimnames=list(object@deeptable@Dimnames[[1]],
					            			object@deeptable@Dimnames[[1]]))

		  	dissmatrix <- tryCatch(as.sparseMatrix.FLMatrix(dissmatrix),
		  							error=function(e){
		  								if(object@diss)
		  								cat("Error:-The distTable schema is incorrect. Refer DBLytix manual")
		  								return(dissmatrix)
		  								})
		
		object@results <- c(object@results,list(diss = dissmatrix))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
                            "(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(dissmatrix)
	}
}

## move to file FLKMedoids.R
call.FLKMedoids<-function(object)
{
	if(!is.null(object@results[["call"]]))
	return(object@results[["call"]])
	else
	{
		callobject <- base::call("pam",x=object@table,k=object@centers)
		
		object@results <- c(object@results,list(call = callobject))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(callobject)
	}
}

## move to file FLKMedoids.R
data.FLKMedoids<-function(object)
{
	if(!is.null(object@results[["data"]]))
	return(object@results[["data"]])
	else if(object@diss==TRUE) dataframe <- c()
	else
	{
		deeptablename <- getTableNameSlot(object@deeptable)
		obs_id_colname <- getIndexSQLExpression(object@deeptable,1)
		var_id_colname <- getIndexSQLExpression(object@deeptable,2)
		cell_val_colname <- getIndexSQLExpression(object@deeptable,3)
		widetable <- gen_wide_table_name("new")
		
		x <- object@deeptable
		x <- as.data.frame(x)
		x$obs_id_colname <- NULL
		dataframe <- x
	}
	object@results <- c(object@results,list(data = dataframe))
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
                        "(",fixed=T))[2],")",fixed=T))[1]
	assign(parentObject,object,envir=parent.frame())
	return(dataframe)
}

## move to file FLKMedoids.R
#' @export
print.FLKMedoids <- function(object)
{
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
                        "(",fixed=T))[2],")",fixed=T))[1]
	results <- list()
	results <- c(results,list(medoids=medoids.FLKMedoids(object)),
						list(id.med=id.med.FLKMedoids(object)),
						list(clustering=clustering.FLKMedoids(object)),
						list(objective=objective.FLKMedoids(object)),
						list(isolation=""),
						list(clusinfo=""),
						list(silinfo=""),
						list(diss=""),
						list(call=call.FLKMedoids(object)),
						list(data=""))
	class(results) <- c("pam","partition","silhouette")

	assign(parentObject,object,envir=parent.frame())
	print(results)
}

## move to file FLKMedoids.R
#' @export
setMethod("show","FLKMedoids",
			function(object)
			{
				parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
                                    "(",fixed=T))[2],")",fixed=T))[1]
				print(object)
				assign(parentObject,object,envir=parent.frame())
			}
		 )

## move to file FLKMedoids.R
#' @export
plot.FLKMedoids <- function(object)
{
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
                        "(",fixed=T))[2],")",fixed=T))[1]
	results <- list()
	dataframe <- data.FLKMedoids(object)
	if(is.null(dataframe) || length(dataframe)==0)
	l <- list(diss=as.matrix(diss.FLKMedoids(object)))
	else
	l <- list(data=dataframe)
	results <- c(results,#list(medoids=medoids(object)),
						#list(id.med=id.med(object)),
						list(clustering=clustering.FLKMedoids(object)),
						list(objective=objective.FLKMedoids(object)),
						#list(isolation=isolation(object)),
						#list(clusinfo=clusinfo(object)),
						list(silinfo=silinfo.FLKMedoids(object)),
						#list(diss=diss(object)),
						list(call=call.FLKMedoids(object)),
						l
						)
	class(results) <- c("pam","partition","silhouette")

	assign(parentObject,object,envir=parent.frame())
	plot(results)
}

## move to file FLKMedoids.R
FLMapping.FLKMedoids <- function(object)
{
	if(!is.null(object@results[["mapping"]]))
	return(object@results[["mapping"]])
	else
	{
		if(object@mapTable!="")
		{
			sqlstr <- paste0("SELECT * FROM ",object@mapTable)
			mapdataframe <- sqlQuery(getFLConnection(),sqlstr)
			if((is.vector(mapdataframe) && length(mapdataframe)==2) || is.null(mapdataframe))
			mapdataframe <- paste0("The mapping table in database is",object@mapTable)
			else if(is.data.frame(mapdataframe))
            t <- dropTable(object@mapTable)
			# t <- sqlSendUpdate(getFLConnection(),paste0(" DROP TABLE ",object@mapTable))
		}
		else mapdataframe <- ""
		
		object@results <- c(object@results,list(mapping = mapdataframe))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
                                "(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(mapdataframe)
	}
}

## move to file pam.R
#' @export
pam.FLMatrix <- function(x,
						k,
						diss=FALSE,
						metric="euclidean",##notUsed
						medoids=NULL,##notUsed
						Stand=FALSE,##notUsed
						cluster.only = FALSE,
    					do.swap = TRUE,##notUsed
    					keep.diss = (!diss && !cluster.only),
    					keep.data = (!diss && !cluster.only),
    					pamonce = FALSE,##notUsed
    					trace.lev = 0,##notUsed
    					iter.max =10,
						excludeCols = "",
						classSpec = list(),
						whereconditions = "",
						distTable=""
						)
{
	x <- as.FLTable(x)
	vcall <- match.call()
	pamobj <- (pam (x=x,
				k=k,
				diss=diss,
				metric=metric,##notUsed
				medoids=medoids,##notUsed
				Stand=Stand,##notUsed
				cluster.only = cluster.only,
				do.swap = do.swap,##notUsed
				keep.diss = keep.diss,
				keep.data = keep.data,
				pamonce =pamonce,##notUsed
				trace.lev = trace.lev,##notUsed
				iter.max =iter.max,
				excludeCols = excludeCols,
				classSpec = classSpec,
				whereconditions = whereconditions,
				distTable=distTable))
	pamobj@results$call <- vcall
	return(pamobj)
}

