#' @include utilities.R
#' @include FLTable.R
#' @include FLMatrix.R
#' @include FLVector.R
NULL
#' An S4 class to represent FLKMedoids
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
	"FLKMedoids",
	slots=list(
		centers="numeric",
		AnalysisID="character",
		wideToDeepAnalysisId="character",
		table="FLTable",
		diss="logical",
		results ="list",
		deeptable="FLTable",
		temptables="list",
		mapTable="character"
	)
)
pam <- function (x,k,...) {
  UseMethod("pam", x)
}

pam.data.frame<-cluster::pam
pam.matrix <- cluster::pam
pam.default <- cluster::pam

call <- function (x,k,...) {
  UseMethod("call", x)
}
call.default <- base::call

#' K-Medoids Clustering.
#'
#' \code{pam} performs k-medoids clustering on FLTable objects.
#'
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
#' @param exclude the comma separated character string of columns to be excluded
#' @param class_spec list describing the categorical dummy variables
#' @param whereconditions takes the where_clause as a string
#' @param distTable name of the in-database table having dissimilarity
#' matrix or distance table
#' @section Constraints:
#' Plotting time increases with size of data and even fail for large
#' datasets because data is fetched to R session for plotting
#' @return \code{pam} gives a list which replicates equivalent R output
#' from \code{pam} in cluster package
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' widetable  <- FLTable("FL_DEMO", "iris", "rownames")
#' kmedoidsobject <- pam(widetable,3)
#' print(kmedoidsobject)
#' plot(kmedoidsobject)
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
		deeptablename <- genRandVarName()
		sqlstr <- paste0("CREATE VIEW ",getOption("ResultDatabaseFL"),".",deeptablename," AS ",constructSelect(x))
		sqlSendUpdate(connection,sqlstr)

		deeptablename1 <- gen_deep_table_name("New")
		sqlstr <- paste0("CREATE VIEW ",getOption("ResultDatabaseFL"),".",deeptablename1,
			" AS SELECT * FROM ",getOption("ResultDatabaseFL"),".",deeptablename,constructWhere(whereconditions))
		t <- sqlQuery(connection,sqlstr)
		if(length(t)>1) stop("Input Table and whereconditions mismatch,Error:",t)

		deepx <- FLTable(connection,
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
		deepx <- FLTable(connection,
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
		if(distTable=="") stop("Since diss=TRUE, provide distTable input in the form of database.table")
		else distTable <- paste0("'",distTable,"'")
	}
	else distTable <- "NULL"

    sqlstr <- paste("CALL FLKMedoids( '",deeptable,"',
			 					   '",getVariables(deepx)[["obs_id_colname"]],"',
			 					   '",getVariables(deepx)[["var_id_colname"]],"',
			 					   '",getVariables(deepx)[["cell_val_colname"]],"',",
			 					   whereClause,",",
			 					   k,",",
			 					   iter.max,",",
			 					   distTable,",
			 					   'KMedoids with clusters=",k,"from AdapteR',
			 					   AnalysisID );")
	
	retobj <- sqlQuery(connection,sqlstr)
	if(length(retobj)>1) stop(retobj)
	AnalysisID <- as.character(retobj[1,1])

	## medoid points are necessary for calculating some results
	sqlstr0 <- paste0("INSERT INTO fzzlKMedoidsCluster 
						SELECT AnalysisID,MedoidID,MedoidID 
						FROM fzzlKMedoidsMedoidIDs 
						WHERE AnalysisID='",AnalysisID,"' ")
	sqlSendUpdate(connection,sqlstr0)
	
	FLKMedoidsobject <- new("FLKMedoids",
						centers=k,
						AnalysisID=AnalysisID,
						wideToDeepAnalysisId=wideToDeepAnalysisId,
						table=x,
						results=list(),
						deeptable=deepx,
						diss=diss,
						temptables=list(),
						mapTable=mapTable)
	if(cluster.only)
	return(FLKMedoidsobject$clustering)
	else return(FLKMedoidsobject)
}

`$.FLKMedoids`<-function(object,property)
{
	#parentObject <- deparse(substitute(object))
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

	if(property=="medoids")
	{
		medoidsmatrix <- medoids(object)
		assign(parentObject,object,envir=parent.frame())
		return(medoidsmatrix)
	}
	else if(property=="id.med")
	{
		id.medvector <- id.med(object)
		assign(parentObject,object,envir=parent.frame())
		return(id.medvector)
	}
	else if(property=="clustering")
	{
		clusteringvector <- clustering(object)
		assign(parentObject,object,envir=parent.frame())
		return(clusteringvector)
	}
	else if(property=="objective")
	{
		objectivevector <- objective(object)
		assign(parentObject,object,envir=parent.frame())
		return(objectivevector)
	}
	else if(property=="isolation")
	{
		isolationvector <- isolation(object)
		assign(parentObject,object,envir=parent.frame())
		return(isolationvector)
	}
	else if(property=="clusinfo")
	{
		clusinfomatrix <- clusinfo(object)
		assign(parentObject,object,envir=parent.frame())
		return(clusinfomatrix)
	}
	else if(property=="silinfo")
	{
		silinfolist <- silinfo(object)
		assign(parentObject,object,envir=parent.frame())
		return(silinfolist)
	}
	else if(property=="diss")
	{
		dissmatrix <- diss(object)
		assign(parentObject,object,envir=parent.frame())
		return(dissmatrix)
	}
	else if(property=="call")
	{
		callobject <- call(object)
		assign(parentObject,object,envir=parent.frame())
		return(callobject)
	}
	else if(property=="data")
	{
		dataframe <- data.FLKMedoids(object)
		assign(parentObject,object,envir=parent.frame())
		return(dataframe)
	}
	else stop(property," is not a valid property")
}

clustering <- function (x, ...) {
   UseMethod("clustering", x)
 }

clustering.FLKMedoids <- function(object)
{
	if(!is.null(object@results[["clustering"]]))
	return(object@results[["clustering"]])
	else
	{
		connection <- getConnection(object@table)
		flag3Check(connection)
		AnalysisID <- object@AnalysisID
		sqlstr<-paste0("SELECT '%insertIDhere%' AS vectorIdColumn,
								 a.ObsID AS vectorIndexColumn, 
						         b.newMedoidID AS vectorValueColumn  
						FROM fzzlKMedoidsCluster a,
						(SELECT z.MedoidID as MedoidID,
							ROW_NUMBER() OVER(ORDER BY z.MedoidID) AS newMedoidID
						FROM fzzlKMedoidsMedoidIDs z 
						WHERE z.AnalysisID = '",AnalysisID,"') AS b 
						WHERE a.AnalysisID = '",AnalysisID,
						"' AND a.MedoidID=b.MedoidID")

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

medoids <- function (x, ...)
{
   UseMethod("medoids", x)
}

medoids.FLKMedoids<-function(object)
{
	if(!is.null(object@results[["medoids"]]))
	return(object@results[["medoids"]])
	else
	{
		if(object@diss)
		medoidsmatrix <- id.med(object)
		else
		{
			connection <- getConnection(object@table)
			flag1Check(connection)
			AnalysisID <- object@AnalysisID
			deeptablename <- paste0(object@deeptable@select@database,".",object@deeptable@select@table_name)
			obs_id_colname <- getVariables(object@deeptable)[["obs_id_colname"]]
			var_id_colname <- getVariables(object@deeptable)[["var_id_colname"]]
			cell_val_colname <- getVariables(object@deeptable)[["cell_val_colname"]]

			sqlstr<-paste0("SELECT '%insertIDhere%' AS MATRIX_ID, 
						         b.newMedoidID AS rowIdColumn,
						         a.",var_id_colname," AS colIdColumn,
						         a.",cell_val_colname," AS valueColumn 
							FROM ",deeptablename," AS a,
							(SELECT z.MedoidID as MedoidID,
								ROW_NUMBER() OVER(ORDER BY z.MedoidID) AS newMedoidID
							FROM fzzlKMedoidsMedoidIDs z 
							WHERE z.AnalysisID = '",AnalysisID,"') AS b 
							WHERE a.",obs_id_colname," = b.MedoidID")

			tblfunqueryobj <- new("FLTableFunctionQuery",
	                        connection = connection,
	                        variables=list(
	                            rowIdColumn="rowIdColumn",
	                            colIdColumn="colIdColumn",
	                            valueColumn="valueColumn"),
	                        whereconditions="",
	                        order = "",
	                        SQLquery=sqlstr)

		  	medoidsmatrix <- new("FLMatrix",
					            select= tblfunqueryobj,
					            dim=c(object@centers,
					            	length(object@deeptable@dimnames[[2]])),
					            dimnames=list(1:object@centers,
					            			object@deeptable@dimnames[[2]]))
		}
		if(class(medoidsmatrix)=="FLMatrix")
		medoidsmatrix <- tryCatch(as.matrix(medoidsmatrix),
      						error=function(e){medoidsmatrix})
		else
		medoidsmatrix <- tryCatch(as.vector(medoidsmatrix),
      						error=function(e){medoidsmatrix})
		
		object@results <- c(object@results,list(medoids = medoidsmatrix))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(medoidsmatrix)
	}
}

id.med <- function(object,...){
	UseMethod("id.med",object)
}

id.med.FLKMedoids<-function(object){
	if(!is.null(object@results[["id.med"]]))
	return(object@results[["id.med"]])
	else
	{
		a <- genRandVarName()
		connection <- getConnection(object@table)
		flag3Check(connection)
		sqlstr<-paste0("SELECT '%insertIDhere%' AS vectorIdColumn, 
						         ROW_NUMBER() OVER(ORDER BY ",a,".MedoidID) AS vectorIndexColumn,",
						         a,".MedoidID AS vectorValueColumn 
						FROM fzzlKMedoidsMedoidIDs ",a,"
						WHERE ",a,".AnalysisID = '",object@AnalysisID,"'")

		tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = connection,
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

		id.medvector <- new("FLVector",
							select = tblfunqueryobj,
							dimnames = list(1:object@centers,
											"vectorValueColumn"),
							isDeep = FALSE)
		id.medvector <- tryCatch(as.vector(id.medvector),
      						error=function(e){id.medvector})

		object@results <- c(object@results,list(id.med = id.medvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(id.medvector)
	}
}

objective <- function(object){
	UseMethod("objective",object)
}

objective.FLKMedoids <- function(object){
	if(!is.null(object@results[["objective"]]))
	return(object@results[["objective"]])
	else
	{
		## DBLytix TotalCost is the average;R cost is absolute.
		## The idea is the same,i.e to see improvement from build to swap
		a <- genRandVarName()
		connection <- getConnection(object@table)
		flag3Check(connection)
		n <- length(object@deeptable@dimnames[[1]])

		sqlstr<-paste0("SELECT '%insertIDhere%' AS vectorIdColumn,",
						         a,".Iteration AS vectorIndexColumn,",
						         a,".TotalCost AS vectorValueColumn 
						FROM fzzlKMedoidsTotalCost ",a,"
						WHERE ",a,".AnalysisID = '",object@AnalysisID,"' 
						AND ",a,".Iteration < 3 
						ORDER BY ",a,".Iteration")

		objectivevector <- as.numeric(sqlQuery(connection,sqlstr)[[3]])
		names(objectivevector) <- c("build","swap")

		object@results <- c(object@results,list(objective = objectivevector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(objectivevector)
	}
	
}

isolation <- function(object){
	UseMethod("isolation",object)
}

isolation.FLKMedoids <- function(object){
	if(!is.null(object@results[["isolation"]]))
	return(object@results[["isolation"]])
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
		c <- paste0(getOption("ResultDatabaseFL"),".",genRandVarName(),"3")
		d <- paste0(getOption("ResultDatabaseFL"),".",genRandVarName(),"4")
		e <- paste0(getOption("ResultDatabaseFL"),".",genRandVarName(),"5")

		##Ensure required temptables exist
		if(is.null(object@temptables[["temptbl4"]]))
		{
			t <- sqlSendUpdate(connection,paste0(" create table ",e," as (SELECT a.",obs_id_colname," AS ObsID,a.",var_id_colname," AS VarID,a.",cell_val_colname," AS Num_Val,b.MedoidID 
									  FROM ",deeptablename," a,fzzlKMedoidsCluster b WHERE a.",obs_id_colname,"=b.ObsID and b.AnalysisID='",object@AnalysisID,"' )WITH DATA"))
			if(length(t)>1) stop(t)
			object@temptables <- c(object@temptables,list(temptbl4=e))
		}
		if(is.null(object@temptables[["temptbl1"]]))
		{
			t <- sqlSendUpdate(connection,paste0(" create table ",c," as (SELECT a.MedoidID as MedoidIDX,b.MedoidID as MedoidIDY,a.ObsID AS ObsIDX,
										b.ObsID AS ObsIDY,FLEuclideanDist(a.Num_Val, b.Num_Val) AS Dist
								  FROM ",object@temptables[["temptbl4"]]," a,",object@temptables[["temptbl4"]]," b 
								  WHERE a.VarID = b.VarID and a.MedoidID = b.MedoidID
								  GROUP BY 1,2,3,4) with data"))
			if(length(t)>1) stop(t)
			object@temptables <- c(object@temptables,list(temptbl1=c))
		}
		if(is.null(object@temptables[["temptbl2"]]))
		{
			t <- sqlSendUpdate(connection,paste0(" create table ",d," as (SELECT a.MedoidID as MedoidIDX,b.MedoidID as MedoidIDY,a.ObsID AS ObsIDX,
										b.ObsID AS ObsIDY,FLEuclideanDist(a.Num_Val, b.Num_Val) AS Dist
								  FROM ",object@temptables[["temptbl4"]]," a,",object@temptables[["temptbl4"]]," b 
								  WHERE a.VarID = b.VarID and a.MedoidID <> b.MedoidID
								  GROUP BY 1,2,3,4) with data"))
			if(length(t)>1) stop(t)
			object@temptables <- c(object@temptables,list(temptbl2=d))
		}

		temptbl1 <- object@temptables[["temptbl1"]]
		temptbl2 <- object@temptables[["temptbl2"]]

		sqlstr<-paste0("SELECT ROW_NUMBER() OVER(ORDER BY a.MedoidIDX) as MedoidID, 
							   CASE WHEN (SUM(CASE WHEN a.maxim < b.minim THEN 0 ELSE 1 END)>0) THEN 0 ELSE 1 END AS isL 
						FROM(SELECT a.MedoidIDX,a.ObsIDX, FLMax(a.Dist) AS maxim
							FROM ",temptbl1," a
							group by 1,2) as a,
						 (select a.MedoidIDX , a.ObsIDX, FLMin(a.Dist) as minim 
						 FROM ",temptbl2," a
						 GROUP BY 1,2) as b
						WHERE a.ObsIDX=b.ObsIDX AND a.MedoidIDX=b.MedoidIDX
						GROUP BY a.MedoidIDX")

		isL <- sqlQuery(connection,sqlstr)[["isL"]]

		sqlstr <- paste0("SELECT ROW_NUMBER() OVER(ORDER BY a.MedoidIDX) AS MedoidID, 
								CASE WHEN (SUM(CASE WHEN a.diameter<b.separation THEN 0 ELSE 1 END)>0) THEN 0 ELSE 1 END AS isLstar 
						FROM(SELECT a.MedoidIDX , FLMax(a.Dist) AS diameter 
							FROM ",temptbl1," a
							GROUP BY 1) as a,
							(SELECT a.MedoidIDX , FLMin(a.Dist) AS separation  
							FROM ",temptbl2," a
						GROUP BY 1) as b
						WHERE  a.MedoidIDX=b.MedoidIDX
						GROUP BY a.MedoidIDX")

		isLstar <- sqlQuery(connection,sqlstr)[["isLstar"]]

		isolationvector <- isL + isLstar
		isolationvector <- factor(isolationvector,levels=c(0,1,2),labels=c("no","L","L*"))
		names(isolationvector) <- 1:object@centers
		object@results <- c(object@results,list(isolation = isolationvector))

		##Drop temptables created if all components have already used them
		if(!is.null(object@results[["clusinfo"]]))
		{
			t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["temptbl1"]]))
			object@temptables[["temptbl1"]] <- NULL
		}
		if(!is.null(object@results[["clusinfo"]]) && !is.null(object@results[["silinfo"]]))
		{
			t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["temptbl2"]]))
			object@temptables[["temptbl2"]] <- NULL
			t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["temptbl4"]]))
			object@temptables[["temptbl4"]] <- NULL
		}

		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(isolationvector)
	}
}

clusinfo <- function(object){
	UseMethod("clusinfo",object)
}

clusinfo.FLKMedoids <- function(object){
	if(!is.null(object@results[["clusinfo"]]))
	return(object@results[["clusinfo"]])
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
		c <- paste0(getOption("ResultDatabaseFL"),".",genRandVarName(),"3")
		d <- paste0(getOption("ResultDatabaseFL"),".",genRandVarName(),"4")
		e <- paste0(getOption("ResultDatabaseFL"),".",genRandVarName(),"5")

		##Ensure required temptables exist
		if(is.null(object@temptables[["temptbl4"]]))
		{
			t <- sqlSendUpdate(connection,paste0(" create table ",e," as (SELECT a.",obs_id_colname," AS ObsID,a.",var_id_colname," AS VarID,a.",cell_val_colname," AS Num_Val,b.MedoidID 
									  FROM ",deeptablename," a,fzzlKMedoidsCluster b WHERE a.",obs_id_colname,"=b.ObsID and b.AnalysisID='",object@AnalysisID,"' )WITH DATA"))
			if(length(t)>1) stop(t)
			object@temptables <- c(object@temptables,list(temptbl4=e))
		}
		if(is.null(object@temptables[["temptbl1"]]))
		{
			t <- sqlSendUpdate(connection,paste0(" create table ",c," as (SELECT a.MedoidID as MedoidIDX,b.MedoidID as MedoidIDY,a.ObsID AS ObsIDX,
										b.ObsID AS ObsIDY,FLEuclideanDist(a.Num_Val, b.Num_Val) AS Dist
								  FROM ",object@temptables[["temptbl4"]]," a,",object@temptables[["temptbl4"]]," b 
								  WHERE a.VarID = b.VarID and a.MedoidID = b.MedoidID
								  GROUP BY 1,2,3,4) with data"))
			if(length(t)>1) stop(t)
			object@temptables <- c(object@temptables,list(temptbl1=c))
		}
		if(is.null(object@temptables[["temptbl2"]]))
		{
			t <- sqlSendUpdate(connection,paste0(" create table ",d," as (SELECT a.MedoidID as MedoidIDX,b.MedoidID as MedoidIDY,a.ObsID AS ObsIDX,
										b.ObsID AS ObsIDY,FLEuclideanDist(a.Num_Val, b.Num_Val) AS Dist
								  FROM ",object@temptables[["temptbl4"]]," a,",object@temptables[["temptbl4"]]," b 
								  WHERE a.VarID = b.VarID and a.MedoidID <> b.MedoidID
								  GROUP BY 1,2,3,4) with data"))
			if(length(t)>1) stop(t)
			object@temptables <- c(object@temptables,list(temptbl2=d))
		}

		temptbl1 <- object@temptables[["temptbl1"]]
		temptbl2 <- object@temptables[["temptbl2"]]

		sqlstr<-paste0("SELECT ROW_NUMBER() OVER(ORDER BY a.MedoidIDX) as MedoidID,c.sizes as sizes, 
								d.maxDiss as max_Diss , d.avgDiss as avg_Diss, a.diameter as diameter, b.separation as separation 
						FROM (SELECT a.MedoidIDX , FLMax(a.Dist) as diameter 
							 FROM ",temptbl1," a
							 GROUP BY 1) as a,
							(SELECT a.MedoidIDX , FLMin(a.Dist) as separation  
							FROM ",temptbl2," a
							GROUP BY 1) as b,
							(SELECT a.MedoidID as MedoidIDX, COUNT(a.ObsID) as sizes from fzzlKMedoidsCluster a 
							 WHERE a.AnalysisID='",object@AnalysisID,"' GROUP BY a.MedoidID) as c,
							(SELECT a.MedoidID as MedoidIDX, FLMax(a.Dist) as maxDiss, FLMean(a.Dist) as avgDiss 
							FROM(SELECT b.MedoidID,a.",obs_id_colname," AS ObsIDX,c.",obs_id_colname," AS ObsIDY,FLEuclideanDist(a.",cell_val_colname,", c.",cell_val_colname,") AS Dist
									  FROM ",deeptablename," a,fzzlKMedoidsCluster b,",deeptablename," c
									  WHERE a.",var_id_colname," = c.",var_id_colname," and  b.MedoidID=c.",obs_id_colname,
									  		" and b.ObsID=a.",obs_id_colname," and b.AnalysisID='",object@AnalysisID,"' 
							GROUP BY 1,2,3) AS a
							GROUP BY 1) as d
						WHERE  a.MedoidIDX=b.MedoidIDX and a.MedoidIDX=c.MedoidIDX and a.MedoidIDX=d.MedoidIDX")

		clusinfoDataFrame <- sqlQuery(connection,sqlstr)
		clusinfoDataFrame$MedoidID <- NULL
		clusinfomatrix <- as.matrix(clusinfoDataFrame)

		
		object@results <- c(object@results,list(clusinfo = clusinfomatrix))
		##Drop temptables created if all components have already used them
		if(!is.null(object@results[["isolation"]]))
		{
			t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["temptbl1"]]))
			object@temptables[["temptbl1"]] <- NULL
		}
		if(!is.null(object@results[["isolation"]]) && !is.null(object@results[["silinfo"]]))
		{
			t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["temptbl2"]]))
			object@temptables[["temptbl2"]] <- NULL
			t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["temptbl4"]]))
			object@temptables[["temptbl4"]] <- NULL
		}
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(clusinfomatrix)
	}
}

silinfo <- function(object){
	UseMethod("silinfo",object)
}

silinfo.FLKMedoids <- function(object){
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
		c <- paste0(getOption("ResultDatabaseFL"),".",genRandVarName(),"3")
		d <- paste0(getOption("ResultDatabaseFL"),".",genRandVarName(),"4")
		e <- paste0(getOption("ResultDatabaseFL"),".",genRandVarName(),"5")

		##Ensure required temptables exist
		if(is.null(object@temptables[["temptbl4"]]))
		{
			t <- sqlSendUpdate(connection,paste0(" create table ",e," as (SELECT a.",obs_id_colname," AS ObsID,a.",var_id_colname," AS VarID,a.",cell_val_colname," AS Num_Val,b.MedoidID 
									  FROM ",deeptablename," a,fzzlKMedoidsCluster b WHERE a.",obs_id_colname,"=b.ObsID and b.AnalysisID='",object@AnalysisID,"' )WITH DATA"))
			if(length(t)>1) stop(t)
			object@temptables <- c(object@temptables,list(temptbl4=e))
		}
		if(is.null(object@temptables[["temptbl3"]]))
		{
			t <- sqlSendUpdate(connection,paste0(" create table ",c," as (SELECT a.MedoidID as MedoidIDX,a.ObsID AS ObsIDX,
											b.ObsID AS ObsIDY,FLEuclideanDist(a.Num_Val, b.Num_Val) AS Dist
									FROM ",object@temptables[["temptbl4"]]," a,",object@temptables[["temptbl4"]]," b 
									WHERE a.VarID = b.VarID and a.MedoidID = b.MedoidID
									GROUP BY 1,2,3) with data"))
			if(length(t)>1) stop(t)
			object@temptables <- c(object@temptables,list(temptbl3=c))
		}
		if(is.null(object@temptables[["temptbl2"]]))
		{
			t <- sqlSendUpdate(connection,paste0(" create table ",d," as (SELECT a.MedoidID as MedoidIDX,b.MedoidID as MedoidIDY,a.ObsID AS ObsIDX,
										b.ObsID AS ObsIDY,FLEuclideanDist(a.Num_Val, b.Num_Val) AS Dist
								  FROM ",object@temptables[["temptbl4"]]," a,",object@temptables[["temptbl4"]]," b 
								  WHERE a.VarID = b.VarID and a.MedoidID <> b.MedoidID
								  GROUP BY 1,2,3,4) with data"))
			if(length(t)>1) stop(t)
			object@temptables <- c(object@temptables,list(temptbl2=d))
		}

		temptbl2 <- object@temptables[["temptbl2"]]
		temptbl3 <- object@temptables[["temptbl3"]]

		sqlstr<-paste0("select a.ObsIDX as obs_id_colname,a.MedoidIDX AS MedoidID,a.MedoidIDY AS neighbor,(a.num/a.den) as sil_width 
						from(select a.MedoidIDX ,b.MedoidIDY,a.ObsIDX ,((b.bi-a.ai)) as num, 
									Case when a.ai>b.bi then a.ai else  b.bi end as den 
							from(select a.MedoidIDX , a.ObsIDX, FLMean(a.Dist) as ai 
								from ",temptbl3," a
								group by 1,2) as a,
								(select b.ObsIDX,b.MedoidIDX,c.MedoidIDY,b.bi  
								from(select a.MedoidIDX, a.ObsIDX,min(a.di) as bi 
									from(select a.MedoidIDX , a.MedoidIDY,a.ObsIDX, cast(FLMean(a.Dist) as decimal(38,7)) as di  
										from ",temptbl2," a
										group by 1,2,3) as a
									group by 1,2) as b,
								(select a.MedoidIDX , a.MedoidIDY,a.ObsIDX, cast(FLMean(a.Dist)as decimal(38,7)) as di  
								from ",temptbl2," a
								group by 1,2,3) as c
							where c.ObsIDX=b.ObsIDX and c.MedoidIDX=b.MedoidIDX and b.bi=c.di) as b
						where a.ObsIDX=b.ObsIDX and a.MedoidIDX=b.MedoidIDX) as a")
		
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

		sqlstr <- paste0("select '%insertIDhere%' AS vectorIdColumn, ROW_NUMBER() OVER(ORDER BY a.MedoidID) as vectorIndexColumn, FLMean(a.sil_width) as vectorValueColumn  
						from(select a.MedoidIDX as MedoidID, a.ObsIDX as ObsID,(a.num/a.den) as sil_width 
							from(select a.MedoidIDX ,a.ObsIDX ,((b.bi-a.ai)) as num, Case when a.ai>b.bi then a.ai else  b.bi end as den 
								from(select a.MedoidIDX , a.ObsIDX, FLMean(a.Dist) as ai 
									from ",temptbl3," a
									group by 1,2) as a,
								(select a.MedoidIDX, a.ObsIDX,FLMin(a.di) as bi 
								from(select a.MedoidIDX , a.MedoidIDY,a.ObsIDX, FLMean(a.Dist) as di  
									from ",temptbl2," a
									group by 1,2,3) as a
								group by 1,2) as b
							where a.ObsIDX=b.ObsIDX and a.MedoidIDX=b.MedoidIDX) as a) as a
							group by a.MedoidID")
		
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
							from(select a.MedoidIDX as MedoidID, a.ObsIDX as ObsID,(a.num/a.den) as sil_width 
								from(select a.MedoidIDX ,a.ObsIDX ,((b.bi-a.ai)) as num, Case when a.ai>b.bi then a.ai else  b.bi end as den 
									from(select a.MedoidIDX , a.ObsIDX, FLMean(a.Dist) as ai 
										from ",temptbl3," a
										group by 1,2) as a,
										(select a.MedoidIDX, a.ObsIDX,FLMin(a.di) as bi 
										from(select a.MedoidIDX , a.MedoidIDY,a.ObsIDX, FLMean(a.Dist) as di  
											from ",temptbl2," a
											group by 1,2,3) as a
										group by 1,2) as b
									where a.ObsIDX=b.ObsIDX and a.MedoidIDX=b.MedoidIDX) as a) as a")
			
			avg.widthvector <- sqlQuery(connection,sqlstr)[["avg_sil_width"]]
		}
		else
		avg.widthvector <- tryCatch(base::mean(widthsmatrix[,"sil_width"]),
									error=function(e) base::mean(widthsmatrix[,"SIL_WIDTH"]))
		# avg.widthvector <- base::mean(widthsmatrix[,"SIL_WIDTH"])

		silinfolist <- list(widths=widthsmatrix,
							clus.avg.widths=clus.avg.widthsvector,
							avg.width=avg.widthvector)

		
		object@results <- c(object@results,list(silinfo = silinfolist))
		##Drop temptables created if all components have already used them
		if(!is.null(object@results[["clusinfo"]]) && !is.null(object@results[["isolation"]]))
		{
			t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["temptbl2"]]))
			object@temptables[["temptbl2"]] <- NULL
			t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["temptbl4"]]))
			object@temptables[["temptbl4"]] <- NULL
		}
		t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["temptbl3"]]))
		object@temptables[["temptbl3"]] <- NULL

		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(silinfolist)
	}
}

diss <- function (x, ...)
{
   UseMethod("diss", x)
}

diss.FLKMedoids<-function(object)
{
	if(!is.null(object@results[["diss"]]))
	return(object@results[["diss"]])
	else
	{
			connection <- getConnection(object@table)
			flag1Check(connection)
			AnalysisID <- object@AnalysisID
			deeptablename <- paste0(object@deeptable@select@database,".",object@deeptable@select@table_name)
			obs_id_colname <- getVariables(object@deeptable)[["obs_id_colname"]]
			var_id_colname <- getVariables(object@deeptable)[["var_id_colname"]]
			cell_val_colname <- getVariables(object@deeptable)[["cell_val_colname"]]

			sqlstr<-paste0("SELECT '%insertIDhere%' as MATRIX_ID, 
									a.",obs_id_colname," AS rowIdColumn,
									b.",obs_id_colname," AS colIdColumn,
									FLEuclideanDist(a.",cell_val_colname,", b.",cell_val_colname,") AS valueColumn
							FROM ",deeptablename," a,",deeptablename," b
							WHERE a.",var_id_colname," = b.",var_id_colname," and a.",obs_id_colname,">b.",obs_id_colname," 
							GROUP BY a.",obs_id_colname,",b.",obs_id_colname)

			tblfunqueryobj <- new("FLTableFunctionQuery",
	                        connection = connection,
	                        variables=list(
	                            rowIdColumn="rowIdColumn",
	                            colIdColumn="colIdColumn",
	                            valueColumn="valueColumn"),
	                        whereconditions="",
	                        order = "",
	                        SQLquery=sqlstr)

		  	dissmatrix <- new("FLMatrix",
					            select= tblfunqueryobj,
					            dim=c(length(object@deeptable@dimnames[[1]]),
					            	length(object@deeptable@dimnames[[1]])),
					            dimnames=list(object@deeptable@dimnames[[1]],
					            			object@deeptable@dimnames[[1]]))

		  	dissmatrix <- tryCatch(as.sparseMatrix.FLMatrix(dissmatrix),
		  							error=function(e) dissmatrix)
		
		object@results <- c(object@results,list(diss = dissmatrix))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(dissmatrix)
	}
}

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

data.FLKMedoids<-function(object)
{
	if(!is.null(object@results[["data"]]))
	return(object@results[["data"]])
	else if(object@diss==TRUE) dataframe <- c()
	else
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
		dataframe <- x
	}
	object@results <- c(object@results,list(data = dataframe))
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	assign(parentObject,object,envir=parent.frame())
	return(dataframe)
}

print.FLKMedoids <- function(object)
{
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	results <- list()
	results <- c(results,list(medoids=medoids(object)),
						list(id.med=id.med(object)),
						list(clustering=clustering(object)),
						list(objective=objective(object)),
						list(isolation=""),
						list(clusinfo=""),
						list(silinfo=""),
						list(diss=""),
						list(call=call(object)),
						list(data=""))
	class(results) <- c("pam","partition","silhouette")

	assign(parentObject,object,envir=parent.frame())
	print(results)
}

setMethod("show","FLKMedoids",
			function(object)
			{
				parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
				print(object)
				assign(parentObject,object,envir=parent.frame())
			}
		 )

plot.FLKMedoids <- function(object)
{
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	results <- list()
	dataframe <- data.FLKMedoids(object)
	if(is.null(dataframe) || length(dataframe)==0)
	l <- list(diss=as.matrix(diss(object)))
	else
	l <- list(data=dataframe)
	results <- c(results,#list(medoids=medoids(object)),
						#list(id.med=id.med(object)),
						list(clustering=clustering(object)),
						list(objective=objective(object)),
						#list(isolation=isolation(object)),
						#list(clusinfo=clusinfo(object)),
						list(silinfo=silinfo(object)),
						#list(diss=diss(object)),
						list(call=call(object)),
						l
						)
	class(results) <- c("pam","partition","silhouette")

	assign(parentObject,object,envir=parent.frame())
	plot(results)
}
