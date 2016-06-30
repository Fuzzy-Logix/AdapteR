#' @include utilities.R
#' @include FLTable.R
#' @include FLMatrix.R
#' @include FLVector.R
NULL

#' An S4 class to represent FLAggClustering
setClass(
	"FLAggClust",
	slots=list(
		AnalysisID="character",
		wideToDeepAnalysisId="character",
		table="FLTable",
		results ="list",
		deeptable="FLTable",
		temptables="list",
		mapTable="character",
		diss="logical"
	)
)

#' @export
agnes <- function (x,...) {
  UseMethod("agnes", x)
}

#' @export
agnes.data.frame<-cluster::agnes
#' @export
agnes.matrix <- cluster::agnes
#' @export
agnes.default <- cluster::agnes

#' Agglomerative Nesting
#'
#' \code{agnes} computes agglomeraive hierarchial 
#' clustering on FLTable objects.
#'
#' @method agnes FLTable
#' @param x an object of class FLTable, can be wide or deep table
#' @param diss logical if \code{x} is dissimilarity matrix.
#' currently not used
#' @param metric only "euclidean" distance supported currently
#' @param Stand logical indicating if standardization
#' should be done before calculating diss matrix
#' @param method character. Allowed methods are "average",
#' "single", "complete", "centroid"
#' @param par.method currently not used and always 0
#' @param keep.diss logicals indicating if the 
#' dissimilarities and/or input data x should be kept in the result
#' @param keep.data logicals indicating if the 
#' dissimilarities and/or input data x should be kept in the result
#' @param trace.lev integer specifying a trace level for 
#' printing diagnostics during the build and swap phase of the algorithm.
#' currently always 0
#' @param maxit maximum number of iterations
#' @param excludeCols the comma separated character string of columns to be excluded
#' @param classSpec list describing the categorical dummy variables
#' @param whereconditions takes the where_clause as a string
#' @param distTable name of the in-database table having dissimilarity
#' matrix or distance table
#' @section Constraints:
#' Plotting for large datasets takes longer time to fetch data.
#' Error is thrown if results cannot be fetched. maxit should be more than
#' no.of. observations for algorithm to reach completion.
#' Error is thrown if algorithm does not reach completion or more than one
#' cluster is formed at any step.
#' If classSpec is not specified, the categorical variables are excluded
#' from analysis by default.
#' @return \code{agnes} returns a list and replicates equivalent R output
#' from \code{agnes} in cluster package. The mapping table can be viewed
#' using \code{object$mapping} if input is wide table.
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' deeptable  <- FLTable("FL_DEMO", "tblUSArrests", "ObsID","VarID","Num_Val")
#' agnesobject <- agnes(deeptable,maxit=50)
#' print(agnesobject)
#' plot(agnesobject)
#' One can specify ClassSpec and transform categorical variables 
#' before clustering. This increases the number of variables in the plot
#' because categorical variable is split into binary numerical variables.
#' The clusters may not be well-defined as is observed in the case below:-
#' widetable  <- FLTable( "FL_DEMO", "iris", "rownames")
#' agnesobjectnew <- agnes(widetable,maxit=500,classSpec=list("Species(setosa)"))
#' The below plot throws warnings!
#' plot(agnesobjectnew)
#' @export
agnes.FLTable <- function(x,
						diss=FALSE,
						metric="euclidean",##notUsed
						Stand=FALSE,##notUsed
						method="average",
						par.method = 0,
    					keep.diss = (!diss),
    					keep.data = (!diss),
    					trace.lev = 0,##notUsed
    					maxit = 500,
						excludeCols = "",
						classSpec = list(),
						whereconditions = "",
						distTable=""
						)
{

	#Type validation
	if(any(!(c(maxit) >= 1)))
    stop("maxit should be atleast 1")
    else
    {
    	maxit <- as.integer(max(maxit))
    	if(maxit<nrow(x))
    	cat(paste0("warning: maxit must atleast be ",
    		"equal to no.of.observations for clustering to reach completion.",
    		"Upon non completion, several features like order,print,plot may not work."))
    }

	argList  <- as.list(environment())

	typeList <- list(	method = "character",
						maxit = "integer",
						excludeCols  = "character",
						classSpec   = "list",
						whereconditions = "character",
						diss = "logical",
						metric = "character",
						Stand = "logical",
						keep.diss = "logical",
						keep.data = "logical",
						distTable = "character",
						par.method = "double"
					)

	classList <- list(x = "FLTable")
	validate_args(argList, typeList, classList)

    connection <- getConnection(x)
    wideToDeepAnalysisId <- ""
    mapTable <- ""
	
	vcall <- match.call()
	methodVector <- c("average","single","complete","centroid")
	if(!(method[1] %in% methodVector))
	stop("method must be one of ",paste0(methodVector,collapse=","))
	else
	methodID <- as.integer(charmatch(method[1],methodVector)[1])
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
		deeptablename <- gen_view_name()
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
		deepx <- setAlias(deepx,"")
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
		deepx <- setAlias(deepx,"")
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

	vnote <- genNote("agnes")
    sqlstr <- paste0("CALL FLAggClustering( '",deeptable,"',
					 					   '",getVariables(deepx)[["obs_id_colname"]],"',
					 					   '",getVariables(deepx)[["var_id_colname"]],"',
					 					   '",getVariables(deepx)[["cell_val_colname"]],"',",
					 					   whereClause,",",
					 					   methodID,",",
					 					   maxit,",",
					 					   fquote(vnote),",AnalysisID );")
	
	retobj <- sqlQuery(connection,sqlstr,
						AnalysisIDQuery=genAnalysisIDQuery("fzzlKMeansInfo",vnote))
	retobj <- checkSqlQueryOutput(retobj)
	AnalysisID <- as.character(retobj[1,1])
	
	FLAggCLustobject <- new("FLAggClust",
							AnalysisID=AnalysisID,
							wideToDeepAnalysisId=wideToDeepAnalysisId,
							table=x,
							results=list(call=vcall),
							deeptable=deepx,
							temptables=list(),
							mapTable=mapTable,
							diss=diss)
	return(FLAggCLustobject)
}

#' @export
`$.FLAggClust`<-function(object,property)
{
	#parentObject <- deparse(substitute(object))
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

	if(property=="order")
	{
		ordervector <- order.FLAggClust(object)
		assign(parentObject,object,envir=parent.frame())
		return(ordervector)
	}
	else if(property=="order.lab")
	{
		order.labvector <- rownames(object@deeptable)
		assign(parentObject,object,envir=parent.frame())
		return(order.labvector)
	}
	else if(property=="height")
	{
		heightvector <- height.FLAggClust(object)
		assign(parentObject,object,envir=parent.frame())
		return(heightvector)
	}
	else if(property=="ac") ##What are crisp clusters?
	{
		ac <- ac.FLAggClust(object)
		assign(parentObject,object,envir=parent.frame())
		return(ac)
	}
	else if(property=="merge")
	{
		mergematrix <- merge.FLAggClust(object)
		assign(parentObject,object,envir=parent.frame())
		return(mergematrix)
	}
	else if(property=="diss")
	{
		dissmatrix <- diss.FLKMedoids(object)
		assign(parentObject,object,envir=parent.frame())
		return(dissmatrix)
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


order.FLAggClust <- function(object)
{
	if(!is.null(object@results[["order"]]))
	return(object@results[["order"]])
	else
	{
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		results <- list()
		heightvector <- height.FLAggClust(object)
		if(length(heightvector)!=(nrow(object@deeptable)-1))
		{
			cat(paste0("Warning:error in algorithm.\nMore than one cluster ",
						"formed at a step or could not reach completion.\nTry to increase maxit.\n"))
			cat("randomly initializing order...\n")
			ordervector <- 1:(nrow(object@deeptable)-1)
			return(ordervector)
		}
		results <- c(results,list(height=heightvector),
							list(data=object$data),
							list(merge=merge.FLAggClust(object)))
		class(results) <- c("agnes","twins")
		ordervector <- tryCatch(stats::order.dendrogram(stats::as.dendrogram(stats::as.hclust(results))),
								error=function(e){
									cat(paste0("Error in algorithm.More than one cluster ",
										"formed at a step or could not reach completion.Try to increase maxit."))
									cat("randomly initializing order...")
									ordervector <- 1:(nrow(object@deeptable)-1)
									ordervector
								})

		object@results <- c(object@results,list(order = ordervector))
		assign(parentObject,object,envir=parent.frame())
		return(ordervector)
	}
}

height.FLAggClust <- function(object)
{
	if(!is.null(object@results[["height"]]))
	return(object@results[["height"]])
	else
	{
		connection <- getConnection(object@table)
		AnalysisID <- object@AnalysisID
		deeptablename <- paste0(object@deeptable@select@database,".",object@deeptable@select@table_name)
		obs_id_colname <- getVariables(object@deeptable)[["obs_id_colname"]]
		var_id_colname <- getVariables(object@deeptable)[["var_id_colname"]]
		cell_val_colname <- getVariables(object@deeptable)[["cell_val_colname"]]
		
		a <- paste0(getOption("ResultDatabaseFL"),".",gen_unique_table_name("3"))
		b <- paste0(getOption("ResultDatabaseFL"),".",gen_unique_table_name("4"))

		##Ensure required temptables exist
		if(is.null(object@temptables[["agnesCentroid"]]))
		{
			t <- sqlSendUpdate(connection,paste0(" CREATE TABLE ",a,
							" AS (SELECT a.HypothesisID AS LevelID,",
										" a.ClusterID,",
										" b.",var_id_colname," AS VarID,
										 AVERAGE(b.",cell_val_colname,") AS Centroid ",
								" FROM fzzlKMeansClusterID a,",
									deeptablename," AS b ",
								" WHERE a.ObsID=b.",obs_id_colname,
								" AND a.AnalysisID='",AnalysisID,"' ",
								" GROUP BY a.HypothesisID,a.ClusterID,",
									" b.",var_id_colname,")WITH DATA"))
			if(length(t)>1) stop(t)
			object@temptables <- c(object@temptables,list(agnesCentroid=a))
		}
		
		if(is.null(object@temptables[["agnesMembership"]]))
		{
			t <- sqlSendUpdate(connection,paste0(" CREATE TABLE ",b," AS ",
						"(SELECT a.HypothesisID AS OldLevel,",
								"a.ClusterID AS OldClusterID,",
								"b.HypothesisID AS NewLevel,",
								"b.ClusterID AS NewClusterID,",
								"a.ObsID ",
						" FROM fzzlKMeansClusterID a,fzzlKMeansClusterID b ",
						" WHERE a.AnalysisID = b.AnalysisID ",
						" AND a.ObsID = b.ObsID ",
						" AND a.AnalysisID = '",AnalysisID,"'",
						" AND a.HypothesisID = b.HypothesisID - 1",
						" AND a.ClusterID <> b.ClusterID) WITH DATA"))

			if(length(t)>1) stop(t)
			object@temptables <- c(object@temptables,list(agnesMembership=b))
		}

		agnes <- object@temptables[["agnesMembership"]]
		agnesCentroid <- object@temptables[["agnesCentroid"]]

		sqlstr<-paste0("SELECT '%insertIDhere%' AS vectorIdColumn,",
							"a.LevelID+1 as vectorIndexColumn,",
							"FLEuclideanDist(a.Centroid, b.Centroid) AS vectorValueColumn ",
						" FROM ",agnesCentroid," a, ",agnesCentroid," b, ",
							"(SELECT DISTINCT OldLevel,OldCLusterID,NewClusterID FROM ",agnes,") c ",
						" WHERE a.LevelID=c.OldLevel AND a.ClusterID=c.OldClusterID ",
							" AND b.LevelID=c.OldLevel AND b.ClusterID=c.NewClusterID ",
							" AND a.varID=b.VarID ",
						" GROUP BY a.ClusterID,b.ClusterID,a.LevelID")

		tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = connection,
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

		heightvector <- new("FLVector",
							select = tblfunqueryobj,
							dimnames = list(1:(length(object@deeptable@dimnames[[1]])-1),
											"vectorValueColumn"),
							isDeep = FALSE)
		heightvector <- tryCatch(as.vector(heightvector),
      						error=function(e){heightvector})
		
		object@results <- c(object@results,list(height = heightvector))
		##Drop temptables created if all components have already used them
		if(!is.null(object@results[["ac"]]) && is.numeric(heightvector))
		{
			t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["agnesCentroid"]]))
			object@temptables[["agnesCentroid"]] <- NULL
		}
		if(!is.null(object@results[["ac"]])
			&& is.numeric(heightvector)
			&& is.matrix(object@results[["merge"]]))
		{
			t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["agnesMembership"]]))
			object@temptables[["agnesMembership"]] <- NULL
		}
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(heightvector)
	}
}

ac.FLAggClust <- function(object){
	if(!is.null(object@results[["ac"]]))
	return(object@results[["ac"]])
	else
	{
		connection <- getConnection(object@table)
		AnalysisID <- object@AnalysisID
		deeptablename <- paste0(object@deeptable@select@database,".",object@deeptable@select@table_name)
		obs_id_colname <- getVariables(object@deeptable)[["obs_id_colname"]]
		var_id_colname <- getVariables(object@deeptable)[["var_id_colname"]]
		cell_val_colname <- getVariables(object@deeptable)[["cell_val_colname"]]
		
		a <- paste0(getOption("ResultDatabaseFL"),".",gen_unique_table_name("3"))
		b <- paste0(getOption("ResultDatabaseFL"),".",gen_unique_table_name("4"))

		##Ensure required temptables exist
		if(is.null(object@temptables[["agnesCentroid"]]))
		{
			t <- sqlSendUpdate(connection,paste0(" CREATE TABLE ",a,
							" AS (SELECT a.HypothesisID AS LevelID,",
										" a.ClusterID,",
										" b.",var_id_colname," AS VarID,
										AVERAGE(b.",cell_val_colname,") AS Centroid ",
								" FROM fzzlKMeansClusterID a,",
									deeptablename," AS b ",
								" WHERE a.ObsID=b.",obs_id_colname,
								" AND a.AnalysisID='",AnalysisID,"' ",
								" GROUP BY a.HypothesisID,a.ClusterID,",
									" b.",var_id_colname,")WITH DATA"))
			if(length(t)>1) stop(t)
			object@temptables <- c(object@temptables,list(agnesCentroid=a))
		}
		
		if(is.null(object@temptables[["agnesMembership"]]))
		{
			t <- sqlSendUpdate(connection,paste0(" CREATE TABLE ",b," AS ",
						"(SELECT a.HypothesisID AS OldLevel,",
								"a.ClusterID AS OldClusterID,",
								"b.HypothesisID AS NewLevel,",
								"b.ClusterID AS NewClusterID,",
								"a.ObsID ",
						" FROM fzzlKMeansClusterID a,fzzlKMeansClusterID b ",
						" WHERE a.AnalysisID = b.AnalysisID ",
						" AND a.ObsID = b.ObsID ",
						" AND a.AnalysisID = '",AnalysisID,"'",
						" AND a.HypothesisID = b.HypothesisID - 1",
						" AND a.ClusterID <> b.ClusterID) WITH DATA"))

			if(length(t)>1) stop(t)
			object@temptables <- c(object@temptables,list(agnesMembership=b))
		}

		agnes <- object@temptables[["agnesMembership"]]
		agnesCentroid <- object@temptables[["agnesCentroid"]]

		sqlstr <- paste0("SELECT FLMean(1-(a.mi/b.Dist)) AS ac FROM ",
							"(SELECT a.ClusterID AS ObsIDX,",
								"b.ClusterID AS ObsIDY,",
								"FLEuclideanDist(a.Centroid, b.Centroid) AS mi ",
							" FROM ",agnesCentroid," a,",agnesCentroid," b,",
								"(SELECT ObsID,min(res1) AS oldlevel FROM ",
									"(SELECT a.ObsID,min(a.OldLevel) AS res1 ",
										" FROM ",agnes," a ",
										" GROUP BY a.ObsID ",
									" UNION ALL ",
									"SELECT CAST(a.NewClusterID AS INT) AS ObsID,",
											"min(a.OldLevel) AS res1 FROM ",agnes," a ",
										" GROUP BY a.NewClusterID) a ",
								" GROUP BY 1) AS c,",
								"fzzlKMeansClusterID AS d ",
								" WHERE a.VarID = b.VarID AND d.AnalysisID='",AnalysisID,"' ",
								" AND c.oldlevel=a.LevelID AND d.HypothesisID=c.oldlevel+1 ",
								" AND b.LevelID=d.HypothesisID AND b.ClusterID=d.ClusterID ",
								" AND d.ObsID=c.ObsID AND CAST(a.ClusterID AS int)=c.ObsID ",
							" GROUP BY 1,2) as a,",
							"(SELECT a.ClusterID AS ObsIDX, b.ClusterID AS ObsIDY,",
								"FLEuclideanDist(a.Centroid, b.Centroid) AS Dist ",
								" FROM ",agnesCentroid," a,",agnesCentroid," b, ",
									"(SELECT * FROM ",agnes,
									" WHERE NewLevel=(SELECT max(NewLevel) FROM ",agnes,")) AS c ",
							" WHERE a.LevelID=c.OldLevel AND a.ClusterID=c.OldClusterID ",
							" AND b.LevelID=c.NewLevel AND b.ClusterID=c.NewClusterID ",
							" AND a.varID=b.VarID ",
							" GROUP BY 1,2) AS b")

		ac <- sqlQuery(connection,sqlstr)[["ac"]]
		object@results <- c(object@results,list(ac = ac))
		##Drop temptables created if all components have already used them
		if(is.numeric(object@temptables[["height"]]))
		{
			t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["agnesCentroid"]]))
			object@temptables[["agnesCentroid"]] <- NULL
		}
		if(is.numeric(object@temptables[["height"]])
			&& is.matrix(object@results[["merge"]]))
		{
			t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["agnesMembership"]]))
			object@temptables[["agnesMembership"]] <- NULL
		}
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(ac)
	}
}

merge.FLAggClust <- function(object){
	if(!is.null(object@results[["merge"]]))
	return(object@results[["merge"]])
	else
	{
		connection <- getConnection(object@table)
		AnalysisID <- object@AnalysisID
		deeptablename <- paste0(object@deeptable@select@database,".",object@deeptable@select@table_name)
		obs_id_colname <- getVariables(object@deeptable)[["obs_id_colname"]]
		var_id_colname <- getVariables(object@deeptable)[["var_id_colname"]]
		cell_val_colname <- getVariables(object@deeptable)[["cell_val_colname"]]
		
		b <- paste0(getOption("ResultDatabaseFL"),".",gen_unique_table_name("4"))

		##Ensure required temptables exist
		if(is.null(object@temptables[["agnesMembership"]]))
		{
			t <- sqlSendUpdate(connection,paste0(" CREATE TABLE ",b," AS ",
						"(SELECT a.HypothesisID AS OldLevel,",
								"a.ClusterID AS OldClusterID,",
								"b.HypothesisID AS NewLevel,",
								"b.ClusterID AS NewClusterID,",
								"a.ObsID ",
						" FROM fzzlKMeansClusterID a,fzzlKMeansClusterID b ",
						" WHERE a.AnalysisID = b.AnalysisID ",
						" AND a.ObsID = b.ObsID ",
						" AND a.AnalysisID = '",AnalysisID,"'",
						" AND a.HypothesisID = b.HypothesisID - 1",
						" AND a.ClusterID <> b.ClusterID) WITH DATA"))

			if(length(t)>1) stop(t)
			object@temptables <- c(object@temptables,list(agnesMembership=b))
		}

		agnes <- object@temptables[["agnesMembership"]]

		sqlstr <- paste0(" SELECT a.NewLevel,CAST(max(res1) AS int),max(res2)",
						" FROM ",
							"(SELECT a.NewLevel,",
								" CASE WHEN a.NewClusterID=b.NewClusterID OR ",
								" a.NewClusterID=CAST(b.ObsID AS int) ",
								" THEN b.NewLevel ELSE (-1*a.NewClusterID) END AS res1,",
								" CASE WHEN a.ObsID=CAST(b.NewClusterID AS float) OR ",
								" a.ObsID= b.ObsID THEN b.NewLevel ELSE (-1*a.ObsID) END AS res2",
							" FROM ",agnes," a,",agnes," b",
							" WHERE b.NewLevel < a.NewLevel AND res1 IS NOT NULL ",
							" AND res2 IS NOT NULL) AS a ",
						" GROUP BY 1 ",
						" UNION ALL ",
						" SELECT a.NewLevel, (-1*a.NewClusterID), (-1*a.ObsID)",
						" FROM ",agnes," a WHERE a.OldLevel=0 ORDER BY 1")

		tryCatch(mergematrix <- sqlQuery(connection,sqlstr),
			error=function(e) stop("cannot fetch data. Try this to view merge:-",sqlstr))
		mergematrix[["NewLevel"]] <- NULL
		mergematrix <- as.matrix(mergematrix)
		dimnames(mergematrix) <- list(NULL,NULL)

		object@results <- c(object@results,list(merge = mergematrix))
		##Drop temptables created if all components have already used them
		if(is.numeric(object@temptables[["height"]])
			&& !is.null(object@results[["ac"]]))
		{
			t<-sqlSendUpdate(connection,paste0(" DROP TABLE ",object@temptables[["agnesMembership"]]))
			object@temptables[["agnesMembership"]] <- NULL
		}
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(mergematrix)
	}
	
}

#' @export
print.FLAggClust <- function(object)
{
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	results <- list()
	heightvector <- height.FLAggClust(object)
	ordervector <- order.FLAggClust(object)
	order.labvector <- object$order.lab
	results <- c(results,list(height=heightvector),
						list(ac=ac.FLAggClust(object)),
						list(order=ordervector),
						list(order.lab=order.labvector),
						list(diss=""),
						list(call=object@results[["call"]]),
						list(data=""),
						list(merge=""))
	class(results) <- c("agnes","partition","silhouette","twins")

	assign(parentObject,object,envir=parent.frame())
	print(results)
}

#' @export
setMethod("show","FLAggClust",
			function(object)
			{
				parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
				print(object)
				assign(parentObject,object,envir=parent.frame())
			}
		 )

#' @export
plot.FLAggClust <- function(object)
{
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	results <- list()
	heightvector <- height.FLAggClust(object)
	if(length(heightvector)!=(nrow(object@deeptable)-1))
		{
			cat(paste0("Warning:error in algorithm.More than one cluster ",
						"formed at a step or could not reach completion.\nTry to increase maxit.\n"))
			cat("Dendrogram plot not available\n")
		}
	dataframe <- object$data
	if(is.null(dataframe) || length(dataframe)==0)
	l <- list(diss=as.matrix(object$diss))
	else
	l <- list(data=dataframe)
	results <- c(results,list(order=order.FLAggClust(object)),
						list(merge=merge.FLAggClust(object)),
						list(height=heightvector),
						list(ac=ac.FLAggClust(object)),
						list(call=object@results[["call"]]),
						l
						)
	class(results) <- c("agnes","partition","silhouette","twins")

	assign(parentObject,object,envir=parent.frame())
	plot(results)
}


#' @export
agnes.FLMatrix <- function(x,
						diss=FALSE,
						metric="euclidean",##notUsed
						Stand=FALSE,##notUsed
						method="average",
						par.method = 0,
    					keep.diss = (!diss),
    					keep.data = (!diss),
    					trace.lev = 0,##notUsed
    					maxit = 500,
						excludeCols = "",
						classSpec = list(),
						whereconditions = "",
						distTable=""
						)
{
	x <- as.FLTable(x)
	return(agnes(x=x,
				diss=diss,
				metric=metric,##notUsed
				Stand=Stand,##notUsed
				method="average",
				par.method = 0,
				keep.diss = keep.diss,
				keep.data = keep.data,
				trace.lev = 0,##notUsed
				maxit = maxit,
				excludeCols = excludeCols,
				classSpec = classSpec,
				whereconditions = whereconditions,
				distTable=distTable
			))
}
