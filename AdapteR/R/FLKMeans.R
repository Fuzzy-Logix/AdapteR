#' @include utilities.R
#' @include FLTable.R
#' @include FLMatrix.R
NULL
#' An S4 class to represent FLKMeans
#'
#' @slot no_of_centers A numeric vector containing the number of clusters, say k
#' @slot AnalysisID A character output used to retrieve the results of analysis
#' @slot odbc_connection ODBC connectivity for R
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
	"FLKMeans",
	slots=list(
		no_of_centers="numeric",
		AnalysisID="character",
		wideToDeepAnalysisId="character",
		odbc_connection="RODBC",
		table="FLTable",
		resultsfetched="vector",
		results ="list",
		deeptablename="character",
		nstart="numeric"
	)
)
kmeans <- function (x, ...) {
  UseMethod("kmeans", x)
}

kmeans.data.frame<-stats::kmeans
kmeans.matrix <- stats::kmeans

#' K-Means Clustering.
#'
#' \code{kmeans} performs k-means clustering on FLTable objects.
#'
#' The wrapper overloads kmeans and implicitly calls FLKMeans.
#' @method kmeans FLTable
#' @param table an object of class FLTable
#' @param centers the number of clusters
#' @param max.iter the maximum number of iterations allowed
#' @param nstart the initial number of random sets
#' @param exclude the comma separated character string of columns to be excluded
#' @param class_spec list describing the categorical dummy variables
#' @param where_clause takes the where_clause as a string 
#' @section Constraints:
#' None
#' @return \code{kmeans} performs k-means clustering and replicates equivalent R output.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' widetable  <- FLTable(connection, "FL_TRAIN", "tblAbaloneWide", "ObsID")
#' kmeansobject <- kmeans(widetable,3,20,2,"Rings,SEX",list("DummyCat(D)","SEX(M)"))
#' print(kmeansobject)
#' @export
kmeans.FLTable<-function(table,
						centers,
						max.iter =10,
						nstart = 1,
						exclude = as.character(c()),
						class_spec = list(),
						where_clause = ""
						)
{

	#Type validation
    if(is_number(centers)){
    centers  <- as.integer(centers)}
    else{
    stop("centers should be an integer")}
	
	if(is_number(max.iter)) {
	  max.iter <- as.integer(max.iter)
	} else {
	  stop("max.iter should be an integer")
	}

	argList  <- as.list(environment())

	typeList <- list(	centers      = "integer",
						max.iter     = "integer",
						nstart       = "double",
						exclude      = "character",
						class_spec   = "list",
						where_clause = "character"
					)

	classList <- list(table = "FLTable")
	validate_args(argList, typeList, classList)

	database<-table@db_name

	class_spec <- list_to_class_spec(class_spec)
	
	if(!table@isDeep){
		#deeptablename <- gen_deep_table_name(table@table_name)
		deeptablename <- "temp_deep_unique_1234_5678"
		
		sqlstr <- paste0("CALL FLWideToDeep('",table@table_name,"',
					  		 '",table@primary_key,"',
					  		 '",deeptablename,"', 
					  		 'ObsID', 
					  		 'VarID',
					  		 'Num_Val','",
					  		 exclude,"','",
					  		 class_spec,"','",
					  		 where_clause,"',
					  		 AnalysisID);")
		print(sqlstr)
		
		ret <- sqlQuery(table@odbc_connection,sqlstr)
		print(ret)
		wideToDeepAnalysisId <- as.character(ret[1,1])
	}
	else 
	{
		deeptablename <- table@table_name
	}

	if(where_clause!="")
    sqlstr <- paste("CALL FLKMeans( '",deeptablename,"',
			 					   'ObsID',
			 					   'VarID',
			 					   'Num_Val','",
			 					   where_clause,"',
			 					   ",centers," ,
			 					   ", max.iter,",",
			 					    nstart,",
			 					   'KMeans, clusters=2, maxiter=10, hypothesis=2',
			 					   AnalysisID );")
    else
    sqlstr <- paste("CALL FLKMeans( '",deeptablename,"',
			 					   'ObsID',
			 					   'VarID',
			 					   'Num_Val',
			 					    NULL,",
			 					    centers,",
			 					   ",max.iter,",",
			 					    nstart,",
			 					   'KMeans, clusters=2, maxiter=10, hypothesis=2',
			 					   AnalysisID )")
	
	print(sqlstr)
	retobj <- sqlQuery(table@odbc_connection,sqlstr)

	print(retobj)
	AnalysisID <- as.character(retobj[1,1])

	new("FLKMeans",
		no_of_centers=centers,
		AnalysisID=AnalysisID,
		wideToDeepAnalysisId=wideToDeepAnalysisId,
		odbc_connection=table@odbc_connection,
		table=table,
		resultsfetched=c(cluster=FALSE,
						centers=FALSE,
						tot.withinss=FALSE,
						betweenss=FALSE,
						totss=FALSE,
						withinss=FALSE,
						size=FALSE),
		deeptablename=deeptablename,
		nstart = nstart
	)
}

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
	else "That's not a valid property"
}

cluster <- function (x, ...) {
   UseMethod("cluster", x)
 }

cluster.FLKMeans<-function(object)
{
	if(object@resultsfetched["cluster"])
	{
		return(object@results[["cluster"]])
	}
	else
	{
		flag3Check(getConnection(object))

		connection = getConnection(object)
		AnalysisID = object@AnalysisID
		sqlstr<-paste0("INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL"),"  
						SELECT ",max_vector_id_value,",
						         ObsID,
						         CAST(SUBSTRING(clusterid FROM POSITION('-' IN clusterid )+1 ) AS INTEGER) 
						FROM fzzlKMeansClusterID 
						WHERE AnalysisID = '",AnalysisID,"' AND
						HypothesisID = ",object@nstart)

		sqlQuery(connection,sqlstr)
		#clustervector <- as.vector(retobj$ClusterID)
		#clustervector <- as.integer(substr(clustervector,nchar(clustervector),nchar(clustervector)))
		max_vector_id_value <<- max_vector_id_value + 1
	
		table <- FLTable(connection,
			             getOption("ResultDatabaseFL"),
			             getOption("ResultVectorTableFL"),
			             "VECTOR_ID",
			             "VECTOR_INDEX",
			             "VECTOR_VALUE")

		clustervector <- new("FLVector", 
							table = table, 
							col_name = table@num_val_name, 
							vector_id_value = max_vector_id_value-1, 
							size = length(object@table))

		object@resultsfetched["cluster"] <- TRUE
		object@results <- c(object@results,list(cluster = clustervector))
		# parentObj <- deparse(substitute(object))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(clustervector)
	}
}

centers <- function (x, ...) 
{
   UseMethod("centers", x)
}

centers.FLKMeans<-function(object)
{
	flag1Check(getConnection(object))
	if(object@resultsfetched["centers"])
	{
		return(object@results[["centers"]])
	}
	else
	{
		connection=getConnection(object)
		AnalysisID=object@AnalysisID
		sqlstr<-paste0("INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultMatrixTableFL")," 
						SELECT ",max_matrix_id_value,
						       ",CAST(SUBSTRING(clusterid FROM POSITION('-' IN clusterid )+1 ) AS INTEGER)
						        ,VarID
						        ,Centroid 
						FROM fzzlKMeansDendrogram 
						WHERE AnalysisID = '",AnalysisID,"' 
						AND HypothesisID = ",object@nstart)

		sqlQuery(connection,sqlstr)
		#centers<-as.vector(retobj$Centroid)
		#row=object@no_of_centers
		#col=length(centers)/row
		#centers<-matrix(centers,nrow=row,ncol=col,byrow=TRUE)
		ncol <- sqlQuery(connection,paste0(" SELECT COUNT(DISTINCT VarID) 
					                         FROM fzzlKMeansDendrogram
					                         WHERE AnalysisID = '",AnalysisID,"'"))[1,1]

		max_matrix_id_value <<- max_matrix_id_value + 1

		centersmatrix <- new("FLMatrix", 
			       odbc_connection = connection, 
			       db_name = getOption("ResultDatabaseFL"), 
			       matrix_table = getOption("ResultMatrixTableFL"), 
				   matrix_id_value = max_matrix_id_value-1,
				   matrix_id_colname = "MATRIX_ID", 
				   row_id_colname = "rowIdColumn", 
				   col_id_colname = "colIdColumn", 
				   cell_val_colname = "valueColumn",
				   nrow = object@no_of_centers, 
				   ncol = ncol, 
				   dimnames = list(c(),c()))

		object@resultsfetched["centers"] <- TRUE
		object@results <- c(object@results,list(centers = centersmatrix))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(centersmatrix)
	}
}

tot.withinss<-function(object,...){
	UseMethod("tot.withinss",object)
}

tot.withinss.FLKMeans<-function(object){
	flag3Check(getConnection(object))
	if(object@resultsfetched["tot.withinss"])
	{
		return(object@results[["tot.withinss"]])
	}
	else
	{
		sqlstr<-paste0("INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL")," 
						SELECT ",max_vector_id_value,
							   ",1,
							   CAST(sum(power((",object@deeptablename,".Num_Val - fzzlKMeansDendrogram.Centroid ),2)) AS NUMBER)  
						FROM fzzlKMeansClusterID,",object@deeptablename,",fzzlKMeansDendrogram 
						WHERE fzzlKMeansDendrogram.AnalysisID = '",object@AnalysisID,"' 
						AND fzzlKMeansClusterID.AnalysisID = '",object@AnalysisID,"' 
						AND ",object@deeptablename,".VarID=fzzlKMeansDendrogram.VarID 
						AND fzzlKMeansClusterID.ClusterID = fzzlKMeansDendrogram.ClusterID 
						AND fzzlKMeansClusterID.ObsID = ",object@deeptablename,".ObsID")

		sqlQuery(getConnection(object),sqlstr)
		max_vector_id_value <<- max_vector_id_value + 1
		
		table <- FLTable(connection,
			             getOption("ResultDatabaseFL"),
			             getOption("ResultVectorTableFL"),
			             "VECTOR_ID",
			             "VECTOR_INDEX",
			             "VECTOR_VALUE")

		tot_withinssvector <- new("FLVector", 
								table = table, 
								col_name = table@num_val_name, 
								vector_id_value = max_vector_id_value-1, 
								size = 1)
		
		object@resultsfetched["tot.withinss"] <- TRUE
		object@results <- c(object@results,list(tot.withinss = tot_withinssvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(tot_withinssvector)
	}
}

withinss<-function(object){
	UseMethod("withinss",object)
}

withinss.FLKMeans<-function(object){
	flag3Check(getConnection(object))
	if(object@resultsfetched["withinss"])
	{
		return(object@results[["withinss"]])
	}
	else
	{
		sqlstr<-paste0("INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL")," 
						SELECT ",max_vector_id_value,
								",fzzlKMeansClusterID.ClusterID
								 ,CAST(sum(power((",object@deeptablename,".Num_Val - fzzlKMeansDendrogram.Centroid ),2)) AS NUMBER)  
						FROM fzzlKMeansClusterID,",object@deeptablename,",fzzlKMeansDendrogram 
						WHERE fzzlKMeansDendrogram.AnalysisID = '",object@AnalysisID,"' 
						AND fzzlKMeansClusterID.AnalysisID = '",object@AnalysisID,"' 
						AND ",object@deeptablename,".VarID=fzzlKMeansDendrogram.VarID 
						AND fzzlKMeansClusterID.ClusterID = fzzlKMeansDendrogram.ClusterID 
						AND fzzlKMeansClusterID.ObsID = ",object@deeptablename,".ObsID 
						GROUP BY fzzlKMeansClusterID.ClusterID")

		sqlQuery(getConnection(object),sqlstr)
		max_vector_id_value <<- max_vector_id_value + 1
		
		table <- FLTable(connection,
			             getOption("ResultDatabaseFL"),
			             getOption("ResultVectorTableFL"),
			             "VECTOR_ID",
			             "VECTOR_INDEX",
			             "VECTOR_VALUE")

		withinssvector <- new("FLVector", 
							table = table, 
							col_name = table@num_val_name, 
							vector_id_value = max_vector_id_value-1, 
							size = object@no_of_centers)

		object@resultsfetched["withinss"] <- TRUE
		object@results <- c(object@results,list(withinss = withinssvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(withinssvector)
	}
	
}

betweenss<-function(object){
	UseMethod("betweenss",object)
}

betweenss.FLKMeans<-function(object){
	flag3Check(getConnection(object))
	if(object@resultsfetched["betweenss"])
	{
		return(object@results[["betweenss"]])
	}
	else
	{
		sqlstr<-paste0("INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL")," 
						SELECT ",max_vector_id_value,
							   ",1,
						 		CAST(sum(power((a.valavg - fzzlKMeansDendrogram.Centroid),2)) AS NUMBER) 
						FROM (SELECT VarID,average(",object@deeptablename,".Num_Val) AS valavg 
							  FROM ",object@deeptablename," 
							  GROUP BY VarID) AS a, 
							 fzzlKMeansClusterID, 
							 fzzlKMeansDendrogram 
						WHERE fzzlKMeansDendrogram.AnalysisID = '",object@AnalysisID,"' 
						AND fzzlKMeansClusterID.AnalysisID = '",object@AnalysisID,"' 
						AND ",object@deeptablename,".VarID=fzzlKMeansDendrogram.VarID 
						AND fzzlKMeansClusterID.ClusterID = fzzlKMeansDendrogram.ClusterID 
						AND fzzlKMeansClusterID.ObsID = ",object@deeptablename,".ObsID 
						AND a.VarID = ",object@deeptablename,".VarID")

		sqlQuery(getConnection(object),sqlstr)
		max_vector_id_value <<- max_vector_id_value + 1
		
		table <- FLTable(connection,
			             getOption("ResultDatabaseFL"),
			             getOption("ResultVectorTableFL"),
			             "VECTOR_ID",
			             "VECTOR_INDEX",
			             "VECTOR_VALUE")

		betweenssvector <- new("FLVector", 
							table = table, 
							col_name = table@num_val_name, 
							vector_id_value = max_vector_id_value-1, 
							size = 1)

		object@resultsfetched["betweenss"] <- TRUE
		object@results <- c(object@results,list(betweenss = betweenssvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(betweenssvector)
	}
}

totss<-function(object){
	UseMethod("totss",object)
}

totss.FLKMeans<-function(object){
	flag3Check(getConnection(object))
	if(object@resultsfetched["totss"])
	{
		return(object@results[["totss"]])
	}
	else
	{
		sqlstr<-paste0("INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL")," 
						SELECT ",max_vector_id_value,
							   ",1,
							    CAST(sum(power((",object@deeptablename,".Num_Val - a.valavg),2)) AS NUMBER) 
						FROM (SELECT VarID,average(",object@deeptablename,".Num_Val) AS valavg 
							  FROM ",object@deeptablename," GROUP BY VarID) AS a, ",
							 object@deeptablename," 
						WHERE a.VarID = ",object@deeptablename,".VarID;")
		
		sqlQuery(getConnection(object),sqlstr)
		max_vector_id_value <<- max_vector_id_value + 1
		
		table <- FLTable(connection,
			             getOption("ResultDatabaseFL"),
			             getOption("ResultVectorTableFL"),
			             "VECTOR_ID",
			             "VECTOR_INDEX",
			             "VECTOR_VALUE")

		totssvector <- new("FLVector", 
							table = table, 
							col_name = table@num_val_name, 
							vector_id_value = max_vector_id_value-1, 
							size = 1)

		object@resultsfetched["totss"] <- TRUE
		object@results <- c(object@results,list(totss = totssvector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(totssvector)
	}
}

size<-function(object){
	UseMethod("size",object)
}

size.FLKMeans<-function(object)
{
	# clustervector=cluster(object)
	# sizevector<-c()
	# i<-1
	# while(i<=object@no_of_centers){
	# 	sizevector[i]<-table(clustervector)[i]
	# 	i<-i+1
	# }
	# sizevector
	flag3Check(getConnection(object))
	if(object@resultsfetched["size"])
	{
		return(object@results[["size"]])
	}
	else
	{
		sqlstr <- paste0("INSERT INTO ",getOption("ResultDatabaseFL"),".",getOption("ResultVectorTableFL")," 
						  SELECT ",max_vector_id_value,
						         ",CAST(SUBSTRING(clusterid FROM POSITION('-' IN clusterid )+1 ) AS INTEGER)  
								  ,COUNT(ObsID) 
						  FROM  fzzlKMeansClusterID 
						  WHERE AnalysisID = '",object@AnalysisID,"'   
	                      AND HypothesisID = ",object@nstart," 
	                      GROUP BY ClusterID")

		sqlQuery(getConnection(object),sqlstr)
		max_vector_id_value <<- max_vector_id_value + 1
		
		table <- FLTable(connection,
			             getOption("ResultDatabaseFL"),
			             getOption("ResultVectorTableFL"),
			             "VECTOR_ID",
			             "VECTOR_INDEX",
			             "VECTOR_VALUE")

		sizevector <- new("FLVector", 
							table = table, 
							col_name = table@num_val_name, 
							vector_id_value = max_vector_id_value-1, 
							size = object@no_of_centers)

		object@resultsfetched["size"] <- TRUE
		object@results <- c(object@results,list(size = sizevector))
		parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
		assign(parentObject,object,envir=parent.frame())
		return(sizevector)
	}
}

# Prints the KMeans values
print.FLKMeans<-function(object)
{
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
	clustervector<-cluster(object)
	centermatrix<-centers(object)
	# i<-1
	# centercount<-""
	# while(i<=object@no_of_centers){
	# 	centercount<-paste0(centercount,table(clustervector)[i],", ")
	# 	i<-i+1
	# }
	temp1 <- as.vector(size(object))
	temp2 <- as.character(paste0(temp1,collapse=","))
	temp3 <- as.character(object@no_of_centers)

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

setMethod("show","FLKMeans",
			function(object)
			{
				parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
				print(object)
				assign(parentObject,object,envir=parent.frame())
			}
		 )

plot.FLKMeans <- function(object)
{
	if(!object@table@isDeep)
	{
		#maptable <- gen_wide_table_name(paste0(object@table@table_name,"map"))
		maptable <- "tempmap_unique_wide_1234_5678"
		print(maptable)

		sqlstr <- paste0(" CREATE TABLE ",maptable," AS ( 
			    	     SELECT a.Final_VarID AS VarID,
			    	     	    a.COLUMN_NAME AS ColumnName,
			    	     	    a.FROM_TABLE AS MapName 
			    	     FROM fzzlRegrDataPrepMap a 
			    	     WHERE a.AnalysisID = '",object@wideToDeepAnalysisId,"' 
			    	     AND a.Final_VarID IS NOT NULL) WITH DATA")
		
		print(sqlstr)
		print(sqlQuery(getConnection(object),sqlstr))

		#widetable <- gen_wide_table_name(paste0(object@table@table_name,"new"))
		widetable <- "temp_unique_wide_1234_5678"
		print(widetable)

		sqlstr <- paste0(" CALL FLDeepToWide('",object@deeptablename,"',
						'ObsID',
						'VarID',
						'Num_Val',
						'",maptable,"',
						'",object@table@table_name,"',
						'",widetable,"',
						Message);")

		print(sqlstr)
		print(sqlQuery(getConnection(object),sqlstr))

		x <- sqlQuery(getConnection(object),
					 paste0(" SELECT * FROM ",widetable," ORDER BY ObsID"))
		
		x <- as.data.frame(x)
		x$"ObsID" <- NULL
	}
	else
	{
		#widetable <- gen_wide_table_name(paste0(object@table@table_name,"new"))
		widetable <- "temp_unique_wide_1234_5678"

		sqlstr <- paste0(" CALL FLDeepToWide('",object@deeptablename,"',
						'",object@table@primary_key,"',
						'",object@table@var_id_name,"',
						'",object@table@num_val_name,"',
						NULL,
						NULL,
						'",widetable,"',
						Message);")
		print(sqlstr)
		print(sqlQuery(getConnection(object),sqlstr))

		x <- sqlQuery(getConnection(object),
					 paste0(" SELECT * FROM ",widetable," ORDER BY ",object@table@primary_key))
		x <- as.data.frame(x)
		x$object@table@primary_key <- NULL
	}

	print(x[20,])
	plot(x,col=as.vector(object$cluster))
	points(as.matrix(object$centers),col=1:object@no_of_centers,pch=8,cex=2)
}
