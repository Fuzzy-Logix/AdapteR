#' @include utilities.R
#' @include FLTable.R
#' @include FLMatrix.R
#' @include FLVector.R
NULL

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
#' @method kmeans FLTable
#' @param x an object of class FLTable, wide or deep
#' @param centers the number of clusters
#' @param iter.max the maximum number of iterations allowed
#' @param nstart the initial number of random sets
#' @param excludeCols the comma separated character string of columns to be excluded
#' @param classSpec list describing the categorical dummy variables
#' @param whereconditions takes the where_clause as a string 
#' @return \code{kmeans} returns a list which replicates equivalent R output
#' from \code{kmeans} in stats package. The mapping table can be viewed
#' using \code{object$mapping} if input is wide table.
#' @section Constraints:
#' If classSpec is not specified, the categorical variables are excluded
#' from analysis by default.
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' widetable  <- FLTable( "FL_DEMO", "tblAbaloneWide", "ObsID")
#' kmeansobject <- kmeans(widetable,3,20,1,"Rings,SEX")
#' print(kmeansobject)
#' plot(kmeansobject)
#' One can specify ClassSpec and transform categorical variables 
#' before clustering. This increases the number of variables in the plot
#' because categorical variable is split into binary numerical variables.
#' The clusters may not be well-defined as is observed in the case below:-
#' kmeansobjectnew <- kmeans(widetable,3,20,1,"Rings,SEX",list("DummyCat(D)","SEX(M)"))
#' plot(kmeansobjectnew)
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
	hkmeansobject <- hkmeans.FLTable(x=x,
									centers=centers,
									levels=1,
									iter.max=iter.max,
									nstart=nstart,
									excludeCols=excludeCols,
									classSpec=classSpec,
									whereconditions=whereconditions)

	return(hkmeansobject)
}
