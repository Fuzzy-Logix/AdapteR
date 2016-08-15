#' @include FLMatrix.R
NULL

#' @export
hclust <- function (d,...) {
  UseMethod("hclust", d)
}

#' @export
hclust.data.frame<- stats::hclust
#' @export
hclust.matrix <- stats::hclust
#' @export
hclust.default <- stats::hclust

#' Hierarchial Clustering
#'
#' \code{hclust} computes hierarchial 
#' clustering on FLTable objects.
#'
#' @param d an object of class FLTable, can be wide or deep table
#' @param method character. Allowed methods are "average",
#' "single", "complete", "centroid"
#' @param maxit maximum number of iterations
#' @param excludeCols the comma separated character string of columns to be excluded
#' @param classSpec list describing the categorical dummy variables
#' @param whereconditions takes the where_clause as a string
#' @section Constraints:
#' Error is thrown if results cannot be fetched. maxit should be more than
#' no.of. observations for algorithm to reach completion.
#' Error is thrown if algorithm does not reach completion or more than one
#' cluster is formed at any step.
#' @return \code{hclust} returns a list and replicates equivalent R output
#' from \code{hclust} in stats package.The mapping table can be viewed
#' using \code{object$mapping} if input is wide table.
#' @section Constraints:
#' If classSpec is not specified, the categorical variables are excluded
#' from analysis by default.
#' @examples
#' deeptable  <- FLTable("tblUSArrests", "ObsID","VarID","Num_Val")
#' hclustobject <- hclust(deeptable,maxit=50)
#' print(hclustobject)
#' plot(hclustobject)
#' @export
hclust.FLTable <- function(d,
						method="average",
    					maxit = 500,
						excludeCols = "",
						classSpec = list(),
						whereconditions = "",
						...
						)
{
	agnesobject <- agnes.FLTable(d,
								maxit=maxit,
								excludeCols=excludeCols,
								classSpec=classSpec,
								whereconditions=whereconditions,
								method=method)

	heightvector <- agnesobject$height
	if(is.FLVector("heightvector"))
	{
		tryCatch(heightvector <- as.vector(heightvector),
			error=function(e)stop("cannot fetch height vector. Try this to view:-",
				cat(constructSelect(heightvector))))
	}
	resultList <- list(merge=agnesobject$merge,
						height=heightvector,
						order=agnesobject$order,
						labels=agnesobject$order.lab,
						call=match.call(),
						method=method,
						dist.method="euclidean"
						)
	class(resultList) <- "hclust"
	return(resultList)
}


#' @export
hclust.FLMatrix <- function(d,
						method="average",
    					maxit = 500,
						excludeCols = "",
						classSpec = list(),
						whereconditions = "",
						...
						)
{
	d <- as.FLTable(d)
	return(hclust(d=d,
				method=method,
				maxit = maxit,
				excludeCols = excludeCols,
				classSpec = classSpec,
				whereconditions = whereconditions,
				...))
}

