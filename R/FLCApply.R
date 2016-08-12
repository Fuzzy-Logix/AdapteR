#' @include FLMatrix.R
NULL

#' Apply a function to a subset of data
#'
#' Partition data based on given column
#' and apply given function on each partition
#' @param data FLTable object
#' @param FUN function to apply on each subset
#' @param column character giving column to partition by
#' or integer index of the column
#' @return list of results from each subset
#' @examples
#' connection <- flConnect(odbcSource="Gandalf",database="FL_DEMO",platform="TD")
#' irisfl <- FLTable("FL_DEMO","iris","rownames")
#' resultList <- FLCApply(irisfl,function(x)kmeans(x,3),"Species")
#' print(resultList$setosa)
#' plot(resultList$virginica)
#' print(resultList$versicolor)
#' @export
FLCApply <- function(data,FUN,column)
{
	if(class(data)!="FLTable") stop(" input FLTable object as data")
	connection <- getConnection(data)
	if(is.numeric(column))
	{
		column <- data@dimnames[[2]][column]
		if(is.na(column) || is.null(column))
		stop(" null dimnames or column not found")
	}
	else if(is.character(column))
	{
		if(!(column %in% data@dimnames[[2]]))
		stop(" null dimnames or column not found")
	}
	sqlstr <- paste0("SELECT DISTINCT a.",column,
					" FROM (",constructSelect(data),") as a")
	columnValues <- sqlQuery(connection,sqlstr)[[1]]
	if(class(data@select)=="FLTableFunctionQuery")
	data <- store(data)

	datalist <- plyr::llply(columnValues,function(x){
		data@select@whereconditions <- c(data@select@whereconditions,
			paste0(tableAndAlias(data),".",column,"=''",x,"''"))
		return(data)
		})
	names(datalist) <- columnValues
	return(plyr::llply(datalist,FUN))

}
