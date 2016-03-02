#' @include FLVector.R
#' @include FLMatrix.R
#' @include FLTable.R
#' @include utilities.R
NULL

#' Correlation.
#'
#' \code{cor} computes correlation of in-database Objects
#'
#' @param x FLMatrix, FLVector or FLTable object or any R object
#' @param y FLMatrix, FLVector or FLTable object or any R object
#' @param ... any additional arguments
#' @section Constraints:
#' The number of non-null pairs must be greater than or equal to 2.
#' If number of non-null pairs is less than 2, FLCorrel returns a NULL.
#' @return \code{cor} returns FLMatrix object representing correlation of x and y.
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' deeptable <- FLTable( "FL_DEMO", 
#' "tblUSArrests", "ObsID","VarID","Num_Val")
#' widetable <- FLTable("FL_DEMO","tblAbaloneWide","ObsID")
#' cor(deeptable,deeptable)
#' cor(widetable,widetable)
#' @export
cor <- function (x,y, ...) {
	UseMethod("cor", x)
}

#' @export
cor.default<-stats::cor

#' @export
cor.FLMatrix <- function(x,y=x,...)
{
	connection <- getConnection(x)
    ##browser()
    if(is.FLMatrix(y))
    {
    	if(nrow(x)!=nrow(y)) stop("incompatible dimensions")

        a <- genRandVarName()
		b <- genRandVarName()
		sqlstr <- paste0("SELECT '%insertIDhere%' AS MATRIX_ID,",
								a,".colIdColumn AS rowIdColumn,",
								b,".colIdColumn AS colIdColumn,",
							 x@select@database,".FLCorrel(",a,".valueColumn,",
							 					  b,".valueColumn) AS valueColumn 
						FROM ( ",constructSelect(x),") AS ",a,
		                  ",( ",constructSelect(y),") AS ",b,
            			constructWhere(c(paste0(a,".rowIdColumn = ",b,".rowIdColumn"))),
            			" GROUP BY ",a,".colIdColumn,",b,".colIdColumn")

		tblfunqueryobj <- new("FLTableFunctionQuery",
                connection = connection,
                variables=list(
                    rowIdColumn="rowIdColumn",
                    colIdColumn="colIdColumn",
                    valueColumn="valueColumn"),
                whereconditions="",
                order = "",
                SQLquery=sqlstr)

		flm <- new("FLMatrix",
                           select= tblfunqueryobj,
                           dim=c(ncol(x),ncol(y)),
		            dimnames = list(
                                colnames(x),
                                colnames(y)))
		return(ensureQuerySize(pResult=flm,
							pInput=list(x,y),
							pOperator="cor"))
    }
    if(is.data.frame(y))
	{
		y <- as.matrix(y)
		if(is.numeric(y)) 
		{ 
			y<-as.FLMatrix(y)
			return(cor(x,y))
		}
		else stop("only numeric entries for correlation")
	}
	if(is.vector(y))
	{
		if(nrow(x)!=length(y)) stop(" incompatible dimensions ")
		else 
		{
			y <- matrix(y,length(y),1)
			y <- as.FLMatrix(y)
			return(cor(x,y))
		}
	}
	if(is.matrix(y))
	{
		if(nrow(x)!=nrow(y)) stop(" incompatible dimensions ")
		else
		{
			y <- as.FLMatrix(y)
			return(cor(x,y))
		}
	}
	if(is.FLVector(y))
	{
		if(length(y) != nrow(x)) stop("incompatible dimensions")
		if(nrow(y)==1 && !y@isDeep)
		{
			y <- as.FLMatrix(y,
                                         sparse=TRUE,rows=length(y),cols=1)
			return(cor(x,y))
		}
		else if(ncol(y)==1 || y@isDeep)
		{
			a <- genRandVarName()
			b <- genRandVarName()
			sqlstr <- paste0("SELECT '%insertIDhere%' AS MATRIX_ID,",
									a,".colIdColumn AS rowIdColumn,
									 1 AS colIdColumn,",
								 x@select@database,".FLCorrel(",a,".valueColumn,",
								 					  b,".vectorValueColumn) AS valueColumn 
							FROM ( ",constructSelect(x),") AS ",a,
			                  ",( ",constructSelect(y),") AS ",b,
	            			constructWhere(c(paste0(a,".rowIdColumn = ",b,".vectorIndexColumn"))),
	            			" GROUP BY ",a,".colIdColumn")

			tblfunqueryobj <- new("FLTableFunctionQuery",
                    connection = connection,
                    variables=list(
                        rowIdColumn="rowIdColumn",
                        colIdColumn="colIdColumn",
                        valueColumn="valueColumn"),
                    whereconditions="",
                    order = "",
                    SQLquery=sqlstr)

			flm <- new("FLMatrix",
                       select= tblfunqueryobj,
                       dim=c(ncol(x),1),
			            dimnames = list(
                          colnames(x),
                          "1"))

			return(ensureQuerySize(pResult=flm,
							pInput=list(x,y),
							pOperator="cor"))
		}
	}
	if(is.FLTable(y))
	{
		if(nrow(x)!=nrow(y)) stop(" incompatible dimensions ")
		else 
		return(t(cor(y,x)))
	}
}

#' @export
cor.numeric <- function(x,y=x,...)
{
	if(is.FLMatrix(y))
	{
		res<- t(cor(y,x))
		return (res)
	}
	else if(is.FLVector(y))
	{
		if(length(y) == length(x))
		{
			x <- matrix(x,length(x),1)
			x <- as.FLMatrix(x)
			return (cor(x,y))
		}
		else stop(" incompatible dimensions ")
	}
	else if(is.FLTable(y))
	{
		if(length(x)!=nrow(y)) stop(" incompatible dimensions ")
		else 
		return(t(cor(y,x)))
	}
	else
	return(cor.default(x,y))
}

#' @export
cor.matrix <- function(x,y=x,...)
{
	if(is.FLMatrix(y))
	{
		res<- t(cor(y,x))
		return (res)
	}
	else if(is.FLVector(y))
	{
		if(length(y) != nrow(x)) stop(" incompatible dimensions ")
		else
		{
			x <- as.FLMatrix(x)
			return (cor(x,y))
		}
	}
	else if(is.FLTable(y))
	{
		if(nrow(x)!=nrow(y)) stop(" incompatible dimensions ")
		else 
		return(t(cor(y,x)))
	}
	else
	return(cor.default(x,y))
}

#' @export
cor.data.frame <- function(x,y=x,...)
{
	if(is.FLMatrix(y))
	{
		res<- t(cor(y,x))
		return (res)
	}
	else if(is.FLVector(y))
	{
		if(length(y) != nrow(x)) stop(" incompatible dimensions ")
		else
		{
			x <- as.matrix(x)
			x <- as.FLMatrix(x)
			return (cor(x,y))
		}
	}
	else if(is.FLTable(y))
	{
		if(nrow(x)!=nrow(y)) stop(" incompatible dimensions ")
		else 
		return(t(cor(y,x)))
	}
	else
	return(cor.default(x,y))
}

#' @export
cor.FLVector <- function(x,y=x,...)
{	
	connection <- getConnection(x)

	if(is.FLVector(y))
	{
		if(length(x)!=length(y)) stop(" incompatible dimensions ")
		if(nrow(y)==1) y <- as.FLVector(as.vector(y))
		if(nrow(x)==1) x <- as.FLVector(as.vector(x))

			a <- genRandVarName()
			b <- genRandVarName()
			sqlstr <- paste0("SELECT '%insertIDhere%' AS MATRIX_ID,",
										"1 AS rowIdColumn,",
										"1 AS colIdColumn,",
									 x@select@database,".FLCorrel(",a,".vectorValueColumn,",
									 					  b,".vectorValueColumn) AS valueColumn 
								FROM ( ",constructSelect(x),") AS ",a,
				                  ",( ",constructSelect(y),") AS ",b,
		            			constructWhere(c(paste0(a,".vectorIndexColumn = ",b,".vectorIndexColumn"))))


			return(sqlQuery(connection,sqlstr)[["valueColumn"]])
	}
	if(is.FLMatrix(y))
	{
		res<- t(cor(y,x))
		return (res)
	}
	if(is.matrix(y))
	{
		if(length(x) == nrow(y))
		{
			res<- t(cor(y,x))
			return (res)
		}
		else stop(" invalid dimensions ")
	}
	if(is.vector(y))
	{
		if(length(x) == length(y))
		{
			res<- t(cor(y,x))
			return (res)
		}
		else stop(" invalid dimensions ")
	}
	if(is.data.frame(y))
	{
		if(length(x) == nrow(y))
		{
			res<- t(cor(y,x))
			return (res)
		}
		else stop(" invalid dimensions ")
	}
	if(is.FLTable(y))
	{
		if(length(x)!=nrow(y)) { stop(" invalid dimensions ") }
		else 
		return(t(cor(y,x)))
	}
}

#' @export
cor.FLTable <- function(x,y=x,...)
{
	connection <- getConnection(x)
	if(is.FLTable(y))
	{
		if(nrow(x) != nrow(y)) stop("incompatible dimensions")

		if(y@isDeep && x@isDeep)
		{
			a <- genRandVarName()
			b <- genRandVarName()
			sqlstr <- paste0("SELECT '%insertIDhere%' AS MATRIX_ID,",
										a,".var_id_colname AS rowIdColumn,",
										b,".var_id_colname AS colIdColumn,",
									 x@select@database,".FLCorrel(",a,".cell_val_colname,",
									 					  b,".cell_val_colname) AS valueColumn 
								FROM ( ",constructSelect(x),") AS ",a,
				                  ",( ",constructSelect(y),") AS ",b,
		            			constructWhere(c(paste0(a,".obs_id_colname = ",b,".obs_id_colname"))),
		            			" GROUP BY ",a,".var_id_colname,",b,".var_id_colname")

			tblfunqueryobj <- new("FLTableFunctionQuery",
                    connection = connection,
                    variables=list(
                        rowIdColumn="rowIdColumn",
                        colIdColumn="colIdColumn",
                        valueColumn="valueColumn"),
                    whereconditions="",
                    order = "",
                    SQLquery=sqlstr)

			flm <- new("FLMatrix",
			            select= tblfunqueryobj,
                       dim=c(ncol(x),ncol(y)),
			            dimnames = list(
                          colnames(x),
                          colnames(y)))

			return(ensureQuerySize(pResult=flm,
							pInput=list(x,y),
							pOperator="cor"))
		}
		if(!y@isDeep && !x@isDeep)
		{
			deepx <- wideToDeep(x)
			deepy <- wideToDeep(y)
			x <- deepx[["table"]]
			y <- deepy[["table"]]
			
			flm <- cor(x,y)
			# varnamesx <- sqlQuery(connection,
			# 					  paste0(" SELECT COLUMN_NAME, Final_VarID 
			# 					  		   FROM fzzlRegrDataPrepMap 
			# 					  		   WHERE AnalysisID = '",deepx[["AnalysisID"]],"' 
			# 	                		   AND Final_VarID IS NOT NULL 
			# 	                		   ORDER BY Final_VarID"))[,c("COLUMN_NAME","Final_VarID")]
			# rownames <- varnamesx[charmatch(rownames(flm),varnamesx[["Final_VarID"]]),"COLUMN_NAME"]
			# varnamesy <- sqlQuery(getConnection(y),
			# 					  paste0(" 	SELECT COLUMN_NAME ,Final_VarID 
			# 					  			FROM fzzlRegrDataPrepMap 
			# 					  			WHERE AnalysisID = '",deepy[["AnalysisID"]],"' 
			# 	                			AND Final_VarID IS NOT NULL 
			# 	                			ORDER BY Final_VarID"))[,c("COLUMN_NAME","Final_VarID")]
			# colnames <- varnamesy[charmatch(colnames(flm),varnamesy[["Final_VarID"]]),"COLUMN_NAME"]
			# # correlmat <- matrix(vec,ncolx,byrow=T,dimnames=list(varnamesx,varnamesy))
			# ##Phani-- names mapping needs to be implemented for this to work
			# flm@dimnames <- list(rownames,colnames)
			return(flm)
		}
		if(y@isDeep && !x@isDeep)
		{
			deepx <- wideToDeep(x)
			x <- deepx[["table"]]
			flm <- cor(x,y)
			varnamesx <- sqlQuery(connection,
								  paste0(" SELECT COLUMN_NAME, Final_VarID 
								  		   FROM fzzlRegrDataPrepMap 
								  		   WHERE AnalysisID = '",deepx[["AnalysisID"]],"' 
				                		   AND Final_VarID IS NOT NULL 
				                		   ORDER BY Final_VarID"))[,c("COLUMN_NAME","Final_VarID")]
			rownames <- varnamesx[charmatch(rownames(flm),varnamesx[["Final_VarID"]]),"COLUMN_NAME"]
			# correlmat <- matrix(vec,ncol(x),byrow=T,dimnames=list(varnamesx,c()))
			flm@dimnames[[1]] <- rownames 
			return(flm)
		}
		if(!y@isDeep && x@isDeep)
		return ( t(cor(y,x)))
	}
	
	if(is.FLMatrix(y))
	{
		if(nrow(x) != nrow(y)) stop("incompatible dimensions")
		if(x@isDeep)
		{
			a <- genRandVarName()
			b <- genRandVarName()
			sqlstr <- paste0("SELECT '%insertIDhere%' AS MATRIX_ID,",
										a,".var_id_colname AS rowIdColumn,",
										b,".colIdColumn AS colIdColumn,",
									 x@select@database,".FLCorrel(",a,".cell_val_colname,",
									 					  b,".valueColumn) AS valueColumn 
								FROM ( ",constructSelect(x),") AS ",a,
				                  ",( ",constructSelect(y),") AS ",b,
		            			constructWhere(c(paste0(a,".obs_id_colname = ",b,".rowIdColumn"))),
		            			" GROUP BY ",a,".var_id_colname,",b,".colIdColumn")

			tblfunqueryobj <- new("FLTableFunctionQuery",
                    connection = connection,
                    variables=list(
                        rowIdColumn="rowIdColumn",
                        colIdColumn="colIdColumn",
                        valueColumn="valueColumn"),
                    whereconditions="",
                    order = "",
                    SQLquery=sqlstr)

			flm <- new("FLMatrix",
			            select= tblfunqueryobj,
                       dim=c(ncol(x),ncol(y)),
			            dimnames = list(
                          colnames(x),
                          colnames(y)))

			return(ensureQuerySize(pResult=flm,
							pInput=list(x,y),
							pOperator="cor"))
		}
		if(!x@isDeep)
		{
			deepx <- wideToDeep(x)
			x <- deepx[["table"]]
			flm <- cor(x,y)
			varnamesx <- sqlQuery(connection,
								  paste0(" SELECT COLUMN_NAME, Final_VarID 
								  		   FROM fzzlRegrDataPrepMap 
								  		   WHERE AnalysisID = '",deepx[["AnalysisID"]],"' 
				                		   AND Final_VarID IS NOT NULL 
				                		   ORDER BY Final_VarID"))[,c("COLUMN_NAME","Final_VarID")]
			rownames <- varnamesx[charmatch(rownames(flm),varnamesx[["Final_VarID"]]),"COLUMN_NAME"]
			# correlmat <- matrix(vec,ncol(x),byrow=T,dimnames=list(varnamesx,c()))
			flm@dimnames[[1]] <- rownames 
			return(flm)
			# varnamesx <- sqlQuery(getConnection(x),
			# 					  paste0(" 	SELECT COLUMN_NAME 
			# 					  			FROM fzzlRegrDataPrepMap 
			# 					  			WHERE AnalysisID = '",dataprepIDx,"' 
			# 	                			AND Final_VarID IS NOT NULL 
			# 	                			ORDER BY Final_VarID"))[,1]
			# correlmat <- matrix(vec,ncol(x),byrow=T,dimnames=list(varnamesx,c()))
			# return(correlmat)
		}
	}

	if(is.FLVector(y))
	{
		if(length(y) != nrow(x)) stop("incompatible dimensions")
		if(nrow(y)==1 && !y@isDeep)
		{
			y <- as.FLMatrix(y,
                                         sparse=TRUE,rows=length(y),cols=1)
			return(cor(x,y))
		}
		else if(ncol(y)==1 || y@isDeep)
		{
			a <- genRandVarName()
			b <- genRandVarName()
			if(!x@isDeep)
			{
				deepx <- wideToDeep(x)
				x <- deepx[["table"]]
				varnamesx <- sqlQuery(connection,
								  paste0(" SELECT COLUMN_NAME, Final_VarID 
								  		   FROM fzzlRegrDataPrepMap 
								  		   WHERE AnalysisID = '",deepx[["AnalysisID"]],"' 
				                		   AND Final_VarID IS NOT NULL 
				                		   ORDER BY Final_VarID"))[,c("COLUMN_NAME","Final_VarID")]
			}
			else varnamesx <- NULL
			sqlstr <- paste0("SELECT '%insertIDhere%' AS MATRIX_ID,",
									a,".var_id_colname AS rowIdColumn,
									 1 AS colIdColumn,",
								 x@select@database,".FLCorrel(",a,".cell_val_colname,",
								 					  b,".vectorValueColumn) AS valueColumn 
							FROM ( ",constructSelect(x),") AS ",a,
			                  ",( ",constructSelect(y),") AS ",b,
	            			constructWhere(c(paste0(a,".obs_id_colname = ",b,".vectorIndexColumn"))),
	            			" GROUP BY ",a,".var_id_colname")

			tblfunqueryobj <- new("FLTableFunctionQuery",
                    connection = connection,
                    variables=list(
                        rowIdColumn="rowIdColumn",
                        colIdColumn="colIdColumn",
                        valueColumn="valueColumn"),
                    whereconditions="",
                    order = "",
                    SQLquery=sqlstr)

			
			flm <- new("FLMatrix",
                       select= tblfunqueryobj,
                       dim=c(ncol(x),1),
                       dimnames = list(
                           colnames(x),
                           "1"))

			if(!is.null(varnamesx))
			flm@dimnames[[1]] <- varnamesx[charmatch(rownames(flm),varnamesx[["Final_VarID"]]),"COLUMN_NAME"]

			return(ensureQuerySize(pResult=flm,
							pInput=list(x,y),
							pOperator="cor"))
		}
	}
	if(is.matrix(y))
	{
		if(nrow(x) == nrow(y))
		{
			y <- as.FLMatrix(y)
			return (cor(x,y))
		}
		else stop(" incompatible dimensions ")
	}
	if(is.numeric(y))
	{
		if(nrow(x) == length(y))
		{
			y <- as.FLMatrix(as.matrix(y))
			return (cor(x,y))
		}
		else stop(" incompatible dimensions ")
	}
	if(is.data.frame(y))
	{
		if(nrow(x) == nrow(y))
		{
			y <- as.FLMatrix(as.matrix(y))
			return (cor(x,y))
		}
		else stop(" incompatible dimensions ")
	}
}

