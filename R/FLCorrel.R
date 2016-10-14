#' @include FLMatrix.R
#' @include FLStatFunctions.R
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
#' Only methods c("pearson","spearman","shuffle") are supported.
#' @return \code{cor} returns FLMatrix object representing correlation of x and y.
#' @examples
#' deeptable <- FLTable( 
#' "tblUSArrests", "ObsID","VarID","Num_Val")
#' widetable <- FLTable("tblAbaloneWide","ObsID")
#' cor(deeptable,deeptable)
#' cor(widetable,widetable)
#' @export
cor <- function(x,y=NULL,use="everything",
				method="pearson",...){
	UseMethod("cor",x)
}

#' @export
cor.default <- stats::cor

#' @export
cor.FLMatrix <- function(x,y=NULL,use="everything",
				method="pearson",...){
	return(FLCorGeneric(x=x,y=y,
						functionName="FLCorrel",
						method=method,...))
	}
#' @export
cor.numeric <- function(x,y=NULL,use="everything",
						method="pearson",...)
return(FLCorGeneric(x=x,y=y,
					functionName="FLCorrel",
					method=method,...))
#' @export
cov.matrix <- function(x,y=NULL,use="everything",
						method="pearson",...)
return(FLCorGeneric(x=x,y=y,
					functionName="FLCorrel",
					method=method,...))
#' @export
cor.data.frame <- function(x,y=NULL,use="everything",
							method="pearson",...)
return(FLCorGeneric(x=x,y=y,
					functionName="FLCorrel",
					method=method,...))
#' @export
cor.FLVector <- function(x,y=NULL,use="everything",
						method="pearson",...)
return(FLCorGeneric(x=x,y=y,
					functionName="FLCorrel",
					method=method,...))
#' @export
cor.FLTable <- function(x,y=NULL,use="everything",
						method="pearson",...)
return(FLCorGeneric(x=x,y=y,
					functionName="FLCorrel",
					method="pearson",...))

#' CoVariance.
#'
#' \code{cov} computes correlation of in-database Objects
#'
#' @param x FLMatrix, FLVector or FLTable object or any R object
#' @param y FLMatrix, FLVector or FLTable object or any R object
#' @param ... any additional arguments
#' @section Constraints:
#' The number of non-null pairs must be greater than or equal to 2.
#' If number of non-null pairs is less than 2, FLCorrel returns a NULL.
#' @return \code{cov} returns FLMatrix object representing correlation of x and y.
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' deeptable <- FLTable("tblUSArrests", "ObsID","VarID","Num_Val")
#' widetable <- FLTable("tblAbaloneWide","ObsID")
#' cov(deeptable,deeptable)
#' cov(widetable,widetable)
#' @export
cov <- function(x,y=NULL,use="everything",
				method="pearson",...){
	UseMethod("cov",x)
}

#' @export
cov.default <- stats::cov

#' @export
cov.FLMatrix <- function(x,y=NULL,use="everything",
						method="pearson",...)
return(FLCorGeneric(x=x,y=y,
					functionName="FLCovar",
					method=method,...))
#' @export
cov.numeric <- function(x,y=NULL,use="everything",
						method="pearson",...)
return(FLCorGeneric(x=x,y=y,
					functionName="FLCovar",
					method=method,...))
#' @export
cov.matrix <- function(x,y=NULL,use="everything",
						method="pearson",...)
return(FLCorGeneric(x=x,y=y,
					functionName="FLCovar",
					method=method,...))
#' @export
cov.data.frame <- function(x,y=NULL,use="everything",
							method="pearson",...)
return(FLCorGeneric(x=x,y=y,
					functionName="FLCovar",
					method=method,...))
#' @export
cov.FLVector <- function(x,y=NULL,use="everything",
						method="pearson",...)
return(FLCorGeneric(x=x,y=y,
					functionName="FLCovar",
					method=method,...))
#' @export
cov.FLTable <- function(x,y=NULL,use="everything",
						method="pearson",...)
return(FLCorGeneric(x=x,y=y,
					functionName="FLCovar",
					method=method,...))

#' Population CoVariance.
#'
#' \code{cov} computes correlation of in-database Objects
#'
#' @param x FLMatrix, FLVector or FLTable object or any R object
#' @param y FLMatrix, FLVector or FLTable object or any R object
#' @param ... any additional arguments
#' @section Constraints:
#' The number of non-null pairs must be greater than or equal to 2.
#' If number of non-null pairs is less than 2, FLCorrel returns a NULL.
#' @return \code{cov} returns FLMatrix object representing correlation of x and y.
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' deeptable <- FLTable("tblUSArrests", "ObsID","VarID","Num_Val")
#' widetable <- FLTable("tblAbaloneWide","ObsID")
#' FLCovarP(deeptable,deeptable)
#' FLCovarP(widetable,widetable)
#' @export
FLCovarP <- function(x,y=NULL,
					use="everything",
					method="pearson",...){
	UseMethod("FLCovarP",x)
}

#' @export
FLCovarP.FLMatrix <- function(x,y=NULL,
							use="everything",
							method="pearson",...)
return(FLCorGeneric(x=x,y=y,
					functionName="FLCovarP",
					method=method,...))
#' @export
FLCovarP.numeric <- function(x,y=NULL,use="everything",
							method="pearson",...)
return(FLCorGeneric(x=x,y=y,
					functionName="FLCovarP",
					method=method,...))
#' @export
FLCovarP.matrix <- function(x,y=NULL,use="everything",
							method="pearson",...)
return(FLCorGeneric(x=x,y=y,
					functionName="FLCovarP",
					method=method,...))
#' @export
FLCovarP.data.frame <- function(x,y=NULL,use="everything",
								method="pearson",...)
return(FLCorGeneric(x=x,y=y,
					functionName="FLCovarP",
					method=method,...))
#' @export
FLCovarP.FLVector <- function(x,y=NULL,use="everything",
							method="pearson",...)
return(FLCorGeneric(x=x,y=y,
					functionName="FLCovarP",
					method=method,...))
#' @export
FLCovarP.FLTable <- function(x,y=NULL,use="everything",
							method="pearson",...)
return(FLCorGeneric(x=x,y=y,
					functionName="FLCovarP",
					method=method,...))


#' variance.
#'
#' \code{cov} computes correlation of in-database Objects
#'
#' @param x FLMatrix, FLVector or FLTable object or any R object
#' @param y FLMatrix, FLVector or FLTable object or any R object
#' @param ... any additional arguments
#' @section Constraints:
#' The number of non-null pairs must be greater than or equal to 2.
#' If number of non-null pairs is less than 2, FLCorrel returns a NULL.
#' @return \code{cov} returns FLMatrix object representing correlation of x and y.
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' deeptable <- FLTable("tblUSArrests", "ObsID","VarID","Num_Val")
#' widetable <- FLTable("tblAbaloneWide","ObsID")
#' FLCovarP(deeptable,deeptable)
#' FLCovarP(widetable,widetable)
#' @export
var <- function(x,y=NULL,...){
	UseMethod("var",x)
}
#' @export
var.default <- stats::var

#' @export
var.FLMatrix <- function(x,y=NULL,...)
return(FLCorGeneric(x=x,y=y,functionName="FLCovar",...))
#' @export
var.numeric <- function(x,y=NULL,...){
	if(missing(y))
	return(var.default(x,...))
	else if(!is.FLVector(y) && !is.FLMatrix(y) && !is.FLTable(y))
	return(var.default(x,y,...))
	else return(FLCorGeneric(x=x,y=y,functionName="FLCovar",...))
}
#' @export
var.matrix <- function(x,y=NULL,...){
	if(missing(y))
	return(var.default(x,...))
	else if(!is.FLVector(y) && !is.FLMatrix(y) && !is.FLTable(y))
	return(var.default(x,y,...))
	else return(FLCorGeneric(x=x,y=y,functionName="FLCovar",...))
}
#' @export
var.data.frame <- function(x,y=NULL,...){
	if(missing(y))
	return(var.default(x,...))
	else if(!is.FLVector(y) && !is.FLMatrix(y) && !is.FLTable(y))
	return(var.default(x,y,...))
	else return(FLCorGeneric(x=x,y=y,functionName="FLCovar",...))
}
#' @export
var.FLSimpleVector <- function(x,y=NULL,...){
	if(missing(y)){
            return(genAggregateFunCall(object=x,fun=FLaggregate,FLfun="FLVar"))
	}
	else return(FLCorGeneric(x=x,y=y,functionName="FLCovar",...))
}
#' @export
var.FLVector <- function(x,y=NULL,...){
	if(missing(y)){
            return(genAggregateFunCall(object=x,fun=FLaggregate,FLfun="FLVar"))
	}
	else return(FLCorGeneric(x=x,y=y,functionName="FLCovar",...))
}
#' @export
var.FLTable <- function(x,y=NULL,...)
return(FLCorGeneric(x=x,y=y,functionName="FLCovar",...))

#' population variance.
#'
#' \code{cov} computes correlation of in-database Objects
#'
#' @param x FLMatrix, FLVector or FLTable object or any R object
#' @param y FLMatrix, FLVector or FLTable object or any R object
#' @param ... any additional arguments
#' @section Constraints:
#' The number of non-null pairs must be greater than or equal to 2.
#' If number of non-null pairs is less than 2, FLCorrel returns a NULL.
#' @return \code{cov} returns FLMatrix object representing correlation of x and y.
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' deeptable <- FLTable("tblUSArrests", "ObsID","VarID","Num_Val")
#' widetable <- FLTable("tblAbaloneWide","ObsID")
#' FLCovarP(deeptable,deeptable)
#' FLCovarP(widetable,widetable)
#' @export
FLVarP <- function(x,y=NULL,...){
	UseMethod("FLVarP",x)
}

varP <- function(x,...){
	if(is.vector(x))
		n <- length(x)
		else n <- nrow(x)
		return((stats::var(x,...)*(n-1))/n)
}
#' @export
FLVarP.FLMatrix <- function(x,y=NULL,...)
return(FLCorGeneric(x=x,y=y,functionName="FLCovarP",...))
#' @export
FLVarP.numeric <- function(x,y=NULL,...){
	if(missing(y))
	return(varP(x,...))
	else return(FLCorGeneric(x=x,y=y,functionName="FLCovarP",...))
}
#' @export
FLVarP.matrix <- function(x,y=NULL,...){
	if(missing(y))
	return(varP(x,...))
	else return(FLCorGeneric(x=x,y=y,functionName="FLCovarP",...))
}
#' @export
FLVarP.data.frame <- function(x,y=NULL,...){
	if(missing(y))
	return(varP(x,...))
	else return(FLCorGeneric(x=x,y=y,functionName="FLCovarP",...))
}
#' @export
FLVarP.FLAbstractColumn <- function(object){
	return(paste0(" FLVarP(",
				paste0(object@columnName,collapse=","),") "))
}
#' @export
FLVarP.FLVector <- function(x,y=NULL,...){
	if(missing(y)){
		if(ncol(x)>1 && !x@isDeep)
		x <- as.FLVector(as.vector(x))
		return(genAggregateFunCall(x,FLVarP.FLAbstractColumn))
	}
	else return(FLCorGeneric(x=x,y=y,functionName="FLCovarP",...))
}
#' @export
FLVarP.FLTable <- function(x,y=NULL,...)
return(FLCorGeneric(x=x,y=y,functionName="FLCovarP",...))


## Generic function to cover FLCorrel,FLCovar,FLCovarP
#' @export
FLCorGeneric <- function (x,y=NULL,functionName,method="pearson",...) {
	UseMethod("FLCorGeneric", x)
}

#' @export
FLCorGeneric.default <- function(x,y=NULL,
								functionName,
								method="pearson",...){
	if(functionName=="FLCorrel")
	return(stats::cor(x,y,...))
	else if(functionName=="FLCovar")
	return(stats::cov(x,y,...))
	else if(functionName=="FLCovarP"){
		if(is.vector(x))
		n <- length(x)
		else n <- nrow(x)
		return((stats::cov(x,y,...)*(n-1))/n)
		}
}

#' @export
FLCorGeneric.FLMatrix <- function(x,y=NULL,
								functionName,
								method="pearson",...)
{
	if(is.null(y))
	y <- x
	connection <- getFLConnection(x)
	pStoreResult <- FALSE
    ##browser()
    if(is.FLMatrix(y))
    {
    	if(nrow(x)!=nrow(y)) stop("incompatible dimensions")

        a <- genRandVarName()
		b <- genRandVarName()
		sqlstr <- genCorrelUDTSql(object1=x,
								object2=y,
								functionName=functionName,
								method=method)
		vstoreFlag <- ifelse(is.null(sqlstr),FALSE,TRUE)
		if(is.null(sqlstr))
		sqlstr <- paste0("SELECT '%insertIDhere%' AS MATRIX_ID,",
								a,".",getIndexSQLName(x,2)," AS rowIdColumn,",
								b,".",getIndexSQLName(y,2)," AS colIdColumn,",
                                 functionName,"(",a,".",getValueSQLName(x),",",
                                 b,".",getValueSQLName(y),") AS valueColumn 
						FROM ( ",constructSelect(x),") AS ",a,
		                  ",( ",constructSelect(y),") AS ",b,
                                 constructWhere(c(paste0(a,".",getIndexSQLName(x,1)," = ",
                                                         b,".",getIndexSQLName(y,1),""))),
            			" GROUP BY ",a,".",getIndexSQLName(x,2),",",b,".",getIndexSQLName(y,2))

		tblfunqueryobj <- new("FLTableFunctionQuery",
                connectionName = attr(connection,"name"),
                variables=list(
                    rowIdColumn="rowIdColumn",
                    colIdColumn="colIdColumn",
                    valueColumn="valueColumn"),
                whereconditions="",
                order = "",
                SQLquery=sqlstr)

		flm <- newFLMatrix(
                           select= tblfunqueryobj,
                           dims=as.integer(c(ncol(x),ncol(y))),
		            Dimnames = list(
                                colnames(x),
                                colnames(y)))
		return(ensureQuerySize(pResult=flm,
							pInput=list(x,y,functionName,...),
							pOperator="FLCorGeneric",
							pStoreResult=vstoreFlag))
    }
    if(is.data.frame(y))
	{
		y <- as.matrix(y)
		if(is.numeric(y)) 
		{ 
			y<-as.FLMatrix(y)
			return(FLCorGeneric(x=x,y=y,
						functionName=functionName,
						method=method,...))
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
			return(FLCorGeneric(x=x,y=y,
						functionName=functionName,
						method=method,...))
		}
	}
	if(is.matrix(y))
	{
		if(nrow(x)!=nrow(y)) stop("incompatible dimensions\n")
		else
		{
			y <- as.FLMatrix(y)
			return(FLCorGeneric(x=x,y=y,
						functionName=functionName,
						method=method,...))
		}
	}
	if(is.FLVector(y))
	{
		if(length(y) != nrow(x)) stop("incompatible dimensions\n")
		if(nrow(y)==1 && !y@isDeep)
		{
			y <- as.FLMatrix(y,
                            sparse=TRUE,rows=length(y),cols=1)
			return(FLCorGeneric(x=x,y=y,
						functionName=functionName,
						method=method,...))
		}
		else if(ncol(y)==1 || y@isDeep)
		{
			a <- genRandVarName()
			b <- genRandVarName()
			sqlstr <- genCorrelUDTSql(object1=x,
								object2=y,
								functionName=functionName,
								method=method)
			vstoreFlag <- ifelse(is.null(sqlstr),FALSE,TRUE)
			if(is.null(sqlstr))
			sqlstr <- paste0("SELECT '%insertIDhere%' AS MATRIX_ID,",
									a,".",getIndexSQLName(x,2)," AS rowIdColumn,
									 1 AS colIdColumn,",
                                        functionName,"(",a,".",getValueSQLName(x),",",
                                         b,".vectorValueColumn) AS valueColumn 
							FROM ( ",constructSelect(x),") AS ",a,
			                  ",( ",constructSelect(y),") AS ",b,
	            			constructWhere(c(paste0(a,".",getIndexSQLName(x,1)," = ",
                                                                b,".vectorIndexColumn"))),
	            			" GROUP BY ",a,".",getIndexSQLName(x,2))

			tblfunqueryobj <- new("FLTableFunctionQuery",
                    connectionName = attr(connection,"name"),
                    variables=list(
                        rowIdColumn="rowIdColumn",
                        colIdColumn="colIdColumn",
                        valueColumn="valueColumn"),
                    whereconditions="",
                    order = "",
                    SQLquery=sqlstr)

			flm <- newFLMatrix(
                       select= tblfunqueryobj,
                       dims=as.integer(c(ncol(x),1)),
			            Dimnames = list(
                          colnames(x),
                          "1"))

			return(ensureQuerySize(pResult=flm,
							pInput=list(x,y,functionName,...),
							pOperator="FLCorGeneric",
							pStoreResult=vstoreFlag))
		}
	}
	if(is.FLTable(y))
	{
		if(nrow(x)!=nrow(y)) stop(" incompatible dimensions\n")
		else 
		return(t(return(FLCorGeneric(x=y,y=x,
						functionName=functionName,
						method=method,...))))
	}
}

#' @export
FLCorGeneric.numeric <- function(x,y=NULL,
								functionName,
								method="pearson",...)
{
	if(is.null(y))
	y <- x
	if(is.FLMatrix(y))
	{
		res<- t(FLCorGeneric(x=y,y=x,
						functionName=functionName,
						method=method,...))
		return (res)
	}
	else if(is.FLVector(y))
	{
		if(length(y) == length(x))
		{
			x <- matrix(x,length(x),1)
			x <- as.FLMatrix(x)
			return(FLCorGeneric(x=x,y=y,
						functionName=functionName,
						method=method,...))
		}
		else stop(" incompatible dimensions ")
	}
	else if(is.FLTable(y))
	{
		if(length(x)!=nrow(y)) stop(" incompatible dimensions ")
		else 
		return(t(FLCorGeneric(x=y,y=x,
						functionName=functionName,
						method=method,...)))
	}
	else
	return(FLCorGeneric.default(x,y,functionName,...))
}

#' @export
FLCorGeneric.matrix <- function(x,y=NULL,
								functionName,
								method="pearson",...)
{
	if(is.null(y))
	y <- x

	if(is.FLMatrix(y))
	{
		res<- t(FLCorGeneric(x=y,y=x,
						functionName=functionName,
						method=method,...))
		return (res)
	}
	else if(is.FLVector(y))
	{
		if(length(y) != nrow(x)) stop(" incompatible dimensions ")
		else
		{
			x <- as.FLMatrix(x)
			return(FLCorGeneric(x=x,y=y,
						functionName=functionName,
						method=method,...))
		}
	}
	else if(is.FLTable(y))
	{
		if(nrow(x)!=nrow(y)) stop(" incompatible dimensions ")
		else 
		return(t(FLCorGeneric(x=y,y=x,
						functionName=functionName,
						method=method,...)))
	}
	else
	return(FLCorGeneric.default(x,y,functionName,...))
}

#' @export
FLCorGeneric.data.frame <- function(x,y=NULL,
									functionName,
									method="pearson",...)
{
	if(is.null(y))
	y <- x

	if(is.FLMatrix(y))
	{
		res<- t(FLCorGeneric(x=y,y=x,
						functionName=functionName,
						method=method,...))
		return (res)
	}
	else if(is.FLVector(y))
	{
		if(length(y) != nrow(x)) stop(" incompatible dimensions ")
		else
		{
			x <- as.matrix(x)
			x <- as.FLMatrix(x)
			return(FLCorGeneric(x=x,y=y,
						functionName=functionName,
						method=method,...))
		}
	}
	else if(is.FLTable(y))
	{
		if(nrow(x)!=nrow(y)) stop(" incompatible dimensions ")
		else 
		return(t(FLCorGeneric(x=y,y=x,
						functionName=functionName,
						method=method,...)))
	}
	else
	return(FLCorGeneric.default(x,y,functionName,...))
}

#' @export
FLCorGeneric.FLVector <- function(x,y=NULL,
								functionName,
								method="pearson",...)
{	
	if(is.null(y))
	y <- x

	connection <- getFLConnection(x)

	if(is.FLVector(y))
	{
		if(length(x)!=length(y)) stop(" incompatible dimensions ")
		if(nrow(y)==1 && !y@isDeep) y <- as.FLVector(as.vector(y))
		if(nrow(x)==1 && !x@isDeep) x <- as.FLVector(as.vector(x))

			a <- genRandVarName()
			b <- genRandVarName()
			sqlstr <- genCorrelUDTSql(object1=x,
								object2=y,
								functionName=functionName,
								method=method)
			vstoreFlag <- ifelse(is.null(sqlstr),FALSE,TRUE)
			if(is.null(sqlstr))
			sqlstr <- paste0("SELECT ",
                                         functionName,"(",a,".vectorValueColumn,",
                                         b,".vectorValueColumn) AS valueColumn 
								FROM ( ",constructSelect(x),") AS ",a,
                                         ",( ",constructSelect(y),") AS ",b,
                                         constructWhere(c(paste0(a,".vectorIndexColumn = ",b,".vectorIndexColumn"))))


			return(sqlQuery(connection,sqlstr)[["valueColumn"]])
	}
	if(is.FLMatrix(y))
	{
		res<- t(FLCorGeneric(x=y,y=x,
						functionName=functionName,
						method=method,...))
		return (res)
	}
	if(is.matrix(y))
	{
		if(length(x) == nrow(y))
		{
			res<- t(FLCorGeneric(x=y,y=x,
						functionName=functionName,
						method=method,...))
			return (res)
		}
		else stop(" invalid dimensions ")
	}
	if(is.vector(y))
	{
		if(length(x) == length(y))
		{
			res<- t(FLCorGeneric(x=y,y=x,
						functionName=functionName,
						method=method,...))
			return (res)
		}
		else stop(" invalid dimensions ")
	}
	if(is.data.frame(y))
	{
		if(length(x) == nrow(y))
		{
			res<- t(FLCorGeneric(x=y,y=x,
						functionName=functionName,
						method=method,...))
			return (res)
		}
		else stop(" invalid dimensions ")
	}
	if(is.FLTable(y))
	{
		if(length(x)!=nrow(y)) { stop(" invalid dimensions ") }
		else 
		return(t(FLCorGeneric(x=y,y=x,
						functionName=functionName,
						method=method,...)))
	}
}

#' @export
FLCorGeneric.FLTable <- function(x,y=NULL,
								functionName,
								method="pearson",...)
{
	if(is.null(y)){
		vnullFlag <- 1
		y <- x
	}
	else vnullFlag <- 0

	connection <- getFLConnection(x)
	if(is.FLTable(y))
	{
		if(nrow(x) != nrow(y)) stop("incompatible dimensions")

		if(y@isDeep && x@isDeep)
		{
			a <- genRandVarName()
			b <- genRandVarName()
			sqlstr <- genCorrelUDTSql(object1=x,
								object2=y,
								functionName=functionName,
								method=method)
			vstoreFlag <- ifelse(is.null(sqlstr),FALSE,TRUE)
			if(is.null(sqlstr))
			sqlstr <- paste0("SELECT '%insertIDhere%' AS MATRIX_ID,",
                                         a,".var_id_colname AS rowIdColumn,",
                                         b,".var_id_colname AS colIdColumn,",
                                         functionName,"(",a,".cell_val_colname,",
                                         b,".cell_val_colname) AS valueColumn 
								FROM ( ",constructSelect(x),") AS ",a,
                                         ",( ",constructSelect(y),") AS ",b,
                                         constructWhere(c(paste0(a,".obs_id_colname = ",b,".obs_id_colname"))),
                                         " GROUP BY ",a,".var_id_colname,",b,".var_id_colname")

			tblfunqueryobj <- new("FLTableFunctionQuery",
                    connectionName = attr(connection,"name"),
                    variables=list(
                        rowIdColumn="rowIdColumn",
                        colIdColumn="colIdColumn",
                        valueColumn="valueColumn"),
                    whereconditions="",
                    order = "",
                    SQLquery=sqlstr)

			flm <- newFLMatrix(
			            select= tblfunqueryobj,
                       dims=as.integer(c(ncol(x),ncol(y))),
			            Dimnames = list(
                          colnames(x),
                          colnames(y)))

			return(ensureQuerySize(pResult=flm,
							pInput=list(x,y,functionName,...),
							pOperator="FLCorGeneric",
							pStoreResult=vstoreFlag))
		}
		if(!y@isDeep && !x@isDeep)
		{
			deepx <- wideToDeep(x)
			x <- deepx[["table"]]

			if(!vnullFlag){
				deepy <- wideToDeep(y)
				y <- deepy[["table"]]
			}
			else{
				deepy <- deepx
				y <- x
			}
			
			flm <-FLCorGeneric(x=x,y=y,
						functionName=functionName,
						method=method,...)
			varnamesx <- sqlQuery(connection,
								  paste0(" SELECT COLUMN_NAME, Final_VarID 
								  		   FROM fzzlRegrDataPrepMap 
								  		   WHERE AnalysisID = '",deepx[["AnalysisID"]],"' 
				                		   AND Final_VarID IS NOT NULL 
				                		   ORDER BY Final_VarID"))[,c("COLUMN_NAME")]
			varnamesy <- sqlQuery(connection,
								  paste0(" SELECT COLUMN_NAME, Final_VarID 
								  		   FROM fzzlRegrDataPrepMap 
								  		   WHERE AnalysisID = '",deepy[["AnalysisID"]],"' 
				                		   AND Final_VarID IS NOT NULL 
				                		   ORDER BY Final_VarID"))[,c("COLUMN_NAME")]

			flm@Dimnames <- list(varnamesx,
								varnamesy)
			return(flm)
		}
		if(y@isDeep && !x@isDeep)
		{
			deepx <- wideToDeep(x)
			x <- deepx[["table"]]
			flm <- FLCorGeneric(x=x,y=y,
						functionName=functionName,
						method=method,...)
			varnamesx <- sqlQuery(connection,
								  paste0(" SELECT COLUMN_NAME, Final_VarID 
								  		   FROM fzzlRegrDataPrepMap 
								  		   WHERE AnalysisID = '",deepx[["AnalysisID"]],"' 
				                		   AND Final_VarID IS NOT NULL 
				                		   ORDER BY Final_VarID"))[,c("COLUMN_NAME","Final_VarID")]
			rownames <- varnamesx[charmatch(rownames(flm),varnamesx[["Final_VarID"]]),"COLUMN_NAME"]
			# correlmat <- matrix(vec,ncol(x),byrow=T,dimnames=list(varnamesx,c()))
			flm@Dimnames[[1]] <- rownames 
			return(flm)
		}
		if(!y@isDeep && x@isDeep)
		return ( t(FLCorGeneric(x=y,y=x,
						functionName=functionName,
						method=method,...)))
	}
	
	if(is.FLMatrix(y))
	{
		if(nrow(x) != nrow(y)) stop("incompatible dimensions")
		if(x@isDeep)
		{
			a <- genRandVarName()
			b <- genRandVarName()
			sqlstr <- genCorrelUDTSql(object1=x,
								object2=y,
								functionName=functionName,
								method=method)
			vstoreFlag <- ifelse(is.null(sqlstr),FALSE,TRUE)
			if(is.null(sqlstr))
			sqlstr <- paste0("SELECT '%insertIDhere%' AS MATRIX_ID,",
                                         a,".var_id_colname AS rowIdColumn,",
                                         b,".",getIndexSQLName(y,2)," AS colIdColumn,",
                                         functionName,"(",a,".cell_val_colname,",
                                         b,".",getValueSQLName(y),") AS valueColumn 
								FROM ( ",constructSelect(x),") AS ",a,
                                         ",( ",constructSelect(y),") AS ",b,
                                         constructWhere(c(paste0(a,".obs_id_colname = ",
                                                                 b,".",getIndexSQLName(y,1)))),
                                         " GROUP BY ",a,".var_id_colname,",b,".",getIndexSQLName(y,2))

			tblfunqueryobj <- new("FLTableFunctionQuery",
                    connectionName = attr(connection,"name"),
                    variables=list(
                        rowIdColumn="rowIdColumn",
                        colIdColumn="colIdColumn",
                        valueColumn="valueColumn"),
                    whereconditions="",
                    order = "",
                    SQLquery=sqlstr)

			flm <- newFLMatrix(
			            select= tblfunqueryobj,
                       dims=as.integer(c(ncol(x),ncol(y))),
			            Dimnames = list(
                          colnames(x),
                          colnames(y)))

			return(ensureQuerySize(pResult=flm,
							pInput=list(x,y,functionName,...),
							pOperator="FLCorGeneric",
							pStoreResult=vstoreFlag))
		}
		if(!x@isDeep)
		{
			deepx <- wideToDeep(x)
			x <- deepx[["table"]]
			flm <- FLCorGeneric(x=x,y=y,
						functionName=functionName,
						method=method,...)
			varnamesx <- sqlQuery(connection,
								  paste0(" SELECT COLUMN_NAME, Final_VarID 
								  		   FROM fzzlRegrDataPrepMap 
								  		   WHERE AnalysisID = '",deepx[["AnalysisID"]],"' 
				                		   AND Final_VarID IS NOT NULL 
				                		   ORDER BY Final_VarID"))[,c("COLUMN_NAME","Final_VarID")]
			rownames <- varnamesx[charmatch(rownames(flm),varnamesx[["Final_VarID"]]),"COLUMN_NAME"]
			flm@Dimnames[[1]] <- rownames 
			return(flm)
		}
	}

	if(is.FLVector(y))
	{
		if(length(y) != nrow(x)) stop("incompatible dimensions")
		if(nrow(y)==1 && !y@isDeep)
		{
			y <- as.FLMatrix(y,
                                         sparse=TRUE,rows=length(y),cols=1)
			return(FLCorGeneric(x=x,y=y,
						functionName=functionName,
						method=method,...))
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
			sqlstr <- genCorrelUDTSql(object1=x,
								object2=y,
								functionName=functionName,
								method=method)
			vstoreFlag <- ifelse(is.null(sqlstr),FALSE,TRUE)
			if(is.null(sqlstr))
			sqlstr <- paste0("SELECT '%insertIDhere%' AS MATRIX_ID,",
									a,".var_id_colname AS rowIdColumn,
									 1 AS colIdColumn,",
                                         functionName,"(",a,".cell_val_colname,",
								 					  b,".vectorValueColumn) AS valueColumn 
							FROM ( ",constructSelect(x),") AS ",a,
			                  ",( ",constructSelect(y),") AS ",b,
	            			constructWhere(c(paste0(a,".obs_id_colname = ",b,".vectorIndexColumn"))),
	            			" GROUP BY ",a,".var_id_colname")

			tblfunqueryobj <- new("FLTableFunctionQuery",
                    connectionName = attr(connection,"name"),
                    variables=list(
                        rowIdColumn="rowIdColumn",
                        colIdColumn="colIdColumn",
                        valueColumn="valueColumn"),
                    whereconditions="",
                    order = "",
                    SQLquery=sqlstr)

			
			flm <- newFLMatrix(
                       select= tblfunqueryobj,
                       dims=as.integer(c(ncol(x),1)),
                       Dimnames = list(
                           colnames(x),
                           "1"))

			if(!is.null(varnamesx))
			flm@Dimnames[[1]] <- varnamesx[charmatch(rownames(flm),varnamesx[["Final_VarID"]]),"COLUMN_NAME"]

			return(ensureQuerySize(pResult=flm,
							pInput=list(x,y,functionName,...),
							pOperator="FLCorGeneric",
							pStoreResult=vstoreFlag))
		}
	}
	if(is.matrix(y))
	{
		if(nrow(x) == nrow(y))
		{
			y <- as.FLMatrix(y)
			return(FLCorGeneric(x=x,y=y,
						functionName=functionName,
						method=method,...))
		}
		else stop(" incompatible dimensions ")
	}
	if(is.numeric(y))
	{
		if(nrow(x) == length(y))
		{
			y <- as.FLMatrix(as.matrix(y))
			return(FLCorGeneric(x=x,y=y,
						functionName=functionName,
						method=method,...))
		}
		else stop(" incompatible dimensions ")
	}
	if(is.data.frame(y))
	{
		if(nrow(x) == nrow(y))
		{
			y <- as.FLMatrix(as.matrix(y))
			return(FLCorGeneric(x=x,y=y,
						functionName=functionName,
						method=method,...))
		}
		else stop(" incompatible dimensions ")
	}
}

#' Weighted Covariance.
#'
#' \code{cov.wt} computes correlation of in-database Objects
#'
#' @param x FLMatrix, FLVector or FLTable object or any R object
#' @param y FLMatrix, FLVector or FLTable object or any R object
#' @param ... any additional arguments
#' @section Constraints:
#' The \code{method} input can be 1,2,3 as in DB-Lytix manual.
#' "ML" method is equivalent to method=1."unbiased" is not supported.
#' The number of non-null pairs must be greater than or equal to 2.
#' If number of non-null pairs is less than 2, FLCorrel returns a NULL.
#' @return \code{cor} returns FLMatrix object representing correlation of x and y.
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' deeptable <- FLTable("tblUSArrests", "ObsID","VarID","Num_Val")
#' widetable <- FLTable("tblAbaloneWide","ObsID")
#' cor(deeptable,deeptable)
#' cor(widetable,widetable)
#' @export
cov.wtGeneric <- function(x,
			    		wt = rep(1/nrow(x), nrow(x)),
			    		cor = FALSE, 
			    		center = TRUE,
			       		method = 1,
			       		rowIdColumn="rowIdColumn",
			       		colIdColumn="colIdColumn",
			       		valueColumn="valueColumn"){
    if(length(wt)!=nrow(x))
    stop("length of 'wt' must equal the number of rows in 'x' \n")
    if(!method %in% c(1,2,3,"ML"))
    stop("method should be 1,2,3 or ML \n")
    if(method=="ML") method <- 1

    if(is.vector(wt))
    wt <- as.FLVector(wt)
    else if(!is.FLVector(wt))
    stop(" wt should be vector or FLVector \n ")

    sqlstr <- paste0("SELECT '%insertIDhere%' AS MATRIX_ID,\n",
    					"a.",colIdColumn," AS rowIdColumn,\n",
    					"b.",colIdColumn," AS colIdColumn,\n",
    					"FLWtCovar(a.",valueColumn,",b.",valueColumn,",c.vectorValueColumn,",method,") AS valueColumn\n",
    				" FROM(",constructSelect(x),") a,\n (",
    						constructSelect(x)," ) b, \n (",
    						constructSelect(wt)," ) c \n ",
    				constructWhere(c(paste0("a.",rowIdColumn," = b.",rowIdColumn))),
            			" GROUP BY a.",colIdColumn,",b.",colIdColumn)

    tblfunqueryobj <- new("FLTableFunctionQuery",
                connectionName = attr(connection,"name"),
                variables=list(
                    rowIdColumn="rowIdColumn",
                    colIdColumn="colIdColumn",
                    valueColumn="valueColumn"),
                whereconditions="",
                order = "",
                SQLquery=sqlstr)

	flm <- newFLMatrix(
                       select= tblfunqueryobj,
                       dims=as.integer(c(ncol(x),ncol(x))),
	            	   Dimnames = list(
                            colnames(x),
                            colnames(x)))
	flm <- ensureQuerySize(pResult=flm,
						pInput=list(x,wt,cor,center,method),
						pOperator="cov.wt")
	cov <- flm
	if(center){
		sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n",
								"a.",colIdColumn," AS vectorIndexColumn, \n",
								"FLWtAvg(b.vectorValueColumn,a.",valueColumn,") AS vectorValueColumn \n",
						" FROM (",constructSelect(x),") a,\n",
								"(",constructSelect(wt),") b ",
						" WHERE a.",rowIdColumn," = b.vectorIndexColumn ",
						" GROUP BY 1,2")

		tblfunqueryobj <- new("FLTableFunctionQuery",
	                    connectionName = attr(connection,"name"),
	                    variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
	                    whereconditions="",
	                    order = "",
	                    SQLquery=sqlstr)

		center <- newFLVector(
					select = tblfunqueryobj,
					Dimnames = list(colnames(x),"vectorValueColumn"),
					isDeep = FALSE)
	}
	n.obs <- nrow(x)
	if(cor)
	cor <- cor(x)
	if(!is.logical(center))
	resultList <- list(cov=cov,
						center=center,
						n.obs=n.obs,
						wt=wt)
	else resultList <- list(cov=cov,
						center=center,
						n.obs=n.obs,
						wt=wt)
	if(!is.logical(cor))
	resultList <- c(resultList,cor=cor)
	return(resultList)
        }

setMethod("cov.wt",signature(x="FLMatrix"),
    function(x,
    		wt = rep(1/nrow(x), nrow(x)),
    		cor = FALSE, 
    		center = TRUE,
       		method = 1){
    	return(cov.wtGeneric(x=x,
    				wt=wt,
    				cor=cor,
    				center=center,
    				method=method))})

setMethod("cov.wt",signature(x="FLTable"),
    function(x,
    		wt = rep(1/nrow(x), nrow(x)),
    		cor = FALSE, 
    		center = TRUE,
       		method = 1){
    	return(cov.wtGeneric(x=x,
    				wt=wt,
    				cor=cor,
    				center=center,
    				method=method,
    				rowIdColumn="obs_id_colname",
    				colIdColumn="var_id_colname",
    				valueColumn="cell_val_colname"))})

#' @export
setGeneric("genCorrelUDTSql", function(object1,
									object2,
									functionName,
									method="pearson") {
    standardGeneric("genCorrelUDTSql")
})

setMethod("genCorrelUDTSql", signature(object1 = "ANY",
                               		object2="ANY"),
		function(object1,
				object2,
				functionName,
				method="pearson") {
	rows=ncol(object1)
	cols=ncol(object2)

	if(functionName %in% c("FLCovarP","FLCorrel","FLCovar")){
		if(method %in% c("spearman"))
			functionName <- paste0(functionName,"Rank")
		else if(method %in% "shuffle")
				functionName <- paste0(functionName,"Shuffle")
		else return(NULL)
	}
	else return(NULL)

	voutCol <- c(oRankCorrel="FLCorrelRank",
						oRankCovarP="FLCovarPRank",
						oRankCovar="FLCovarRank",
						oShuffleCorrel="FLCorrelShuffle",
						oShuffleCovarP="FLCovarPShuffle",
						oShuffleCovar="FLCovarShuffle")
	voutCol <- names(voutCol)[functionName==voutCol]

	vfuncName <- c(FLRankCorrelUDT="FLCorrelRank",
					FLRankCovarUDT="FLCovarRank",
					FLRankCovarPUDT="FLCovarPRank",
					FLShuffleCorrelUDT="FLCorrelShuffle",
					FLShuffleCovarUDT="FLCovarShuffle",
					FLShuffleCovarPUDT="FLCovarPShuffle")

	functionName <- names(vfuncName)[vfuncName==functionName]

    sqlstr <- paste0("WITH z (pGroupID, pXValue, pYValue) AS ( \n ",
						" SELECT  DENSE_RANK()OVER(ORDER BY ",
									getVarIDColAliasName(object=object1,
														alias="a"),",",
									getVarIDColAliasName(object=object2,
														alias="b"),"), \n ",
									getCellValColAliasName(object=object1,
														alias="a"),", \n ",
									getCellValColAliasName(object=object2,
														alias="b")," \n ",
						" FROM(",constructSelect(object1),") a,\n ",
							" (",constructSelect(object2),") b \n ",
						" WHERE ",getObsIDColAliasName(object=object1,
														alias="a"),
							" = ",getObsIDColAliasName(object=object2,
														alias="b"),") \n ",
					" SELECT '%insertIDhere%' AS MATRIX_ID,\n ",
							" a.oGroupId - (CAST((a.oGroupID-0.355)/",
								rows," AS INT)*",rows,") AS rowIdColumn,\n ",
							" CAST((a.oGroupid -0.355)/",rows," AS INT)+1 AS colIdColumn,\n ",
							" a.",voutCol," AS valueColumn \n ",
					" FROM TABLE (",functionName,"(z.pGroupID, z.pXValue, z.pYValue) \n ",
									" HASH BY z.pGroupID \n ",
									" LOCAL ORDER BY z.pGroupID) AS a ")
    return(sqlstr)
})

#' @export
setGeneric("getObsIDColAliasName", function(object,alias="") {
    standardGeneric("getObsIDColAliasName")
})
setMethod("getObsIDColAliasName", signature(object = "FLMatrix"),
		function(object,alias) 
		return(paste0(ifelse(alias=="",paste0(""),paste0(alias,".")),"rowIdColumn")))
setMethod("getObsIDColAliasName", signature(object = "FLVector"),
function(object,alias)
return(paste0(ifelse(alias=="",paste0(""),paste0(alias,".")),"vectorIndexColumn")))
setMethod("getObsIDColAliasName", signature(object = "FLTable"),
function(object,alias) 
return(paste0(ifelse(alias=="",paste0(""),paste0(alias,".")),"obs_id_colname")))

#' @export
setGeneric("getVarIDColAliasName", function(object,alias="") {
    standardGeneric("getVarIDColAliasName")
})
setMethod("getVarIDColAliasName", signature(object = "FLMatrix"),
		function(object,alias) 
		return(paste0(ifelse(alias=="",paste0(""),paste0(alias,".")),"colIdColumn")))
setMethod("getVarIDColAliasName", signature(object = "FLVector"),
function(object,alias) 
return("1"))
setMethod("getVarIDColAliasName", signature(object = "FLTable"),
function(object,alias) 
return(paste0(ifelse(alias=="",paste0(""),paste0(alias,".")),"var_id_colname")))

#' @export
setGeneric("getCellValColAliasName", function(object,alias="") {
    standardGeneric("getCellValColAliasName")
})
setMethod("getCellValColAliasName", signature(object = "FLMatrix"),
		function(object,alias) 
		return(paste0(ifelse(alias=="",paste0(""),
			paste0(alias,".")),"valueColumn")))
setMethod("getCellValColAliasName", signature(object = "FLVector"),
function(object,alias) 
return(paste0(ifelse(alias=="",paste0(""),paste0(alias,".")),"vectorValueColumn")))
setMethod("getCellValColAliasName", signature(object = "FLTable"),
function(object,alias) 
return(paste0(ifelse(alias=="",paste0(""),paste0(alias,".")),"cell_val_colname")))
