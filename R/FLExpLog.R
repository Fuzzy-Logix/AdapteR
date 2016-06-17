#' @include utilities.R
#' @include FLIs.R
#' @include FLCastFunctions.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLTable.R
#' @include FLDims.R
#' @include FLPrint.R

NULL

setGeneric("FLExpLog", function(functionName,x,m1=0,p1=0,lnb=1,...)
    standardGeneric("FLExpLog"))

setMethod("FLExpLog",signature(x="FLMatrix"),
	function(functionName, x,m1=0,p1=0,lnb=1,...){
		a <- genRandVarName()

		## Check validity of x for log
		if(functionName=="log" || functionName=="sqrt")
		{
			if(p1==1) vcondition <- " <= -1 "
			if(functionName=="sqrt")
			vcondition <- " < 0 "
			else vcondition <- " <= 0 "
			sqlstr <- paste0(" SELECT COUNT(a.valueColumn) AS cnt",
						" FROM(",constructSelect(x),") AS a",
						" WHERE a.valueColumn",vcondition)
			vcount <- sqlQuery(getOption("connectionFL"),sqlstr)
			if(length(vcount)>1 || is.null(vcount))
			stop(vcount)
			else if(vcount[["cnt"]]>0) stop("invalid argument")
		}

		sqlstr <- paste0(" SELECT '%insertIDhere%' AS MATRIX_ID,",
							a,".rowIdColumn AS rowIdColumn,",
							a,".colIdColumn AS colIdColumn,",
							functionName,"(",a,".valueColumn+(1*(",p1,")))/",lnb,"-(",m1,") AS valueColumn",
						" FROM(",constructSelect(x),") AS ",a)

		tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = getOption("connectionFL"),
                        variables=list(
                            rowIdColumn="rowIdColumn",
                            colIdColumn="colIdColumn",
                            valueColumn="valueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)
		flm <- new("FLMatrix",
                           select= tblfunqueryobj,
                           dim=x@dim,
                           dimnames=dimnames(x))

		return(ensureQuerySize(pResult=flm,
						pInput=list(x),
						pOperator="FLExpLog"))
		})

setMethod("FLExpLog",signature(x="FLVector"),
	function(functionName, x,m1=0,p1=0,lnb=1,...){
		a <- genRandVarName()
		if(ncol(x)>1 && !x@isDeep)
		{
			newColnames <- renameDuplicates(colnames(x))
			maxLength <- length(colnames(x))
			sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
										1:maxLength," AS vectorIndexColumn,",
							functionName,"(",a,".",newColnames,"+(1*(",p1,")))/",lnb,"-(1*(",m1,")) AS vectorValueColumn",
							" FROM (",constructSelect(x),") AS ",a,
							    collapse=" UNION ALL ")
			dimnames <- list(1:maxLength,
							"vectorValueColumn")
		}
		else if(ncol(x)==1 || x@isDeep)
		{
			a <- genRandVarName()
			sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
								a,".vectorIndexColumn AS vectorIndexColumn,",
							functionName,"(",a,".vectorValueColumn+(1*(",p1,")))/",lnb,"-(1*(",m1,")) AS vectorValueColumn",
							" FROM(",constructSelect(x),") AS ",a)
			if(ncol(x)>1 && x@isDeep)
			dimnames <- list(dimnames(x)[[2]],
							"vectorValueColumn")
			else dimnames <- list(dimnames(x)[[1]],
								"vectorValueColumn")
		}
		tblfunqueryobj <- new("FLTableFunctionQuery",
	                    connection = getOption("connectionFL"),
	                    variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
	                    whereconditions="",
	                    order = "",
	                    SQLquery=sqlstr)

		flv <- new("FLVector",
					select = tblfunqueryobj,
					dimnames = dimnames,
					isDeep = FALSE)

		return(ensureQuerySize(pResult=flv,
							pInput=list(x),
							pOperator="FLExpLog"))
		})

#' Logarithms and Exponentials of in-database objects.
#'
#' Element-wise Logarithms and Exponentials of in-database objects.
#'
#' The \code{exp} computes the exponential function
#' The \code{log} computes the logrithm to base as specified(default e)
#' The \code{logb} computes the logrithm to base as specified(default e)
#' The \code{log1p} computes the log10(1+x)
#' The \code{log10} computes the logarithm to base 10
#' The \code{log2} computes the logarithm to base 2
#' The \code{expm1} computes exp(x)-1
#' All types of operands are possible just like in R 
#' and the result is an in-database object.
#' @param x can be an in-database object like FLMatrix,FLVector or
#' a normal R object
#' @param base a positive number with respect to which logs are computed
#' @return returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' flmatrix <- FLMatrix("FL_DEMO", 
#' "tblMatrixMulti", 1,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' ResultFLmatrix <- exp(flmatrix)
#' ResultFLmatrix <- logb(flmatrix,3)
#' ResultFLmatrix <- log10(flmatrix)
#' ResultFLmatrix <- log1p(flmatrix)
#' ResultFLmatrix <- log(flmatrix)
#' ResultFLmatrix <- log2(flmatrix)
#' deeptable <- FLTable("FL_DEMO","tblUSArrests","ObsID","VarID","Num_Val")
#' flvector <- deeptable[1:5,1]
#' resultFLVector <- exp(flvector)
#' resultFLVector <- log(flvector,4)

# setGeneric("exp",function(x,...)
# 	standardGeneric("exp"),
# 	useAsDefault = function(x,...) base::exp(x))

setMethod("exp",signature(x="FLMatrix"),
	function(x) FLExpLog(functionName="exp",
							x=x))
setMethod("exp",signature(x="FLVector"),
	function(x) FLExpLog(functionName="exp",
							x=x))

setMethod("expm1",signature(x="FLMatrix"),
	function(x) FLExpLog(functionName="exp",
							x=x,
							m1=1))
setMethod("expm1",signature(x="FLVector"),
	function(x) FLExpLog(functionName="exp",
							x=x,
							m1=1))

setMethod("log10",signature(x="FLMatrix"),
	function(x) FLExpLog(functionName="log",
							x=x))
setMethod("log10",signature(x="FLVector"),
	function(x) FLExpLog(functionName="log",
							x=x))

setMethod("log1p",signature(x="FLMatrix"),
	function(x) FLExpLog(functionName="log",
							x=x,
							p1=1, lnb=base::logb(exp(1),10)))
setMethod("log1p",signature(x="FLVector"),
	function(x) FLExpLog(functionName="log",
							x=x,
							p1=1, lnb=base::logb(exp(1),10)))

setMethod("log",signature(x="FLMatrix"),
	function(x,base=base::exp(1)) FLExpLog(functionName="log",
							x=x,
							lnb=base::logb(base[1],10)))
setMethod("log",signature(x="FLVector"),
	function(x,base=base::exp(1)) FLExpLog(functionName="log",
							x=x,
							lnb=base::logb(base[1],10)))

setMethod("logb",signature(x="FLMatrix"),
	function(x,base=base::exp(1)) FLExpLog(functionName="log",
							x=x,
							lnb=base::logb(base[1],10)))
setMethod("logb",signature(x="FLVector"),
	function(x,base=base::exp(1)) FLExpLog(functionName="log",
							x=x,
							lnb=base::logb(base[1],10)))

setMethod("log2",signature(x="FLMatrix"),
	function(x) FLExpLog(functionName="log",
							x=x,
							lnb=base::logb(2,10)))
setMethod("log2",signature(x="FLVector"),
	function(x) FLExpLog(functionName="log",
							x=x,
							lnb=base::logb(2,10)))
setMethod("sqrt", signature(x="FLVector"), 
	function(x)FLExpLog(functionName="sqrt",
		                           x=x))
setMethod("sqrt", signature(x="FLMatrix"), 
	function(x)FLExpLog(functionName="sqrt",
		                           x=x))
#' @export
order <- function(...,na.last=TRUE,decreasing=FALSE)
{
	#browser()
	vlist <- list(...)
	vtemp <- unlist(lapply(vlist,function(x)is.FLVector(x)||is.FLMatrix(x)))
	if(!any(vtemp))
	return(base::order(c(...),na.last=na.last,decreasing=decreasing))
	##order only the first flvector.R also behaves similarly.
	vflvector <- vlist[vtemp][[1]]
	a <- genRandVarName()
	if(decreasing) vdesc <- paste0("DESC")
	else vdesc <- ""
	vsqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,\n",
						"ROW_NUMBER()OVER(ORDER BY ",a,".vectorValueColumn ",vdesc,",",
							a,".vectorIndexColumn ) AS vectorIndexColumn,\n",
						a,".vectorIndexColumn AS vectorValueColumn \n",
					  " FROM (",constructSelect(vflvector),") AS ",a)
	if(is.FLMatrix(vflvector))
	vsqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,\n",
						"ROW_NUMBER()OVER(ORDER BY ",a,".valueColumn ",vdesc,",",
							a,".colIdColumn,",a,".rowIdColumn) AS vectorIndexColumn,\n",
						"ROW_NUMBER()OVER(ORDER BY ",a,".colIdColumn,",a,".rowIdColumn) AS vectorValueColumn \n",
					  " FROM (",constructSelect(vflvector)," ) AS ",a)

	tblfunqueryobj <- new("FLTableFunctionQuery",
	                    connection = getOption("connectionFL"),
	                    variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
	                    whereconditions="",
	                    order = "",
	                    SQLquery=vsqlstr)

	flv <- new("FLVector",
				select = tblfunqueryobj,
				dimnames = list(1:length(vflvector),
								"vectorValueColumn"),
				isDeep = FALSE)

	return(ensureQuerySize(pResult=flv,
						pInput=list(...,na.last=na.last,decreasing=decreasing),
						pOperator="order"))
}

#' @export
sort.FLVector <- function(x,decreasing=FALSE,...)
{
	decreasing <- as.logical(decreasing)
	if(is.na(decreasing) || length(decreasing) < 1 
		|| is.null(decreasing))
	stop("decreasing should be logical in sort")

	a <- genRandVarName()
	if(decreasing) vdesc <- paste0("DESC")
	else vdesc <- ""
	vsqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,\n",
						"ROW_NUMBER()OVER(ORDER BY ",a,".vectorValueColumn ",vdesc,",",
							a,".vectorIndexColumn ) AS vectorIndexColumn,\n",
						a,".vectorValueColumn AS vectorValueColumn \n",
					  " FROM (",constructSelect(x),") AS ",a)

	tblfunqueryobj <- new("FLTableFunctionQuery",
	                    connection = getOption("connectionFL"),
	                    variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
	                    whereconditions="",
	                    order = "",
	                    SQLquery=vsqlstr)

	flv <- new("FLVector",
				select = tblfunqueryobj,
				dimnames = list(1:length(x),
								"vectorValueColumn"),
				isDeep = FALSE)

	return(ensureQuerySize(pResult=flv,
						pInput=list(x,decreasing=decreasing,...),
						pOperator="sort"))
}

#' @export
sort.FLMatrix <- function(x,decreasing=FALSE,...)
{
	decreasing <- as.logical(decreasing)
	if(is.na(decreasing) || length(decreasing) < 1 
		|| is.null(decreasing))
	stop("decreasing should be logical in sort")

	a <- genRandVarName()
	if(decreasing) vdesc <- paste0("DESC")
	else vdesc <- ""
	vsqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,\n",
						"ROW_NUMBER()OVER(ORDER BY ",a,".valueColumn ",vdesc,",",
							a,".colIdColumn,",a,".rowIdColumn) AS vectorIndexColumn,\n",
						a,".valueColumn AS vectorValueColumn \n",
					  " FROM (",constructSelect(x),") AS ",a)

	tblfunqueryobj <- new("FLTableFunctionQuery",
	                    connection = getOption("connectionFL"),
	                    variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
	                    whereconditions="",
	                    order = "",
	                    SQLquery=vsqlstr)

	flv <- new("FLVector",
				select = tblfunqueryobj,
				dimnames = list(1:length(x),
								"vectorValueColumn"),
				isDeep = FALSE)

	return(ensureQuerySize(pResult=flv,
						pInput=list(x,decreasing=decreasing,...),
						pOperator="sort"))

}
