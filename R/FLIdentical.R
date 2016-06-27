#' @include utilities.R
#' @include FLIs.R
#' @include FLCastFunctions.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLTable.R
#' @include FLDims.R
#' @include FLPrint.R
NULL

#' Equality of in-database objects.
#'
#' \code{identical} checks the equality of in-database objects.
#'
#' The equality of in-database objects mimics the normal addition of R data types.
#' One can check equality of FLMatrices, FLMatrix - R matrices, FLVectors and
#' FLVector - RVector.
#' @param pObj1 can be an in-database object like FLMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param pObj2 can be an in-database object like FLMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{identical} returns a logical TRUE or FALSE.
#' @section Constraints:
#' Currently only \code{dgCMatrix},\code{dgeMatrix},\code{dsCMatrix},
#' \code{dgTMatrix},\code{matrix},\code{Matrix},\code{vector} R types
#' are supported.
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' flmatrix <- FLMatrix("FL_DEMO", 
#' "tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' Rvector <- 1:5
#' Result <- identical(flmatrix,flmatrix)
#' Result <- identical(Rvector,as.FLVector(Rvector,connection))
#' @export

identical <- function(pObj1,pObj2)
{
	UseMethod("identical", pObj1)
}

#' @export
identical.default <- base::identical

#' @export
identical.FLMatrix <- function(pObj1, pObj2)
{
	connection <- getConnection(pObj1)
	if(is.FLMatrix(pObj2))
	{
		checkSameDims(pObj1,pObj2)
		a <- genRandVarName()
		b <- genRandVarName()

		sqlstr <- paste0("SELECT 0
						 FROM (",constructSelect(pObj1),") AS ",a,
						 	",(",constructSelect(pObj2),") AS ",b,
                        constructWhere(c(paste0(a,".rowIdColumn = ",b,".rowIdColumn"),
                                         paste0(a,".colIdColumn = ",b,".colIdColumn"),
                                         paste0(a,".valueColumn <> ",b,".valueColumn"))))

		sqlstr <- ensureQuerySize(pResult=sqlstr,
	            pInput=list(pObj1,pObj2),
	            pOperator="identical")

		retobj <- sqlQuery(connection,sqlstr)

		if(nrow(retobj) == 0)
		return(TRUE)
		else if (nrow(retobj) > 0)
		return(FALSE)
	}
	else if(is.matrix(pObj2)||class(pObj2)=="dgCMatrix"
		    ||class(pObj2)=="dgeMatrix"||class(pObj2)=="dsCMatrix"
		    ||class(pObj2)=="dgTMatrix")
	{
		checkSameDims(pObj1,pObj2)
		pObj2 <- as.FLMatrix(pObj2)
		return(identical(pObj1,pObj2))
	}
	else
	return(FALSE)
}

#' @export
identical.FLVector <- function(pObj1, pObj2)
{
	connection <- getConnection(pObj1)
	if(is.FLVector(pObj2))
	{
		if(length(pObj1) != length(pObj2)) stop("non-conformable dimensions")
		a <- genRandVarName()
		b <- genRandVarName()
		newColnames1 <- renameDuplicates(colnames(pObj1))
		newColnames2 <- renameDuplicates(colnames(pObj2))
		sqlstr <- paste0("SELECT 0
						 FROM (",constructSelect(pObj1),") AS ",a,
						 	",(",constructSelect(pObj2),") AS ",b,
                        constructWhere(c(paste0(a,".vectorIndexColumn = ",b,".vectorIndexColumn"),
                                         paste0(a,".",newColnames1," <> ",b,".",newColnames2))))

		sqlstr <- ensureQuerySize(pResult=sqlstr,
	            pInput=list(pObj1,pObj2),
	            pOperator="identical")

		retobj <- sqlQuery(connection,sqlstr)

		if(nrow(retobj) == 0)
		return(TRUE)
		else if (nrow(retobj) > 0)
		return(FALSE)
	}
	else if(is.vector(pObj2))
	{
		if(length(pObj1) != length(pObj2)) stop("non-conformable dimensions")
		pObj2 <- as.FLVector(pObj2)
		return(identical(pObj1,pObj2))
	}
	else
	return(FALSE)
}

#' @export
identical.matrix <- function(pObj1,pObj2)
{
	if(is.FLMatrix(pObj2))
	{
		checkSameDims(pObj1,pObj2)
		pObj1 <- as.FLMatrix(pObj1)
		return(identical(pObj1,pObj2))
	}
	else
	return(base::identical(pObj1,pObj2))
}

#' @export
identical.dgCMatrix <- identical.matrix
#' @export
identical.dgeMatrix <- identical.matrix
#' @export
identical.dgTMatrix <- identical.matrix
#' @export
identical.dsCMatrix <- identical.matrix

#' @export
identical.numeric <- function(pObj1,pObj2)
{
	if(is.FLVector(pObj2))
	{
		if(length(pObj1) != length(pObj2)) stop("non-conformable dimensions")
		pObj1 <- as.FLVector(pObj1)
		return(identical(pObj1,pObj2))
	}
	else
	return(base::identical(pObj1,pObj2))
}

NULL
#' Equality of in-database objects.
#'
#' \code{==} checks the equality of in-database objects.
#'
#' The equality of in-database objects mimics the normal addition of R data types.
#' One can check equality of FLMatrices, FLMatrix - R matrices, FLVectors and
#' FLVector - RVector.
#' @param pObj1 can be an in-database object like FLMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param pObj2 can be an in-database object like FLMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{==} returns a logical TRUE or FALSE matrix similar to R output
#' @section Constraints:
#' Currently only \code{dgCMatrix},\code{dgeMatrix},\code{dsCMatrix},
#' \code{dgTMatrix},\code{matrix},\code{Matrix},\code{vector} R types
#' are supported. Comparision of FLMatrix with FLVector is not currently Supported.
#' In case of FLVector and Rvector comparision use FLVector==RVector in place of 
#' RVector==FLVector
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' flmatrix <- FLMatrix("FL_DEMO", 
#' "tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' flvector <- as.FLVector(1:5)
#' Result <- flmatrix == flmatrix
#' Result <- flvector==flvector
#' Result <- flvector==1:5
#' @export

"==" <- function(pObj1,pObj2)
{
	UseMethod("==", pObj1)
}

#' @export
`==.default` <- function(pObj1,pObj2)
{
	op <- .Primitive("==")
	op(pObj1,pObj2)
}

#' @export
`==.FLMatrix` <- function(pObj1, pObj2)
{
	connection <- getConnection(pObj1)
	if(is.FLMatrix(pObj2))
	{
		checkSameDims(pObj1,pObj2)
		a <- genRandVarName()
		b <- genRandVarName()

		sqlstr <- paste0("SELECT '%insertIDhere%' AS MATRIX_ID,",
								   a,".rowIdColumn AS rowIdColumn,",
								   a,".colIdColumn AS colIdColumn,
								   CASE 
								    WHEN ",a,".valueColumn <> ",b,".valueColumn THEN 'FALSE' 
								    WHEN ",a,".valueColumn = ",b,".valueColumn THEN 'TRUE' 
								   END AS valueColumn 
						 FROM (",constructSelect(pObj1),") AS ",a,
						 	",(",constructSelect(pObj2),") AS ",b,
                        constructWhere(c(paste0(a,".rowIdColumn = ",b,".rowIdColumn"),
                                         paste0(a,".colIdColumn = ",b,".colIdColumn"))))

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
	            dimnames=dimnames(pObj1))

	    flm <- ensureQuerySize(pResult=flm,
		            pInput=list(pObj1,pObj2),
		            pOperator="==",
		            pStoreResult=TRUE)
	    return(flm)
	    return(matrix(as.logical(as.matrix(flm)),nrow(pObj1),ncol(pObj1)))
	}
	if(is.matrix(pObj2)||class(pObj2)=="dgCMatrix"
		    ||class(pObj2)=="dgeMatrix"||class(pObj2)=="dsCMatrix"
		    ||class(pObj2)=="dgTMatrix")
	{
		checkSameDims(pObj1,pObj2)
		pObj2 <- as.FLMatrix(pObj2)
		return("=="(pObj1,pObj2))
	}
	# if(is.FLVector(pObj2))
	# {
	# 	# pObj2 <- as.FLMatrix(pObj2, sparse=TRUE,rows=nrow(pObj1),cols=ncol(pObj1))
	# 	# return(pObj1==pObj2)
	# }
	if(is.vector(pObj2))
	{
		pObj2 <- as.FLMatrix(matrix(pObj2,nrow(pObj1),ncol(pObj1)))
		return(pObj1==pObj2)
	}
	
	return(stop("incomparable inputs"))
}

#' @export
`==.FLVector` <- function(pObj1, pObj2)
{
	if(is.FLVector(pObj2))
	{
		connection <- getConnection(pObj2)
		if(checkMaxQuerySize(pObj1))
		pObj1 <- store(pObj1)
		if(checkMaxQuerySize(pObj2))
		pObj2 <- store(pObj2)
		#if(length(pObj1) != length(pObj2)) stop("non-conformable dimensions")
		a <- genRandVarName()
		b <- genRandVarName()

		if(!pObj1@isDeep && !pObj2@isDeep)
		{
			newColnames1 <- renameDuplicates(colnames(pObj1))
			newColnames2 <- renameDuplicates(colnames(pObj2))
			if(length(newColnames1)==1 && length(newColnames2)==1)
			{
			sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,",
									a,".vectorIndexColumn AS vectorIndexColumn,
									CASE 
									 WHEN ",a,".",newColnames1," <> ",b,".",newColnames2, " THEN 0 
									 WHEN ",a,".",newColnames1," = ",b,".",newColnames2, " THEN 1 
									END AS vectorValueColumn 
							 FROM (",constructSelect(pObj1),") AS ",a,
							 	",(",constructSelect(pObj2),") AS ",b,
	                        constructWhere(c(paste0(a,".vectorIndexColumn = ",b,".vectorIndexColumn"))),
	                        collapse=" UNION ALL ")
			dimnames <- list(rownames(pObj1),"vectorValueColumn")
			}
			else if(length(newColnames1)>1 && length(newColnames2)>1)
			{
			sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,",
									1:max(length(newColnames1),length(newColnames2))," AS vectorIndexColumn,
									CASE 
									 WHEN ",a,".",newColnames1," <> ",b,".",newColnames2, " THEN 0 
									 WHEN ",a,".",newColnames1," = ",b,".",newColnames2, " THEN 1 
									END AS vectorValueColumn 
							 FROM (",constructSelect(pObj1),") AS ",a,
							 	",(",constructSelect(pObj2),") AS ",b,
	                        collapse=" UNION ALL ")
			dimnames <- list(1:max(length(newColnames1),length(newColnames2)),"vectorValueColumn")
			}
			else if(length(newColnames1)>1) return(store(pObj1)==pObj2)
			else return(store(pObj2)==pObj1)
		}
		else
		{
			if(pObj1@isDeep && pObj2@isDeep)
			{
				sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,",
										a,".vectorIndexColumn AS vectorIndexColumn,
										CASE 
										 WHEN ",a,".vectorValueColumn <> ",b,".vectorValueColumn THEN 0 
										 WHEN ",a,".vectorValueColumn = ",b,".vectorValueColumn THEN 1 
										END AS vectorValueColumn 
								 FROM (",constructSelect(pObj1),") AS ",a,
								 	",(",constructSelect(pObj2),") AS ",b,
		                        constructWhere(c(paste0(a,".vectorIndexColumn = ",b,".vectorIndexColumn"))),
		                        collapse=" UNION ALL ")
				dimnames <- list(rownames(pObj1),c("vectorIdColumn","vectorIndexColumn","vectorValueColumn"))
			}
			else return(as.vector(pObj1)==as.vector(pObj2))
		}

		tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = connection,
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

		flv <- ensureQuerySize(pResult=flv,
	            pInput=list(pObj1,pObj2),
	            pOperator="==")
		return(as.logical(as.vector(flv)))
	}
	if(is.vector(pObj2))
	{
		#if(length(pObj1) != length(pObj2)) stop("non-conformable dimensions")
		pObj2 <- as.FLVector(pObj2)
		return("=="(pObj1,pObj2))
	}
	if(is.matrix(pObj2))
	{
		pObj2 <- as.FLMatrix(pObj2)
		return(pObj2==pObj1)
	}
	# if(is.FLMatrix(pObj2))
	# return(pObj2==pObj1)

	return(stop("incomparable inputs"))
}

#' @export
`==.matrix` <- function(pObj1,pObj2)
{
	if(is.FLMatrix(pObj2))
	{
		checkSameDims(pObj1,pObj2)
		pObj1 <- as.FLMatrix(pObj1,connection=getConnection(pObj2))
		return("=="(pObj1,pObj2))	
	}
	else
	return(base::"=="(pObj1,pObj2))
}

#' @export
`==.dgCMatrix` <- `==.matrix`
#' @export
`==.dgeMatrix` <- `==.matrix`
#' @export
`==.dgTMatrix` <- `==.matrix`
#' @export
`==.dsCMatrix` <- `==.matrix`

#' @export
`==.numeric` <- function(pObj1,pObj2)
{
	if(is.FLVector(pObj2))
	{
		#if(length(pObj1) != length(pObj2)) stop("non-conformable dimensions")
		pObj1 <- as.FLVector(pObj1,connection=getConnection(pObj2))
		return("=="(pObj1,pObj2))	
	}
	else
	return(base::"=="(pObj1,pObj2))
}
