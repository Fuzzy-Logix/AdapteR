#' @include FLMatrix.R
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
#' flmatrix <- FLMatrix("tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
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
	connection <- getFLConnection(pObj1)
	if(is.FLMatrix(pObj2))
	{
		if(!all(dim(pObj1)==dim(pObj2)))
		return(FALSE)

		sqlstr <- paste0(" SELECT a.rowIdColumn AS rowIdColumn, \n ",
								"a.colIdColumn AS colIdColumn, \n ",
								" CASE WHEN FLSum(a.valueColumn)<>0 THEN 'FALSE' ELSE 'TRUE' END AS EqualityColumn \n ",
								" FROM(",constructSelect(pObj1,joinNames=FALSE)," UNION ALL ",
									" SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
									" b.rowIdColumn AS rowIdColumn, \n ",
									" b.colIdColumn AS colIdColumn, \n ",
									" b.valueColumn*(-1) AS valueColumn \n ",
									" FROM(",constructSelect(pObj2),") AS b) AS a \n ",
							 " GROUP BY a.rowIdColumn,a.colIdColumn \n ",
							 " HAVING EqualityColumn = 'FALSE' ")

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
		if(!all(dim(pObj1)==dim(pObj2)))
		return(FALSE)
		pObj2 <- as.FLMatrix(pObj2)
		return(identical(pObj1,pObj2))
	}
	else
	return(FALSE)
}

#' @export
identical.FLVector <- function(pObj1, pObj2)
{
	connection <- getFLConnection(pObj1)
	if(is.FLVector(pObj2))
	{
		if(length(pObj1) != length(pObj2)) return(FALSE)
		a <- genRandVarName()
		b <- genRandVarName()
		newColnames1 <- renameDuplicates(colnames(pObj1))
		newColnames2 <- renameDuplicates(colnames(pObj2))
		sqlstr <- paste0("SELECT 'TRUE' 
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
		if(length(pObj1) != length(pObj2)) return(FALSE)
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
		if(!all(dim(pObj1)==dim(pObj2)))
		return(FALSE)
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
		if(length(pObj1) != length(pObj2)) return(FALSE)
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
#' flmatrix <- FLMatrix("FL_DEMO.tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
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
	connection <- getFLConnection(pObj1)
	if(is.FLMatrix(pObj2))
	{
		checkSameDims(pObj1,pObj2)

		sqlstr <- paste0(" SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
								" a.rowIdColumn AS rowIdColumn, \n ",
								"a.colIdColumn AS colIdColumn, \n ",
								" CASE WHEN FLSum(a.valueColumn)<>0 THEN 'FALSE' ELSE 'TRUE' END AS valueColumn \n ",
								" FROM(",constructSelect(pObj1,joinNames=FALSE)," \n UNION ALL \n ",
									" SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
									" b.rowIdColumn AS rowIdColumn, \n ",
									" b.colIdColumn AS colIdColumn, \n ",
									" b.valueColumn*(-1) AS valueColumn \n ",
									" FROM(",constructSelect(pObj2),") AS b) AS a \n ",
							 " GROUP BY 1,2,3 ")

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
	            dims=dim(pObj1),
	            Dimnames=dimnames(pObj1))

	    flm <- ensureQuerySize(pResult=flm,
		            pInput=list(pObj1,pObj2),
		            pOperator="==")
	    return(flm)
	}
	if(is.matrix(pObj2)||class(pObj2)=="dgCMatrix"
		    ||class(pObj2)=="dgeMatrix"||class(pObj2)=="dsCMatrix"
		    ||class(pObj2)=="dgTMatrix")
	{
		checkSameDims(pObj1,pObj2)
		pObj2 <- as.FLMatrix(pObj2)
		return("=="(pObj1,pObj2))
	}
	if(is.FLVector(pObj2))
	{
		pObj2 <- as.FLMatrix(pObj2, sparse=TRUE,rows=nrow(pObj1),cols=ncol(pObj1))
		return(pObj1==pObj2)
	}
	if(is.vector(pObj2))
	{
		pObj2 <- as.FLMatrix(matrix(pObj2,nrow(pObj1),ncol(pObj1)))
		return(pObj1==pObj2)
	}
	if(is.FLTable(pObj2))
	{
		if(!pObj2@isDeep)
		pObj2 <- wideToDeep(pObj2)[["table"]]
		pObj2 <- as.FLMatrix(pObj2)
		return(pObj1==pObj2)
	}
	return(stop("incomparable inputs"))
}

#' @export
`==.FLVector` <- function(pObj1, pObj2)
{
	if(is.FLVector(pObj2))
	{
		connection <- getFLConnection()
		if(checkQueryLimits(pObj1))
		pObj1 <- store(pObj1)
		if(checkQueryLimits(pObj2))
		pObj2 <- store(pObj2)

		ifelse(length(pObj1)>length(pObj2),{
			vmaxlen <- length(pObj1);
			vminlen <- length(pObj2);
			vmaxref <- "a";
			ifelse(pObj1@isDeep && length(colnames(pObj1))>1,
			vmaxrownames <- colnames(pObj1),
			vmaxrownames <- rownames(pObj1))
			},{
				vmaxlen <- length(pObj2);
				vmaxref <- "b";
				vminlen <- length(pObj1);
				ifelse(pObj2@isDeep && length(colnames(pObj2))>1,
				vmaxrownames <- colnames(pObj2),
				vmaxrownames <- rownames(pObj2))
				})

		if(ncol(pObj1)>1 && !pObj1@isDeep 
			&& ncol(pObj2)>1 && !pObj2@isDeep)
		{
			newColnames1 <- renameDuplicates(colnames(pObj1))
			newColnames2 <- renameDuplicates(colnames(pObj2))
			sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
									1:vmaxlen," AS vectorIndexColumn, \n ",
									" CASE \n ",
									" WHEN (a.",newColnames1," - b.",newColnames2, ") <> 0 THEN 'FALSE' ELSE 'TRUE' \n ",
									" END AS vectorValueColumn \n ",
							" FROM (",constructSelect(pObj1),") AS a, \n ",
							 	"(",constructSelect(pObj2),") AS b \n ",
	                        collapse=" UNION ALL ")
			dimnames <- list(1:vmaxlen,"vectorValueColumn")
		}
		if(ncol(pObj1)>1 && !pObj1@isDeep)
		pObj1 <- store(pObj1)
		if(ncol(pObj2)>1 && !pObj2@isDeep)
		pObj2 <- store(pObj2)
		if((pObj1@isDeep && pObj2@isDeep) 
			||(pObj1@isDeep && ncol(pObj2)==1)
			||(pObj2@isDeep && ncol(pObj1)==1)
			||(ncol(pObj1)==1 && ncol(pObj2)==1)){
			sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
									vmaxref,".vectorIndexColumn AS vectorIndexColumn \n ,",
									"CASE \n ",
									" WHEN (a.vectorValueColumn - b.vectorValueColumn) <> 0 \n ",
										 " THEN 'FALSE' ELSE 'TRUE' END AS vectorValueColumn \n ",
							" FROM (",constructSelect(pObj1),") AS a, \n ",
                            "(",constructSelect(pObj2),") AS b \n ",
                            ## gk: todo: revert to using FLMOD after FLTable redesign of rownames handling
                            ##constructWhere(c(paste0(" FLMOD(a.vectorIndexColumn,",vminlen,
	                        ## ") = FLMOD(b.vectorIndexColumn,",vminlen,")"))))

	                        constructWhere(c(paste0("(a.vectorIndexColumn MOD ",vminlen,
	                        					") = (b.vectorIndexColumn MOD ",vminlen,")"))))

			dimnames <- list(vmaxrownames,"vectorValueColumn")
		}

		tblfunqueryobj <- new("FLTableFunctionQuery",
                        connectionName = attr(connection,"name"),
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

		flv <- newFLVector(
				select = tblfunqueryobj,
				Dimnames = dimnames,
				isDeep = FALSE)

		flv <- ensureQuerySize(pResult=flv,
	            pInput=list(pObj1,pObj2),
	            pOperator="==")
		return(flv)
	}
	if(is.vector(pObj2))
	{
		pObj2 <- as.FLVector(pObj2)
		return("=="(pObj1,pObj2))
	}
	if(is.matrix(pObj2))
	{
		pObj2 <- as.FLMatrix(pObj2)
		return(pObj2==pObj1)
	}
	if(is.FLMatrix(pObj2))
	{
		pObj1 <- as.FLMatrix(pObj1, sparse=TRUE,
					rows=nrow(pObj2),cols=ncol(pObj2))
		return(pObj1==pObj2)
	}
	if(is.FLTable(pObj2))
	{
		if(!pObj2@isDeep)
		pObj2 <- wideToDeep(pObj2)[["table"]]
		pObj2 <- as.FLMatrix(pObj2)
		return(pObj1==pObj2)
	}
	return(stop("incomparable inputs"))
}

#' @export
`==.FLTable` <- function(pObj1,pObj2)
{
	if(!pObj1@isDeep)
	pObj1 <- wideToDeep(pObj1)[["table"]]
	pObj1 <- as.FLMatrix(pObj1)
	pObj2 <- as.FLMatrix(pObj2)
	return(pObj1==pObj2)
}


#' @export
`==.matrix` <- function(pObj1,pObj2)
{
	if(is.FL(pObj2)){
		pObj1 <- as.FLMatrix(pObj1)
		return(pObj1==pObj2)
	}
	else return(base::"=="(pObj1,pObj2))
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
	if(is.FL(pObj2))
	{
		pObj1 <- as.FLVector(pObj1)
		return("=="(pObj1,pObj2))	
	}
	else
	return(base::"=="(pObj1,pObj2))
}


#' @export
any <- function(...,na.rm=FALSE){
	return(FLanyall(...,na.rm=na.rm,
					vfunction="any"))
}

#' @export
all <- function(...,na.rm=FALSE){
	return(FLanyall(...,na.rm=na.rm,
					vfunction="all"))
}

FLanyall <- function(...,na.rm=FALSE,vfunction="all"){
	#browser()
	vlist <- list(...)
	vtemp <- unlist(lapply(vlist,
		function(x)
			return(is.FL(x))))

	ifelse(vfunction=="all",{
			vbasefunc <- base::all;
			vresult <- TRUE;
		},{
			vbasefunc <- base::any;
			vresult <- FALSE;
		})

    if(!base::any(vtemp))
    return(vbasefunc(...,na.rm=na.rm))

    vresult <- vbasefunc(unlist(vlist[!vtemp]))

    vlist <- vlist[vtemp]

    getColumnName <- function(x){
    	vmapp <- c(vectorValueColumn="FLVector",
    				valueColumn="FLMatrix",
    				cell_val_colname="FLTable")
    	vres <- names(vmapp)[vmapp==class(x)]
    	names(vres) <- NULL
    	return(vres)
    }
    vlength <- length(vlist)
    getTrueorFalse <- function(x,vfunction){
    	vrescolumn <- getColumnName(x)
    	vreqLogic <- ifelse(vfunction=="all",
    						fquote("FALSE"),
    						fquote("TRUE"))
    	vsqlstr <- paste0("SELECT a.",vrescolumn," \n ",
    				" FROM (",constructSelect(x),") AS a \n ",
    				" WHERE a.",vrescolumn," = ",vreqLogic
    				)
    	vresult <- sqlQuery(getFLConnection(),
    				vsqlstr)
    	if(nrow(vresult)>0){
	    	if(vfunction=="all") return(FALSE)
	    	else return(TRUE)}
    	else{
    		if(vfunction=="all") return(TRUE)
	    	else return(FALSE)}
    }
    ## for loop is used because instead of joining all inputs
    ## this would check sequentially for FALSE and when found
    ## terminates the loop.
    for(i in vlist){
    	if(is.FLTable(i) && !i@isDeep)
    	i <- wideToDeep(i)[["table"]]
    	if(vfunction=="all"){
	    	vresult <- (vresult && getTrueorFalse(x=i,vfunction=vfunction))
	    	if(!vresult) return(vresult)
	    }
    	else{
    		vresult <- (vresult || getTrueorFalse(x=i,vfunction=vfunction))
	    	if(vresult) return(vresult)
    	}
    }
    return(vresult)
}
