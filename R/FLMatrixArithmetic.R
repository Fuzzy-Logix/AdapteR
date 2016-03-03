#' @include utilities.R
#' @include FLIs.R
#' @include FLCastFunctions.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLTable.R
#' @include FLDims.R
#' @include FLPrint.R
NULL

#' @export
"FLMatrixArithmetic" <- function(pObj1,pObj2,pOperator)
{
    UseMethod("FLMatrixArithmetic", pObj1)
}

#' @export
FLMatrixArithmetic.default <- function(pObj1,pObj2,pOperator)
{
	op <- .Primitive(pOperator)
	op(pObj1,pObj2)
}

#' @export
FLMatrixArithmetic.FLMatrix <- function(pObj1,pObj2,pOperator)
{
	connection <- getConnection(pObj1)
	if(is.FLMatrix(pObj2))
	{
		flag1Check(connection)
		if(pOperator %in% c("+","-","%/%","%%","/","*"))
		checkSameDims(pObj1,pObj2)
		else if(pOperator %in% c("%*%"))
				if(ncol(pObj1) != nrow(pObj2))
				stop("non-conformable dimensions")

		a <- genRandVarName()
		b <- genRandVarName()
		dimnames <- dimnames(pObj1)
                dims <- dim(pObj1)

		if(pOperator %in% c("%%","/","*"))
		sqlstr <-   paste0(" SELECT '%insertIDhere%' AS MATRIX_ID,",
	            		   			a,".rowIdColumn AS rowIdColumn,",
	            		   			a,".colIdColumn AS colIdColumn,",
	            		   			a,".valueColumn ",
	            		   			ifelse(pOperator=="%%"," MOD ",pOperator)," ",
	            		   			b,".valueColumn AS valueColumn 
	            		    FROM ( ",constructSelect(pObj1),") AS ",a,
			                  ",( ",constructSelect(pObj2),") AS ",b,
	            			constructWhere(c(paste0(a,".rowIdColumn = ",b,".rowIdColumn"),
					 		  	paste0(a,".colIdColumn = ",b,".colIdColumn"),
			 		  			paste0(b,".valueColumn<>0"))))

		if(pOperator %in% c("%/%"))
		sqlstr <-   paste0(" SELECT '%insertIDhere%' AS MATRIX_ID,",
	            		   			a,".rowIdColumn AS rowIdColumn,",
	            		   			a,".colIdColumn AS colIdColumn,
	            		   			CAST( ",a,".valueColumn / ",
	            		   			b,".valueColumn AS INT ) AS valueColumn 
	            		    FROM ( ",constructSelect(pObj1),") AS ",a,
			                  ",( ",constructSelect(pObj2),") AS ",b,
	            			constructWhere(c(paste0(a,".rowIdColumn = ",b,".rowIdColumn"),
					 		  	paste0(a,".colIdColumn = ",b,".colIdColumn"),
			 		  			paste0(b,".valueColumn<>0"))))

		if(pOperator %in% c("%*%"))
		{
			sqlstr <-paste0(" SELECT '%insertIDhere%' AS MATRIX_ID,",
									 a,".rowIdColumn AS rowIdColumn,",
									 b,".colIdColumn AS colIdColumn,
									 SUM(",a,".valueColumn * ",b,".valueColumn) AS valueColumn  
									 FROM (",constructSelect(pObj1),") AS ",a,
	                                    ",(",constructSelect(pObj2),") AS ",b,
	                        constructWhere(paste0(a,".colIdColumn = ",b,".rowIdColumn")),
	                        " GROUP BY 1,2,3")
			dimnames <- list(dimnames(pObj1)[[1]],
                                         dimnames(pObj2)[[2]])
                        dims <- c(dim(pObj1)[[1]],
                                  dim(pObj2)[[2]])
		}

                ## todo gk,phani,kumar: implement with group by and test performance:
                ##
                ## select col, row, sum(vals)
                ## from (select * from a union all select * from b)
                ## group by col, row
		if(pOperator %in% c("+","-"))
		{
			sqlstr <-paste0(" SELECT DISTINCT '%insertIDhere%' AS MATRIX_ID,
								",a,".rowIdColumn AS rowIdColumn,
								",a,".colIdColumn AS colIdColumn,
								",a,".valueColumn AS valueColumn  
						 FROM ( ",constructSelect(pObj1),") AS ",a,
			            " except ",
			            "SELECT '%insertIDhere%' AS MATRIX_ID,
			            		",a,".rowIdColumn AS rowIdColumn,
			            		",a,".colIdColumn AS colIdColumn,
			            		",a,".valueColumn AS valueColumn 
			              FROM  ( ",constructSelect(pObj1),") AS ",a,
			                  ",( ",constructSelect(pObj2),") AS ",b,
			            constructWhere(c(paste0(a,".rowIdColumn = ",b,".rowIdColumn"),
					 		  	paste0(a,".colIdColumn = ",b,".colIdColumn"))),
			            " UNION ALL ",
			            " SELECT DISTINCT '%insertIDhere%' AS MATRIX_ID,
								",b,".rowIdColumn AS rowIdColumn,
								",b,".colIdColumn AS colIdColumn,
								",b,".valueColumn*(",pOperator,"1) AS valueColumn 
						 FROM  ( ",constructSelect(pObj2),") AS ",b,
			            " except ",
			            "SELECT '%insertIDhere%' AS MATRIX_ID,
			            		",b,".rowIdColumn AS rowIdColumn,",
			            		b,".colIdColumn AS colIdColumn,",
			            		b,".valueColumn*(",pOperator,"1) AS valueColumn 
			             FROM ( ",constructSelect(pObj1),") AS ",a,
			                ",( ",constructSelect(pObj2),") AS ",b,
			            constructWhere(c(paste0(a,".rowIdColumn = ",b,".rowIdColumn"),
					 		  	paste0(a,".colIdColumn = ",b,".colIdColumn"))),
			            " UNION ALL ",
			            " SELECT '%insertIDhere%' AS MATRIX_ID,
			            		",a,".rowIdColumn AS rowIdColumn,",
			            		a,".colIdColumn AS colIdColumn,",
			            		a,".valueColumn ",pOperator," ",b,".valueColumn AS valueColumn 
			              FROM  ( ",constructSelect(pObj1),") AS ",a,
			                  ",( ",constructSelect(pObj2),") AS ",b,
			            constructWhere(c(paste0(a,".rowIdColumn = ",b,".rowIdColumn"),
					 		  	paste0(a,".colIdColumn = ",b,".colIdColumn"))))
		}

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
                           dim=dims,
                           dimnames=dimnames)

		return(ensureQuerySize(pResult=flm,
						pInput=list(pObj1,pObj2),
						pOperator=pOperator))

		# return(flm)
	}
	else if(is.vector(pObj2))
		{
			if(pOperator %in% c("+","-","%/%","%%","/","*"))
			pObj2 <- as.FLMatrix(matrix(pObj2,nrow(pObj1),ncol(pObj1)))
			else if(pOperator %in% c("%*%"))
			{
				if(length(pObj2)==ncol(pObj1))
				pObj2 <- as.FLMatrix(matrix(pObj2))
				else if(ncol(pObj1)==1)
				pObj2 <- as.FLMatrix(matrix(pObj2,1))
				else
				stop("non-conformable dimensions")
			}

			return(do.call(pOperator,list(pObj1,pObj2)))
		}
	else if(is.matrix(pObj2)||class(pObj2)=="dgCMatrix"||class(pObj2)=="dgeMatrix"
			||class(pObj2)=="dsCMatrix"||class(pObj2)=="dgTMatrix")
		{
			pObj2 <- as.FLMatrix(pObj2)
			return(do.call(pOperator,list(pObj1,pObj2)))
		}
	else if(is.FLVector(pObj2))
		{

			if(pOperator %in% c("+","-","%/%","%%","/","*"))
			pObj2 <- as.FLMatrix(pObj2,
                                             sparse=TRUE,rows=nrow(pObj1),cols=ncol(pObj1))
			else if(pOperator %in% c("%*%"))
			{
				if(length(pObj2) == ncol(pObj1))
				pObj2 <- as.FLMatrix(pObj2)
				else if(ncol(pObj1)==1)
				pObj2 <- as.FLMatrix(pObj2,rows=1,cols=length(pObj2))
				else
				stop("non-conformable dimensions")
			}

			return(do.call(pOperator,list(pObj1,pObj2)))
		}
	else stop("Operation Currently Not Supported")
}

#' @export
FLMatrixArithmetic.FLVector <- function(pObj1,pObj2,pOperator)
{
	connection <- getConnection(pObj1)
	if(is.FLMatrix(pObj2))
	{
		if(pOperator %in% c("%*%"))
		  if(length(pObj1) == nrow(pObj2))
			pObj1 <- as.FLMatrix(pObj1,rows=1,cols=length(pObj1))
		  else if(nrow(pObj2)==1)
			pObj1 <- as.FLMatrix(pObj1)
			else
			stop(" non-conformable dimensions ")
		else if(pOperator %in% c("+","-","%/%","%%","/","*"))
		pObj1 <- as.FLMatrix(pObj1,
					sparse=TRUE,rows=nrow(pObj2),cols=ncol(pObj2))
		
		return(do.call(pOperator,list(pObj1,pObj2)))
	}
	else if(is.vector(pObj2))
	{
		if(pOperator %in% c("%*%"))
		  if(length(pObj1) != length(pObj2))
			stop("non-conformable dimensions")
		  else
		    pObj2 <- as.FLMatrix(matrix(pObj2))
		else if(pOperator %in% c("+","-","%/%","%%","/","*"))
		pObj2 <- as.FLVector(pObj2)

		return(do.call(pOperator,list(pObj1,pObj2)))
	}
	else if(is.matrix(pObj2)||class(pObj2)=="dgCMatrix"
		    ||class(pObj2)=="dgeMatrix"||class(pObj2)=="dsCMatrix"
		    ||class(pObj2)=="dgTMatrix")
	{
		pObj2 <- as.FLMatrix(pObj2)
		return(do.call(pOperator,list(pObj1,pObj2)))
	}
	else if(is.FLVector(pObj2))
	{
		a <- genRandVarName()
		b <- genRandVarName()
		flag3Check(connection)

		if(pOperator %in% c("%*%"))
		{
			if(length(pObj2) != length(pObj1)) stop(" non-conformable dimensions ")
			pObj1 <- as.FLMatrix(pObj1,rows=1,cols=length(pObj1))
			pObj2 <- as.FLMatrix(pObj2)
			return(pObj1 %*% pObj2)
		}
		else if(pOperator %in% c("+","-","%/%","%%","/","*"))
		{

			if(nrow(pObj1)==1 && nrow(pObj2)==1)
			{
				if(ncol(pObj2)>ncol(pObj1))
				max_length <- ncol(pObj2)
				else max_length <- ncol(pObj1)
				newColnames1 <- renameDuplicates(colnames(pObj1))
				newColnames2 <- renameDuplicates(colnames(pObj2))

				if(pOperator %in% c("%/%"))
				sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
										1:max_length," AS vectorIndexColumn,
										CAST( ",a,".",newColnames1,
										"/",b,".",newColnames2," AS INT ) AS vectorValueColumn 
								 FROM (",constructSelect(pObj1),") AS ",a,", 
								    (",constructSelect(pObj2),") AS ",b,
							    collapse=" UNION ALL ")

				else if(pOperator %in% c("+","-","%%","/","*"))
				sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
										1:max_length," AS vectorIndexColumn,
										",a,".",newColnames1,
										ifelse(pOperator=="%%"," MOD ",pOperator),
										b,".",newColnames2," AS vectorValueColumn 
								 FROM (",constructSelect(pObj1),") AS ",a,", 
								    (",constructSelect(pObj2),") AS ",b,
							    collapse=" UNION ALL ")

				dimnames <- list(1:max_length,
								"vectorValueColumn")
			}
			if(ncol(pObj1)==1 && ncol(pObj2)==1)
			{
				max_length <- max(length(rownames(pObj1)),length(rownames(pObj2)))
				if(pOperator %in% c("%/%"))
				
				sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
									1:max_length, " AS vectorIndexColumn",
									",CAST(",a,".vectorValueColumn",
									"/",b,".vectorValueColumn AS INT) AS vectorValueColumn 
								 FROM (",constructSelect(pObj1),") AS ",a,", 
								    (",constructSelect(pObj2),") AS ",b,
								" WHERE ",a,".vectorIndexColumn IN('",rownames(pObj1),"') AND ",
									b,".vectorIndexColumn IN('",rownames(pObj2),"')",
							    collapse=" UNION ALL ")

				else if(pOperator %in% c("+","-","%%","/","*"))
				
				sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
										1:max_length, " AS vectorIndexColumn",
										",",a,".vectorValueColumn",
										ifelse(pOperator=="%%"," MOD ",pOperator),
										b,".vectorValueColumn AS vectorValueColumn 
								 FROM (",constructSelect(pObj1),") AS ",a,", 
								    (",constructSelect(pObj2),") AS ",b,
								" WHERE ",a,".vectorIndexColumn IN('",rownames(pObj1),"') AND ",
									b,".vectorIndexColumn IN('",rownames(pObj2),"')",
							    collapse=" UNION ALL ")

				dimnames <- list(1:max_length,
								"vectorValueColumn")
			}

			if(ncol(pObj1)==1 && nrow(pObj2)==1)
			{
				if(ncol(pObj2)>nrow(pObj1))
				max_length <- ncol(pObj2)
				else max_length <- nrow(pObj1)

				newColnames1 <- renameDuplicates(colnames(pObj1))
				newColnames2 <- renameDuplicates(colnames(pObj2))

				if(pOperator %in% c("%/%"))
				sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
									1:max_length, " AS vectorIndexColumn",
									",CAST(",a,".vectorValueColumn",
									"/",b,".",newColnames2," AS INT) AS vectorValueColumn 
								 FROM (",constructSelect(pObj1),") AS ",a,", 
								    (",constructSelect(pObj2),") AS ",b,
								" WHERE ",a,".vectorIndexColumn IN('",pObj1@dimnames[[1]],"')",
							    collapse=" UNION ALL ")

				else if(pOperator %in% c("+","-","%%","/","*"))
				sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
										1:max_length, " AS vectorIndexColumn",
										",",a,".vectorValueColumn",
										ifelse(pOperator=="%%"," MOD ",pOperator),
										b,".",newColnames2," AS vectorValueColumn 
								 FROM (",constructSelect(pObj1),") AS ",a,", 
								    (",constructSelect(pObj2),") AS ",b,
								" WHERE ",a,".vectorIndexColumn IN('",pObj1@dimnames[[1]],"')",
							    collapse=" UNION ALL ")

				dimnames <- list(1:max_length,
								"vectorValueColumn")
			}

			if(nrow(pObj1)==1 && ncol(pObj2)==1)
			{
				if(nrow(pObj2)>ncol(pObj1))
				max_length <- nrow(pObj2)
				else max_length <- ncol(pObj1)

				newColnames1 <- renameDuplicates(colnames(pObj1))
				newColnames2 <- renameDuplicates(colnames(pObj2))

				if(pOperator %in% c("%/%"))
				sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
									1:max_length, " AS vectorIndexColumn",
									",CAST(",a,".",newColnames1,
									"/",b,".vectorValueColumn AS INT) AS vectorValueColumn 
								 FROM (",constructSelect(pObj1),") AS ",a,", 
								    (",constructSelect(pObj2),") AS ",b,
								" WHERE ",b,".vectorIndexColumn IN('",pObj2@dimnames[[1]],"')",
							    collapse=" UNION ALL ")
				else if(pOperator %in% c("+","-","%%","/","*"))
				sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
										1:max_length, " AS vectorIndexColumn",
										",",a,".",newColnames1,
										ifelse(pOperator=="%%"," MOD ",pOperator),
										b,".vectorValueColumn AS vectorValueColumn 
								 FROM (",constructSelect(pObj1),") AS ",a,", 
								    (",constructSelect(pObj2),") AS ",b,
								" WHERE ",b,".vectorIndexColumn IN('",pObj2@dimnames[[1]],"')",
							    collapse=" UNION ALL ")

				dimnames <- list(1:max_length,
								"vectorValueColumn")
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

			return(ensureQuerySize(pResult=flv,
								pInput=list(pObj1,pObj2),
								pOperator=pOperator))

			# return(flv)
		}
	}
	else cat("ERROR::Operation Currently Not Supported")
}

#' @export
FLMatrixArithmetic.matrix <- function(pObj1,pObj2,pOperator)
{
	if(pOperator %in% c("+","-","%/%","%%","/","*"))
	return(FLMatrixArithmetic.sparseMatrix(pObj1,pObj2,pOperator))
	else if(pOperator %in% c("%*%"))
	{
		if((is.FLMatrix(pObj2) && ncol(pObj1)!=nrow(pObj2))||
			(is.FLVector(pObj2) && !(length(pObj2)==ncol(pObj1) || ncol(pObj1)==1)))
		stop("non-conformable dimensions")
		else return(FLMatrixArithmetic.sparseMatrix(pObj1,pObj2,pOperator))
	}
}

#' @export
FLMatrixArithmetic.numeric <- function(pObj1,pObj2,pOperator)
{	
	if(missing(pObj2))
	{
		op <- .Primitive(pOperator)
		return(op(pObj1))
	}
	if(pOperator %in% c("%*%"))
	{
		if(is.FLMatrix(pObj2))
		{
			connection <- getConnection(pObj2)
			if(nrow(pObj2)==length(pObj1))
			pObj1 <- as.FLMatrix(matrix(pObj1,1))
			else if(nrow(pObj2)==1)
			pObj1 <- as.FLMatrix(matrix(pObj1))
			else
			stop("non-conformable dimensions")
			return(pObj1 %*% pObj2)
		}
		else if(class(pObj2)=="FLVector")
		{
			connection <- getConnection(pObj2)
			if(length(pObj2) != length(pObj1)) stop("non-conformable dimensions")
			pObj1 <- as.FLMatrix(matrix(pObj1,1))
			return(pObj1 %*% pObj2)
		}
	}
	if(is.FLMatrix(pObj2) || is.FLVector(pObj2))
	{
		connection <- getConnection(pObj2)
		pObj1 <- as.FLVector(pObj1)
		return(do.call(pOperator,list(pObj1,pObj2)))
	}
	else
	return(FLMatrixArithmetic.default(pObj1,pObj2,pOperator))
}

#' @export
FLMatrixArithmetic.sparseMatrix <- function(pObj1,pObj2,pOperator)
{
	if(is.FLMatrix(pObj2)||is.FLVector(pObj2))
	{
		pObj1 <- as.FLMatrix(pObj1,getConnection(pObj2))
		return(do.call(pOperator,list(pObj1,pObj2)))
	}
	else
	return(FLMatrixArithmetic.default(pObj1,pObj2,pOperator))
}
#' @export
FLMatrixArithmetic.dgCMatrix <- FLMatrixArithmetic.sparseMatrix
#' @export
FLMatrixArithmetic.dgeMatrix <- FLMatrixArithmetic.sparseMatrix
#' @export
FLMatrixArithmetic.dgTMatrix <- FLMatrixArithmetic.sparseMatrix
#' @export
FLMatrixArithmetic.dsCMatrix <- FLMatrixArithmetic.sparseMatrix

NULL
#' Addition of in-database objects.
#'
#' \code{+} does the addition of in-database objects.
#'
#' The addition of in-database objects mimics the normal addition of R data types.
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param pObj1 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param pObj2 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{+} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' flmatrix <- FLMatrix("FL_DEMO", 
#' "tblMatrixMulti", 1,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix + Rvector
#' @export

"+" <- function(pObj1,pObj2)
{
    UseMethod("+", pObj1)
}

#' @export
`+.default` <- function(pObj1,pObj2)
return(FLMatrixArithmetic.default(pObj1,pObj2,"+"))

#' @export
`+.matrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"+"))

#' @export
`+.numeric` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"+"))

#' @export
`+.FLMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"+"))

#' @export
`+.FLVector` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"+"))

#' @export
`+.dgCMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"+"))

#' @export
`+.dgeMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"+"))

#' @export
`+.dsCMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"+"))

#' @export
`+.dgTMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"+"))

NULL
#' Subtraction of in-database objects.
#'
#' \code{-} does the subtraction of in-database objects.
#'
#' The subtraction of in-database objects mimics the normal subtraction of R data types.
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param pObj1 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param pObj2 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{-} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' flmatrix <- FLMatrix("FL_DEMO", 
#' "tblMatrixMulti", 2,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix - Rvector
#' @export

"-" <- function(pObj1,pObj2)
{
    UseMethod("-", pObj1)
}

#' @export
`-.default` <- function(pObj1,pObj2)
return(FLMatrixArithmetic.default(pObj1,pObj2,"-"))

#' @export
`-.matrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"-"))

#' @export
`-.numeric` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"-"))

#' @export
`-.FLMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"-"))

#' @export
`-.FLVector` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"-"))

#' @export
`-.dgCMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"-"))

#' @export
`-.dgeMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"-"))

#' @export
`-.dsCMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"-"))

#' @export
`-.dgTMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"-"))

NULL
#' Cross-Product of in-database objects.
#'
#' \code{\%*\%} does the Cross-Product of in-database objects.
#'
#' The Cross-Product of in-database objects mimics the \code{\%*\%} of R data types.
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param pObj1 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param pObj2 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{\%*\%} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' flmatrix <- FLMatrix("FL_DEMO", 
#' "tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix %*% Rvector
#' @export

"%*%" <- function(pObj1,pObj2)
{
    UseMethod("%*%", pObj1)
}

#' @export
`%*%.default` <- function(pObj1,pObj2)
return(FLMatrixArithmetic.default(pObj1,pObj2,"%*%"))

#' @export
`%*%.matrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"%*%"))

#' @export
`%*%.numeric` <- function(pObj1,pObj2)	
return(FLMatrixArithmetic(pObj1,pObj2,"%*%"))

#' @export
crossProdFLMatrix <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"%*%"))

#' @export
`%*%.FLMatrixBind` <- crossProdFLMatrix
 
#' @export
`%*%.FLMatrix` <- crossProdFLMatrix

#' @export
`%*%.FLVector` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"%*%"))

#' @export
`%*%.dgeMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"%*%"))

#' @export
`%*%.dsCMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"%*%"))

#' @export
`%*%.dgTMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"%*%"))

NULL
#' remainder of division on in-database objects.
#'
#' \code{\%\%} calculates the remainder of in-database object division.
#'
#' The remainder of in-database objects mimics the normal remainder of R data types.
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param pObj1 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param pObj2 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{\%\%} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @section Constraints: division by 0 gives inf in R, but is not supported for 
#' in-database objects
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' flmatrix <- FLMatrix("FL_DEMO", 
#' "tblMatrixMulti", 1,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix %% Rvector
#' @export

"%%" <- function(pObj1,pObj2)
{
    UseMethod("%%", pObj1)
}

#' @export
`%%.default` <- function(pObj1,pObj2)
return(FLMatrixArithmetic.default(pObj1,pObj2,"%%"))

#' @export
`%%.matrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"%%"))

#' @export
`%%.numeric` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"%%"))

#' @export
`%%.FLMatrix` <- function(pObj1, pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"%%"))

#' @export
`%%.FLVector` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"%%"))

#' @export
`%%.dgCMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"%%"))

#' @export
`%%.dgeMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"%%"))

#' @export
`%%.dgTMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"%%"))

#' @export
`%%.dsCMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"%%"))

NULL
#' Element-Wise Multiplication of in-database objects.
#'
#' \code{*} does the Element-wise Multiplication of in-database objects.
#'
#' The Element-wise Multiplication of in-database objects mimics the normal Element-wise Multiplication of R data types.
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param pObj1 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param pObj2 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{*} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' flmatrix <- FLMatrix("FL_DEMO", 
#' "tblMatrixMulti", 1,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix * Rvector
#' @export

"*" <- function(pObj1,pObj2)
{
    UseMethod("*", pObj1)
}

#' @export
`*.default` <- function(pObj1,pObj2)
return(FLMatrixArithmetic.default(pObj1,pObj2,"*"))

#' @export
`*.matrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"*"))

#' @export
`*.numeric` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"*"))

#' @export
`*.FLMatrix` <- function(pObj1, pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"*"))

#' @export
`*.FLVector` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"*"))

#' @export
`*.dgCMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"*"))

#' @export
`*.dgeMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"*"))

#' @export
`*.dgTMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"*"))

#' @export
`*.dsCMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"*"))

NULL
#' Element-wise Division of in-database objects.
#'
#' \code{/} does the Element-wise Division of in-database objects.
#'
#' The Element-wise Division of in-database objects mimics the \code{/} of R data types.
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param pObj1 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param pObj2 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{/} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @section Constraints: division by 0 gives inf in R, but is not supported for 
#' in-database objects
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' flmatrix <- FLMatrix("FL_DEMO", 
#' "tblMatrixMulti", 1,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix / Rvector
#' @export

"/" <- function(pObj1,pObj2)
{
    UseMethod("/", pObj1)
}

#' @export
`/.default` <- function(pObj1,pObj2)
return(FLMatrixArithmetic.default(pObj1,pObj2,"/"))

#' @export
`/.matrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"/"))

#' @export
`/.numeric` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"/"))

#' @export
`/.FLMatrix` <- function(pObj1, pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"/"))

#' @export
`/.FLVector` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"/"))

#' @export
`/.dgCMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"/"))

#' @export
`/.dgeMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"/"))

#' @export
`/.dgTMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"/"))

#' @export
`/.dsCMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"/"))

NULL
#' Integer Division of in-database objects.
#'
#' \code{\%/\%} does the Element-wise Integer Division of in-database objects.
#'
#' The Element-wise Integer Division of in-database objects mimics the \code{\%/\%} of R data types.
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param pObj1 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param pObj2 can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{\%/\%} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @section Constraints: division by 0 gives inf in R, but is not supported for 
#' in-database objects
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' flmatrix <- FLMatrix("FL_DEMO", 
#' "tblMatrixMulti", 1,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix %/% Rvector
#' @export

"%/%" <- function(pObj1,pObj2)
{
    UseMethod("%/%", pObj1)
}

#' @export
`%/%.default` <- function(pObj1,pObj2)
return(FLMatrixArithmetic.default(pObj1,pObj2,"%/%"))

#' @export
`%/%.matrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"%/%"))

#' @export
`%/%.numeric` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"%/%"))

#' @export
`%/%.FLMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"%/%"))

#' @export
`%/%.FLVector` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"%/%"))

#' @export
`%/%.dgCMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"%/%"))

#' @export
`%/%.dgeMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"%/%"))

#' @export
`%/%.dgTMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"%/%"))

#' @export
`%/%.dsCMatrix` <- function(pObj1,pObj2)
return(FLMatrixArithmetic(pObj1,pObj2,"%/%"))

