#' @include utilities.R
#' @include FLIs.R
#' @include FLCastFunctions.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLSparseMatrix.R
#' @include FLTable.R
#' @include FLDims.R
#' @include FLPrint.R
NULL

#' Element-Wise Multiplication of in-database objects.
#'
#' \code{*} does the Element-wise Multiplication of in-database objects.
#'
#' The Element-wise Multiplication of in-database objects mimics the normal Element-wise Multiplication of R data types.
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param x can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param y can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{*} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 1)
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix * Rvector
#' @export

"*" <- function(x,y)
{
    UseMethod("*", x)
}

`*.default` <- function(vec,flmatobj1)
{
	op <- .Primitive("*")
	op(vec,flmatobj1)
}

`*.matrix` <- function(x,flmatobj1)
{
	if(is.FLMatrix(flmatobj1))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj1))
		flmatobj2*flmatobj1
	}
	else if(is.FLVector(flmatobj1))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj1))
		flmatobj2*flmatobj1
	}
	else 
	{
		op <- .Primitive("*")
		op(x,flmatobj1)
	}
}

`*.numeric` <- function(x,obj1)
{	if(is.FLMatrix(obj1))
	{
		obj2 <- as.FLVector(x,getConnection(obj1))
		obj2 * obj1
	}
	else if(class(obj1)=="FLVector")
	{
		obj2 <- as.FLVector(x,getConnection(obj1))
		obj2*obj1
	}
	else
	{
		op <- .Primitive("*")
		op(x,obj1)
	}
}

`*.FLMatrix` <- function(flmatobj1, flmatobj2)
{
	connection <- getConnection(flmatobj1)
	if(is.FLMatrix(flmatobj2))
	{
		###Phani-- division by 0 is ignored and 0 is returned.
		checkSameDims(flmatobj1,flmatobj2)
		flag1Check(getConnection(flmatobj1))
		a <- genRandVarName()
		b <- genRandVarName()

		sqlstr <-   paste0(" SELECT '%insertIDhere%' AS MATRIX_ID,",
	            		   			a,".rowIdColumn AS rowIdColumn,",
	            		   			a,".colIdColumn AS colIdColumn,",
	            		   			a,".valueColumn * ",b,".valueColumn AS valueColumn 
	            		    FROM ( ",constructSelect(flmatobj1),") AS ",a,
			                  ",( ",constructSelect(flmatobj2),") AS ",b,
	            			constructWhere(c(paste0(a,".rowIdColumn = ",b,".rowIdColumn"),
					 		  	paste0(a,".colIdColumn = ",b,".colIdColumn"),
			 		  			paste0(b,".valueColumn!=0"))))

		tblfunqueryobj <- new("FLTableFunctionQuery",
                        odbc_connection = connection,
                        variables=list(
                            rowIdColumn="rowIdColumn",
                            colIdColumn="colIdColumn",
                            valueColumn="valueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

		flm <- new("FLMatrix",
		            select= tblfunqueryobj,
		            dimnames=dimnames(flmatobj1))

		return(flm)
	}
	else if(is.vector(flmatobj2))
		{
			flmatobj2 <- as.FLMatrix(matrix(flmatobj2,nrow(flmatobj1),ncol(flmatobj1)),connection)
			flmatobj1*flmatobj2
		}
	else if(is.matrix(flmatobj2))
		{
			flmatobj2 <- as.FLMatrix(flmatobj2,connection)
			flmatobj1*flmatobj2
		}
	else if(class(flmatobj2)=="dgCMatrix"||class(flmatobj2)=="dgeMatrix"
			||class(flmatobj2)=="dsCMatrix"||class(flmatobj2)=="dgTMatrix")
		{
			flmatobj2 <- as.FLMatrix(flmatobj2,getConnection(flmatobj1))
			flmatobj1*flmatobj2
		}
	else if(is.FLVector(flmatobj2))
		{
			flmatobj2 <- as.FLMatrix(flmatobj2,getConnection(flmatobj1),
							sparse=TRUE,rows=nrow(flmatobj1),cols=ncol(flmatobj1))
				
			return(flmatobj1*flmatobj2)
		}
	else stop("Operation Currently Not Supported")
}

`*.FLVector` <- function(pObj1,pObj2)
{
	if(is.FLMatrix(pObj2))
	{
		flmatobj1 <- pObj2
		flmatobj2 <- as.FLMatrix(pObj1,getConnection(flmatobj1),
			sparse=TRUE,rows=nrow(flmatobj1),cols=ncol(flmatobj1))

		return(flmatobj2 * flmatobj1)
	}
	else if(is.vector(pObj2))
	{
		pObj2 <- as.FLVector(pObj2,getConnection(pObj1))
		pObj1 * pObj2
	}
	else if(is.matrix(pObj2))
	{
		pObj2 <- as.FLMatrix(pObj2,getConnection(pObj1))
		pObj1*pObj2
	}
	else if(class(pObj2)=="dgCMatrix"||class(pObj2)=="dgeMatrix"
		||class(pObj2)=="dsCMatrix"||class(pObj2)=="dgTMatrix")
	{
		pObj2 <- as.FLMatrix(pObj2,getConnection(pObj1))
		pObj1*pObj2
	}
	else if(is.FLVector(pObj2))
	{
		a <- genRandVarName()
		b <- genRandVarName()
		connection <- getConnection(pObj2)
		flag3Check(connection)

		if(nrow(pObj1)==1 && nrow(pObj2)==1)
		{
			if(ncol(pObj2)>ncol(pObj1))
			max_length <- ncol(pObj2)
			else max_length <- ncol(pObj1)
			newColnames1 <- renameDuplicates(colnames(pObj1))
			newColnames2 <- renameDuplicates(colnames(pObj2))

			sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
									1:max_length," AS vectorIndexColumn,
									",a,".",newColnames1,
									" * ",b,".",newColnames2," AS vectorValueColumn 
							 FROM (",constructSelect(pObj1),") AS ",a,", 
							    (",constructSelect(pObj2),") AS ",b,
						    collapse=" UNION ALL ")


			tblfunqueryobj <- new("FLTableFunctionQuery",
                    odbc_connection = connection,
                    variables = list(
		                obs_id_colname = "vectorIndexColumn",
		                cell_val_colname = "vectorValueColumn"),
                    whereconditions="",
                    order = "",
                    SQLquery=sqlstr)

			flv <- new("FLVector",
						select = tblfunqueryobj,
						dimnames = list(1:max_length,
										"vectorValueColumn"),
						isDeep = FALSE)

			return(flv)

		}
		if(ncol(pObj1)==1 && ncol(pObj2)==1)
		{

			sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
									a,".vectorIndexColumn AS vectorIndexColumn,
									",a,".",pObj1@dimnames[[2]],
									" * ",b,".",pObj2@dimnames[[2]]," AS vectorValueColumn 
							 FROM (",constructSelect(pObj1),") AS ",a,", 
							    (",constructSelect(pObj2),") AS ",b,
							 constructWhere(c(paste0(a,".vectorIndexColumn = ",
											b,".vectorIndexColumn"))))

			tblfunqueryobj <- new("FLTableFunctionQuery",
                    odbc_connection = connection,
                    variables = list(
		                obs_id_colname = "vectorIndexColumn",
		                cell_val_colname = "vectorValueColumn"),
                    whereconditions="",
                    order = "",
                    SQLquery=sqlstr)

			flv <- new("FLVector",
						select = tblfunqueryobj,
						dimnames = list(1:length(pObj1),
										"vectorValueColumn"),
						isDeep = FALSE)

			return(flv)
		}

		if(ncol(pObj1)==1 && nrow(pObj2)==1)
		{
			if(ncol(pObj2)>nrow(pObj1))
			max_length <- ncol(pObj2)
			else max_length <- nrow(pObj1)

			newColnames1 <- renameDuplicates(colnames(pObj1))
			newColnames2 <- renameDuplicates(colnames(pObj2))

			sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
									1:max_length, " AS vectorIndexColumn",
									",",a,".",newColnames1,
									" * ",b,".",newColnames2," AS vectorValueColumn 
							 FROM (",constructSelect(pObj1),") AS ",a,", 
							    (",constructSelect(pObj2),") AS ",b,
							" WHERE ",a,".vectorIndexColumn IN('",pObj1@dimnames[[1]],"')",
						    collapse=" UNION ALL ")

			tblfunqueryobj <- new("FLTableFunctionQuery",
                    odbc_connection = connection,
                    variables = list(
		                obs_id_colname = "vectorIndexColumn",
		                cell_val_colname = "vectorValueColumn"),
                    whereconditions="",
                    order = "",
                    SQLquery=sqlstr)

			flv <- new("FLVector",
						select = tblfunqueryobj,
						dimnames = list(1:max_length,
										"vectorValueColumn"),
						isDeep = FALSE)

			return(flv)
		}

		if(nrow(pObj1)==1 && ncol(pObj2)==1)
		{
			if(nrow(pObj2)>ncol(pObj1))
			max_length <- nrow(pObj2)
			else max_length <- ncol(pObj1)

			newColnames1 <- renameDuplicates(colnames(pObj1))
			newColnames2 <- renameDuplicates(colnames(pObj2))

			sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
									1:max_length, " AS vectorIndexColumn",
									",",a,".",newColnames1,
									" * ",b,".",newColnames2," AS vectorValueColumn 
							 FROM (",constructSelect(pObj1),") AS ",a,", 
							    (",constructSelect(pObj2),") AS ",b,
							" WHERE ",b,".vectorIndexColumn IN('",pObj2@dimnames[[1]],"')",
						    collapse=" UNION ALL ")

			tblfunqueryobj <- new("FLTableFunctionQuery",
                    odbc_connection = connection,
                    variables = list(
		                obs_id_colname = "vectorIndexColumn",
		                cell_val_colname = "vectorValueColumn"),
                    whereconditions="",
                    order = "",
                    SQLquery=sqlstr)

			flv <- new("FLVector",
						select = tblfunqueryobj,
						dimnames = list(1:max_length,
										"vectorValueColumn"),
						isDeep = FALSE)

			return(flv)
		}
	}
	else cat("ERROR::Operation Currently Not Supported")
}

`*.dgCMatrix` <- function(x,flmatobj)
{
	if(is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 * flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 * flmatobj
	}
	else
	{
		op <- .Primitive("*")
		op(x,flmatobj)
	}

}

`*.dgeMatrix` <- function(x,flmatobj)
{
	if(is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 * flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 * flmatobj
	}
	else
	{
		op <- .Primitive("*")
		op(x,flmatobj)
	}

}

`*.dgTMatrix` <- function(x,flmatobj)
{
	if(is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 * flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 * flmatobj
	}
	else
	{
		op <- .Primitive("*")
		op(x,flmatobj)
	}

}

`*.dsCMatrix` <- function(x,flmatobj)
{
	if(is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 * flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 * flmatobj
	}
	else
	{
		op <- .Primitive("*")
		op(x,flmatobj)
	}
}
