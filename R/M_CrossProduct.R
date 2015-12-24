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

#' Cross-Product of in-database objects.
#'
#' \code{\%*\%} does the Cross-Product of in-database objects.
#'
#' The Cross-Product of in-database objects mimics the \code{\%*\%} of R data types.
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param x can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param y can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{\%*\%} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix %*% Rvector
#' @export

"%*%" <- function(x,y)
{
    UseMethod("%*%", x)
}

`%*%.default` <- function(vec,flmatobj1)
{
	op <- .Primitive("%*%")
	op(vec,flmatobj1)
}

`%*%.matrix` <- function(x,flmatobj1)
{
	if(is.FLMatrix(flmatobj1))
	{
		if(ncol(x)!=nrow(flmatobj1)) {stop("non-conformable dimensions")}

		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj1))
		flmatobj2%*%flmatobj1
	}
	# else if(is.FLSparseMatrix(flmatobj1))
	# {
	# 	if(ncol(x)!=nrow(flmatobj1)) {stop("non-conformable dimensions")}

	# 	flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj1))
	# 	flmatobj2%*%flmatobj1
	# }
	else if(is.FLVector(flmatobj1))
	{
		if(length(flmatobj1)==ncol(x) || ncol(x)==1)
		{
			flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj1))
			flmatobj2%*%flmatobj1
		}
		else stop("non-conformable dimensions")
	}
	else 
	{
		op <- .Primitive("%*%")
		op(x,flmatobj1)
	}
}



`%*%.numeric` <- function(x,obj1)
{	
	if(missing(obj1))
	{
		op <- .Primitive("%*%")
		return(op(x))
	}
	if(is.FLMatrix(obj1))
	{
		if(nrow(obj1)==length(x))
		obj2 <- as.FLMatrix(matrix(x,1),getConnection(obj1))
		else if(nrow(obj1)==1)
		obj2 <- as.FLMatrix(matrix(x),getConnection(obj1))
		else
		stop("non-conformable dimensions")
		return(obj2 %*% obj1)
	}
	# else if(class(obj1)=="FLSparseMatrix")
	# {
	# 	if(nrow(obj1) != length(x)) stop("non-conformable dimensions")
	# 	obj2 <- as.FLVector(x,getConnection(obj1))
	# 	obj2 %*% obj1
	# }
	else if(class(obj1)=="FLVector")
	{
		if(length(obj1) != length(x)) stop("non-conformable dimensions")
		obj2 <- as.FLMatrix(matrix(x,1),getConnection(obj1))
		obj2 %*% obj1
	}
	else
	{
		op <- .Primitive("%*%")
		op(x,obj1)
	}
}

crossProdFLMatrix <- function(flmatobj1, flmatobj2)
{
	ncol1 <- ncol(flmatobj1)
	
	if(is.FLMatrix(flmatobj2))
	{
		nrow2 <- nrow(flmatobj2)

		if(ncol1 != nrow2)
		stop("non-conformable dimensions")

		flag1Check(getConnection(flmatobj1))

        ##gk: refactor to not store!
		vSqlStr<-paste0(" INSERT INTO ",
                        getRemoteTableName(result_db_name,result_matrix_table),
                        " SELECT ",max_matrix_id_value," AS MATRIX_ID ,
								 a.rowIdColumn AS ROW_ID ,
								 b.colIdColumn AS COL_ID , 
								 FLSUMPROD(a.valueColumn,b.valueColumn) AS CELL_VAL 
								 FROM (",constructSelect(flmatobj1),") AS a,
                                      (",constructSelect(flmatobj2),") AS b ",
                        constructWhere("a.colIdColumn = b.rowIdColumn"),
                        " GROUP BY ROW_ID,COL_ID")
						
		sqlSendUpdate(getConnection(flmatobj1), vSqlStr)

		MID <- max_matrix_id_value	
		max_matrix_id_value <<- max_matrix_id_value + 1
		return(FLMatrix( 
		       connection = getConnection(flmatobj1), 
		       database = result_db_name, 
		       matrix_table = result_matrix_table, 
			   matrix_id_value = MID,
			   matrix_id_colname = "MATRIX_ID", 
			   row_id_colname = "rowIdColumn", 
			   col_id_colname = "colIdColumn", 
			   cell_val_colname = "valueColumn",
			   ))
	}
	else if(is.vector(flmatobj2))
		{
			if(length(flmatobj2)==ncol1)
			flmatobj2 <- as.FLMatrix(matrix(flmatobj2),getConnection(flmatobj1))
			else if(ncol1==1)
			flmatobj2 <- as.FLMatrix(matrix(flmatobj2,1),getConnection(flmatobj1))
			else
			stop("non-conformable dimensions")
			return(flmatobj1 %*% flmatobj2)
		}
	else if(is.matrix(flmatobj2))
		{
			if(nrow(flmatobj2) != ncol1) stop("non-conformable dimensions")
			flmatobj2 <- as.FLMatrix(flmatobj2,getConnection(flmatobj1))
			flmatobj1 %*% flmatobj2
		}
	else if(class(flmatobj2)=="dgCMatrix"||class(flmatobj2)=="dgeMatrix"
		||class(flmatobj2)=="dsCMatrix"||class(flmatobj2)=="dgTMatrix")
		{
			if(nrow(flmatobj2) != ncol1) stop("non-conformable dimensions")
			flmatobj2 <- as.FLMatrix(flmatobj2,getConnection(flmatobj1))
			flmatobj1 %*% flmatobj2
		}
	else if(is.FLVector(flmatobj2))
		{
			if(length(flmatobj2) == ncol1)
			flmatobj2 <- as.FLMatrix(flmatobj2,getConnection(flmatobj1))
			else if(ncol1==1)
			flmatobj2 <- as.FLMatrix(flmatobj2,getConnection(flmatobj1),rows=1,cols=length(flmatobj2))
			else
			stop("non-conformable dimensions")

			
			    # vSqlStr<-paste0(" UPDATE ",vTempFlm@db_name,".",vTempFlm@matrix_table,
							# 	" FROM ( SELECT ",vTempFlm@matrix_id_value," AS mid ,
							# 					a.",getVariables(flmatobj1)$rowIdColumn," AS rid , 
							# 					CAST(((b.",flmatobj2@obs_id_colname,"-0.5)/",ncol(flmatobj1),")+1 as INT) AS cid , 
							# 					SUM(a.",getVariables(flmatobj1)$valueColumn,"*b.",flmatobj2@col_name,") AS cval 
							# 			 FROM ",remoteTable(flmatobj1)," a,",
							# 			 		remoteTable(flmatobj2)," b 
							# 			 WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
							# 			 AND a.",getVariables(flmatobj1)$colIdColumn,"=b.",flmatobj2@obs_id_colname,
							# 		    "-(CAST(((b.",flmatobj2@obs_id_colname,"-0.5)/",ncol(flmatobj1),") as INT) *",ncol(flmatobj1),") 
							# 		    GROUP BY 1,2,3",") c ",
							#     " SET ",vTempFlm@variables$valueColumn,"= c.cval ",
							#     " WHERE ",vTempFlm@matrix_id_colname,"= c.mid 
							#       AND ",vTempFlm@variables$rowIdColumn,"= c.rid 
							#       AND ",vTempFlm@variables$colIdColumn,"= c.cid")
			
			return(flmatobj1 %*% flmatobj2)
		}
	else stop("Operation Currently Not Supported")
}

`%*%.FLMatrixBind` <- crossProdFLMatrix
    
`%*%.FLMatrix` <- crossProdFLMatrix


`%*%.FLVector` <- function(pObj1,pObj2)
{
	if(is.vector(pObj2))
	{
		if(length(pObj1) != length(pObj2)) stop("non-conformable dimensions")
		pObj2 <- as.FLMatrix(matrix(pObj2),getConnection(pObj1))
		pObj1 %*% pObj2
	}
	else if(is.matrix(pObj2))
	{
		pObj2 <- as.FLMatrix(pObj2,getConnection(pObj1))
		pObj1 %*% pObj2
	}
	else if(class(pObj2)=="dgCMatrix"||class(pObj2)=="dgeMatrix"
		||class(pObj2)=="dsCMatrix"||class(pObj2)=="dgTMatrix")
	{
		pObj2 <- as.FLMatrix(pObj2,getConnection(pObj1))
		pObj1 %*% pObj2
	}
	else if(is.FLMatrix(pObj2))
	{

		if(length(pObj1) == nrow(pObj2))
		pObj1 <- as.FLMatrix(pObj1,pObj2@odbc_connection,rows=1,cols=length(pObj1))
		else if(nrow(pObj2)==1)
		pObj1 <- as.FLMatrix(pObj1,pObj2@odbc_connection)
		else
		stop(" non-conformable dimensions ")

			    # sqlSendUpdate(getConnection(flmatobj1),
			    # 		 paste0(" UPDATE ",vTempFlm@db_name,".",vTempFlm@matrix_table,
							# 	" FROM ( SELECT ",vTempFlm@matrix_id_value," AS mid ,
							# 					b.",flmatobj2@obs_id_colname,
							# 				    "-(CAST(((b.",flmatobj2@obs_id_colname,"-0.5)/1) as INT) * 1) AS rid , 
							# 				    a.",getVariables(flmatobj1)$colIdColumn," AS cid , 
							# 				    SUM(a.",getVariables(flmatobj1)$valueColumn,"*b.",flmatobj2@col_name,") AS cval 
							# 			 FROM ",remoteTable(flmatobj1)," a,",
							# 			 		remoteTable(flmatobj2)," b 
							# 			 WHERE a.",flmatobj1@matrix_id_colname,"=",flmatobj1@matrix_id_value," 
							# 			 AND a.",getVariables(flmatobj1)$rowIdColumn,"= CAST(((b.",flmatobj2@obs_id_colname,"-0.5)/1)+1 as INT) 
							# 			 GROUP BY 1,2,3",") c ",
							#     " SET ",vTempFlm@variables$valueColumn,"= c.cval ",
							#     " WHERE ",vTempFlm@matrix_id_colname,"= c.mid 
							#       AND ",vTempFlm@variables$rowIdColumn,"= c.rid 
							#       AND ",vTempFlm@variables$colIdColumn,"= c.cid"))

		return(pObj1 %*% pObj2)
	}
	else if(is.FLVector(pObj2))
	{
		if(length(pObj2) != length(pObj1)) stop(" non-conformable dimensions ")
		flmatobj1 <- as.FLMatrix(pObj1,getConnection(pObj1),rows=1,cols=length(pObj1))
		flmatobj2 <- as.FLMatrix(pObj2,pObj2@odbc_connection)
		return(flmatobj1 %*% flmatobj2)		
	}
	else cat("ERROR::Operation Currently Not Supported")
}

`%*%.dgCMatrix` <- function(x,flmatobj)
{
	if(is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 %*% flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 %*% flmatobj
	}
	else
	{
		op <- .Primitive("%*%")
		op(x,flmatobj)
	}
}

`%*%.dgeMatrix` <- function(x,flmatobj)
{
	if(is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 %*% flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 %*% flmatobj
	}
	else
	{
		op <- .Primitive("%*%")
		op(x,flmatobj)
	}
}

`%*%.dsCMatrix` <- function(x,flmatobj)
{
	if(is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 %*% flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 %*% flmatobj
	}
	else
	{
		op <- .Primitive("%*%")
		op(x,flmatobj)
	}
}

`%*%.dgTMatrix` <- function(x,flmatobj)
{
	if(is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 %*% flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 %*% flmatobj
	}
	else
	{
		op <- .Primitive("%*%")
		op(x,flmatobj)
	}
}
