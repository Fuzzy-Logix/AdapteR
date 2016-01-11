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

#' Subtraction of in-database objects.
#'
#' \code{-} does the subtraction of in-database objects.
#'
#' The subtraction of in-database objects mimics the normal subtraction of R data types.
#' All combinations of operands are possible just like in R and the result is an in-database object.
#' @param x can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @param y can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
#' a normal R object like matrix,sparseMatrix,vector
#' @return \code{-} returns an in-database object if there is atleast one in-database object 
#' as input.Otherwise, the default behavior of R is preserved
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 1)
#' Rvector <- 1:5
#' ResultFLmatrix <- flmatrix - Rvector
#' @export

"-" <- function(x,y)
{
    UseMethod("-", x)
}

`-.default` <- function(vec,flmatobj1)
{
	op <- .Primitive("-")
	op(vec,flmatobj1)
}

`-.matrix` <- function(x,flmatobj1)
{
	if(is.FLMatrix(flmatobj1))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj1))
		flmatobj2-flmatobj1
	}
	# else if(is.FLSparseMatrix(flmatobj1))
	# {
	# 	flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj1))
	# 	flmatobj2-flmatobj1
	# }
	else if(is.FLVector(flmatobj1))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj1))
		flmatobj2-flmatobj1
	}
	else 
	{
		op <- .Primitive("-")
		op(x,flmatobj1)
	}
}


`-.numeric` <- function(x,obj1)
{	
	if(missing(obj1))
	{
		op <- .Primitive("-")
		return(op(x))
	}
	if(is.FLMatrix(obj1))
	{

		obj2 <- as.FLVector(x,getConnection(obj1))
		obj2 - obj1
	}
	# else if(class(obj1)=="FLSparseMatrix")
	# {
	# 	obj2 <- as.FLVector(x,getConnection(obj1))
	# 	obj2-obj1
	# }
	else if(class(obj1)=="FLVector")
	{
		obj2 <- as.FLVector(x,getConnection(obj1))
		obj2-obj1
	}
	else
	{
		op <- .Primitive("-")
		op(x,obj1)
	}
}

`-.FLMatrix` <- function(flmatobj1, flmatobj2)
{
	connection <- getConnection(flmatobj1)
	nrow1 <- nrow(flmatobj1)
	ncol1 <- ncol(flmatobj1)
	if(is.FLMatrix(flmatobj2))
	{
		checkSameDims(flmatobj1,flmatobj2)
		flag1Check(getConnection(flmatobj1))
		#MID <- getMaxMatrixId(getConnection(flmatobj1))
		var1 <- genRandVarName()
		var2 <- genRandVarName()
        ## gk: todo:refactor
		sqlstr <-paste0(" SELECT DISTINCT '%insertIDhere%' AS MATRIX_ID,
								",var1,".rowIdColumn AS rowIdColumn,
								",var1,".colIdColumn AS colIdColumn,
								",var1,".valueColumn AS valueColumn  
						 FROM ( ",constructSelect(flmatobj1),") AS ",var1,
			            " except ",
			            "SELECT '%insertIDhere%' AS MATRIX_ID,
			            		",var1,".rowIdColumn AS rowIdColumn,
			            		",var1,".colIdColumn AS colIdColumn,
			            		",var1,".valueColumn AS valueColumn 
			              FROM  ( ",constructSelect(flmatobj1),") AS ",var1,
			                  ",( ",constructSelect(flmatobj2),") AS ",var2,
			            constructWhere(c(paste0(var1,".rowIdColumn = ",var2,".rowIdColumn"),
					 		  	paste0(var1,".colIdColumn = ",var2,".colIdColumn"))),
			            " UNION ALL ",
			            " SELECT DISTINCT '%insertIDhere%' AS MATRIX_ID,
								",var2,".rowIdColumn AS rowIdColumn,
								",var2,".colIdColumn AS colIdColumn,
								",var2,".valueColumn*(-1) AS valueColumn 
						 FROM  ( ",constructSelect(flmatobj2),") AS ",var2,
			            " except ",
			            "SELECT '%insertIDhere%' AS MATRIX_ID,
			            		",var2,".rowIdColumn AS rowIdColumn,",
			            		var2,".colIdColumn AS colIdColumn,",
			            		var2,".valueColumn*(-1) AS valueColumn 
			             FROM ( ",constructSelect(flmatobj1),") AS ",var1,
			                ",( ",constructSelect(flmatobj2),") AS ",var2,
			            constructWhere(c(paste0(var1,".rowIdColumn = ",var2,".rowIdColumn"),
					 		  	paste0(var1,".colIdColumn = ",var2,".colIdColumn"))),
			            " UNION ALL ",
			            " SELECT '%insertIDhere%' AS MATRIX_ID,
			            		",var1,".rowIdColumn AS rowIdColumn,",
			            		var1,".colIdColumn AS colIdColumn,",
			            		var1,".valueColumn - ",var2,".valueColumn AS valueColumn 
			              FROM  ( ",constructSelect(flmatobj1),") AS ",var1,
			                  ",( ",constructSelect(flmatobj2),") AS ",var2,
			            constructWhere(c(paste0(var1,".rowIdColumn = ",var2,".rowIdColumn"),
					 		  	paste0(var1,".colIdColumn = ",var2,".colIdColumn"))))
		
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
			flmatobj2 <- as.FLMatrix(matrix(flmatobj2,nrow(flmatobj1),ncol(flmatobj1)),getConnection(flmatobj1))
			flmatobj1-flmatobj2
		}
	else if(is.matrix(flmatobj2))
		{
			flmatobj2 <- as.FLMatrix(flmatobj2,getConnection(flmatobj1))
			flmatobj1-flmatobj2
		}
	else if(class(flmatobj2)=="dgCMatrix"||class(flmatobj2)=="dgeMatrix"
		||class(flmatobj2)=="dsCMatrix"||class(flmatobj2)=="dgTMatrix")
		{
			flmatobj2 <- as.FLMatrix(flmatobj2,getConnection(flmatobj1))
			flmatobj1-flmatobj2
		}
	else if(is.FLVector(flmatobj2))
		{
			flmatobj2 <- as.FLMatrix(flmatobj2,getConnection(flmatobj1),
							sparse=TRUE,rows=nrow(flmatobj1),cols=ncol(flmatobj1))

			# sqlstr0 <-paste0(" UPDATE ",
			# 				remoteTable(flmatobj2),
			# 		        " FROM ( SELECT DISTINCT ",flmatobj2@matrix_id_value," AS mid,
			# 		        				a.",getVariables(flmatobj2)$rowIdColumn," AS rid,
			# 		        				a.",getVariables(flmatobj2)$colIdColumn," AS cid,
			# 		        				a.",getVariables(flmatobj2)$valueColumn,"*(-1) AS cval 
			# 		        		 FROM ",remoteTable(flmatobj2)," a ",
			# 		        		 constructWhere(constraintsSQL(flmatobj2,"a")),") c ",
			# 		        " SET ",getVariables(flmatobj2)$valueColumn,"= c.cval ",
			# 		        constructWhere(c(paste0(flmatobj2@matrix_id_colname,"= c.mid "),
			# 		        	paste0(getVariables(flmatobj2)$rowIdColumn,"= c.rid "),
			# 		        	paste0(getVariables(flmatobj2)$colIdColumn,"= c.cid "))))
			# sqlstr1 <-paste0(" UPDATE ",
			# 				remoteTable(flmatobj2),
			# 				" FROM ( SELECT DISTINCT ",flmatobj2@matrix_id_value," AS mid,
			# 								a.",getVariables(flmatobj1)$rowIdColumn," AS rid,
			# 								a.",getVariables(flmatobj1)$colIdColumn," AS cid,
			# 								a.",getVariables(flmatobj1)$valueColumn,"+b.",
			# 			            			getVariables(flmatobj2)$valueColumn," AS cval 
			# 			             FROM ",remoteTable(flmatobj1)," a, ",
			# 			             		remoteTable(flmatobj2)," b ",
			# 			            constructWhere(c(constraintsSQL(flmatobj1,"a"),
			# 		 		  		constraintsSQL(flmatobj2,"b"),
			# 		 		  		paste0("a.",getVariables(flmatobj1)$rowIdColumn,"=b.",getVariables(flmatobj2)$rowIdColumn),
			# 		 		  		paste0("a.",getVariables(flmatobj1)$colIdColumn,"=b.",getVariables(flmatobj2)$colIdColumn))),
			# 			            ") c ",
			# 				" SET ",getVariables(flmatobj2)$valueColumn,"= c.cval ",
			# 				constructWhere(c(paste0(flmatobj2@matrix_id_colname,"= c.mid "),
			# 		        	paste0(getVariables(flmatobj2)$rowIdColumn,"= c.rid "),
			# 		        	paste0(getVariables(flmatobj2)$colIdColumn,"= c.cid "))))

			# sqlstr <- paste(sqlstr0,sqlstr1,sep=";")
			# sqlSendUpdate(getConnection(flmatobj1),sqlstr)

			return(flmatobj1-flmatobj2)
		}
	else stop("Operation Currently Not Supported")
}

`-.FLVector` <- function(pObj1,pObj2)
{
	if(is.vector(pObj2))
	{
		pObj2 <- as.FLVector(pObj2,getConnection(pObj1))
		pObj1-pObj2
	}
	else if(is.matrix(pObj2))
	{
		pObj2 <- as.FLMatrix(pObj2,getConnection(pObj1))
		pObj1-pObj2
	}
	else if(class(pObj2)=="dgCMatrix"||class(pObj2)=="dgeMatrix"
		||class(pObj2)=="dsCMatrix"||class(pObj2)=="dgTMatrix")
	{
		pObj2 <- as.FLMatrix(pObj2,getConnection(pObj1))
		pObj1-pObj2
	}
	else if(is.FLMatrix(pObj2))
	{
		flmatobj1 <- pObj2
		flmatobj2 <- as.FLMatrix(pObj1,getConnection(flmatobj1),
			sparse=TRUE,rows=nrow(flmatobj1),cols=ncol(flmatobj1))

		# sqlstr <-paste0(" UPDATE ",
		# 				remoteTable(flmatobj2),
		# 		        " FROM ( SELECT DISTINCT ",flmatobj2@matrix_id_value," AS mid,
		# 		        				a.",getVariables(flmatobj1)$rowIdColumn," AS rid,
		# 		        				a.",getVariables(flmatobj1)$colIdColumn," AS cid,
		# 		        				b.",getVariables(flmatobj2)$valueColumn,"-a.",getVariables(flmatobj1)$valueColumn," AS cval 
		# 		        		 FROM ",remoteTable(flmatobj1)," a, ",
		# 		        		 		remoteTable(flmatobj2)," b ",
		# 		        		 constructWhere(c(constraintsSQL(flmatobj1,"a"),
		# 			 		  		constraintsSQL(flmatobj2,"b"),
		# 			 		  		paste0("a.",getVariables(flmatobj1)$rowIdColumn,"=b.",getVariables(flmatobj2)$rowIdColumn),
		# 			 		  		paste0("a.",getVariables(flmatobj1)$colIdColumn,"=b.",getVariables(flmatobj2)$colIdColumn))),
		# 		        		 ") c ",
		# 				" SET ",getVariables(flmatobj2)$valueColumn,"= c.cval ",
		# 				constructWhere(c(paste0(flmatobj2@matrix_id_colname,"= c.mid "),
		# 			        	paste0(getVariables(flmatobj2)$rowIdColumn,"= c.rid "),
		# 			        	paste0(getVariables(flmatobj2)$colIdColumn,"= c.cid "))))

		# sqlQuery(getConnection(flmatobj1),sqlstr)
		return(flmatobj2-flmatobj1)
	}
	else if(is.FLVector(pObj2))
	{
		connection <- getConnection(pObj2)
		flag3Check(connection)
		a <- genRandVarName()
		b <- genRandVarName()

		if(nrow(pObj1)==1 && nrow(pObj2)==1)
		{
			if(ncol(pObj2)>ncol(pObj1))
			max_length <- ncol(pObj2)
			else max_length <- ncol(pObj1)
			newColnames1 <- renameDuplicates(colnames(pObj1))
			newColnames2 <- renameDuplicates(colnames(pObj2))

			sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn",
									",",1:max_length, " AS vectorIndexColumn",
									",",a,".",newColnames1,
									"-",b,".",newColnames2," AS vectorValueColumn",
							" FROM (",constructSelect(pObj1),") AS ",a,", 
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
			#return(store(object=flv))
		}

		if(ncol(pObj1)==1 && ncol(pObj2)==1)
		{
			## Phani-- Subtraction is done based on matching of
			## primary keys in both vectors.If one vector is short, it is
            ## not repeated as is done in R.
            ##
            ## gk: we can defer this for now, we need modulo.
            ### Phani-- modulo approach I used in earlier version,
            ### But here primary key can be non-numeric
			sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn",
									",",a,".vectorIndexColumn,
									",a,".",pObj1@dimnames[[2]],
									"-",b,".",pObj2@dimnames[[2]]," AS vectorValueColumn",
							" FROM (",constructSelect(pObj1),") AS ",a,", 
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
			# return(store(object=flv))
		}

		if(ncol(pObj1)==1 && nrow(pObj2)==1)
		{
			if(ncol(pObj2)>nrow(pObj1))
			max_length <- ncol(pObj2)
			else max_length <- nrow(pObj1)
			newColnames1 <- renameDuplicates(colnames(pObj1))
			newColnames2 <- renameDuplicates(colnames(pObj2))

			sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn",
									",",1:max_length, " AS vectorIndexColumn",
									",",a,".",newColnames1,
									"-",b,".",newColnames2," AS vectorValueColumn 
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
						dimnames = list(1:length(pObj1),
										"vectorValueColumn"),
						isDeep = FALSE)

			return(flv)
			# return(store(object=flv))
		}

		if(nrow(pObj1)==1 && ncol(pObj2)==1)
		{
			if(nrow(pObj2)>ncol(pObj1))
			max_length <- nrow(pObj2)
			else max_length <- ncol(pObj1)
			newColnames1 <- renameDuplicates(colnames(pObj1))
			newColnames2 <- renameDuplicates(colnames(pObj2))

			sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn",
									",",1:max_length, " AS vectorIndexColumn",
									",",a,".",newColnames1,
									"-",b,".",newColnames2," AS vectorValueColumn 
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
						dimnames = list(1:length(pObj1),
										  "vectorValueColumn"),
						isDeep = FALSE)

			return(flv)
			# return(store(object=flv))
		}
		
	}
	else cat("ERROR::Operation Currently Not Supported")
}


`-.dgCMatrix` <- function(x,flmatobj)
{
	if(is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 - flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 - flmatobj
	}
	else
	{
		op <- .Primitive("-")
		op(x,flmatobj)
	}

}

`-.dgeMatrix` <- function(x,flmatobj)
{
	if(is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 - flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 - flmatobj
	}
	else
	{
		op <- .Primitive("-")
		op(x,flmatobj)
	}

}

`-.dsCMatrix` <- function(x,flmatobj)
{
	if(is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 - flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 - flmatobj
	}
	else
	{
		op <- .Primitive("-")
		op(x,flmatobj)
	}

}

`-.dgTMatrix` <- function(x,flmatobj)
{
	if(is.FLMatrix(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 - flmatobj
	}
	else if(is.FLVector(flmatobj))
	{
		flmatobj2 <- as.FLMatrix(x,getConnection(flmatobj))
		flmatobj2 - flmatobj
	}
	else
	{
		op <- .Primitive("-")
		op(x,flmatobj)
	}

}
