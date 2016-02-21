#' @include utilities.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

#' Matrix Diagonals
#'
#' Extract or replace the diagonal of a matrix, or construct a diagonal matrix.
#'
#' diag has three distinct usages:
#' x is a FLMatrix, when it extracts the diagonal.
#' x is a scalar (length-one FLVector) and the only argument, it returns a square identity matrix of size given by the scalar.
#' x is a FLVector, either of length at least 2. This returns a square matrix with the given diagonal entries.
#' @param x is an object of class FLMatrix or FLVector
#' @return If x is a FLMatrix then diag(x) returns the diagonal of x as FLVector object.
#'   If x is FLVector, the value is a diagonal square FLMatrix with diagonal elements as given in FLVector.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' resultFLVector <- diag(flmatrix)
#' DeepTable <- FLTable(connection, "FL_TRAIN", "tblVectorDeep","vector_id","vector_key","vector_value")
#' flvectorDeep <- FLVector(DeepTable,"vector_value",1)
#' resultFLMatrix <- diag(flvectorDeep)
#' @export

diag<-function(x, ...)
{
	UseMethod("diag", x)
}


diag.default <- base::diag

diag.FLMatrix<-function(object)
{
	
	connection<-getConnection(object)
	flag3Check(connection)

	table <- FLTable(connection,
		             object@select@database,
		             paste0(object@select@table_name," AS mtrx "),
		             getVariables(object)$rowIdColumn,
		             whereconditions=c(object@select@whereconditions,
		             	paste0(getVariables(object)$rowIdColumn,
		             		"=",getVariables(object)$colIdColumn)))

	return(table[,getVariables(object)$valueColumn])
}

diag.FLVector <- function(object)
{
	connection <- getConnection(object)
	flag1Check(connection)

	if(length(object)==1)
	{
		flag1Check(connection)
		value <- as.vector(object)
		MID <- getMaxMatrixId(connection)

        sqlstr <- paste(sapply(1:value,FUN=function(i)
        				paste0(" INSERT INTO ",
        				getRemoteTableName(getOption("ResultDatabaseFL"),getOption("ResultMatrixTableFL")),
        				" SELECT ",MID,",",
        						   i,",",
        						   i,",",
        						   1)),collapse=";")

        sqlSendUpdate(connection,sqlstr)
	 	 

		return(FLMatrix( 
		 	        connection = connection, 
		 	        database = getOption("ResultDatabaseFL"), 
		 	        table_name = getOption("ResultMatrixTableFL"), 
		 	        matrix_id_value = MID,
			        matrix_id_colname = "MATRIX_ID", 
			        row_id_colname = "rowIdColumn", 
			        col_id_colname = "colIdColumn", 
			        cell_val_colname = "valueColumn"))
	
	}
	else if(length(object)>1)
	{
		if(object@isDeep)
		return(FLMatrix( 
		 	        connection = connection, 
		 	        database = object@select@database, 
		 	        table_name = object@select@table_name, 
		 	        matrix_id_value = "",
			        matrix_id_colname = "", 
			        row_id_colname = getVariables(object)$obs_id_colname, 
			        col_id_colname = getVariables(object)$obs_id_colname, 
			        cell_val_colname = getVariables(object)$cell_val_colname,
			        whereconditions = object@select@whereconditions
			        ))

		else
		{
			
			if(length(object@dimnames[[1]])==1)
			{
				MID <- getMaxMatrixId(connection)
		        sqlstr <- paste(sapply(1:length(object),FUN=function(i)
		        				paste0(" INSERT INTO ",
		        				getRemoteTableName(getOption("ResultDatabaseFL"),getOption("ResultMatrixTableFL")),
		        				" SELECT ",MID,",",
		        						   i,",",
		        						   i,",",
		        						   object@dimnames[[2]][i],
		        				" FROM ",getRemoteTableName(object@select@database,object@select@table_name),
		        				constructWhere(constraintsSQL(object)))),collapse=";")

		        sqlSendUpdate(connection,sqlstr)

				return(FLMatrix( 
				 	        connection = connection, 
				 	        database = getOption("ResultDatabaseFL"), 
				 	        table_name = getOption("ResultMatrixTableFL"), 
				 	        matrix_id_value = MID,
					        matrix_id_colname = "MATRIX_ID", 
					        row_id_colname = "rowIdColumn", 
					        col_id_colname = "colIdColumn", 
					        cell_val_colname = "valueColumn"
					        ))
			}
			else return(FLMatrix( 
				 	        connection = connection, 
				 	        database = object@select@database, 
				 	        table_name = object@select@table_name, 
				 	        matrix_id_value = "",
					        matrix_id_colname = "", 
					        row_id_colname = getVariables(object)$obs_id_colname, 
					        col_id_colname = getVariables(object)$obs_id_colname, 
					        cell_val_colname = object@dimnames[[2]],
					        whereconditions = object@select@whereconditions
					        ))
		}
	}
}
