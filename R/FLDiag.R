#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
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

diag.FLMatrix<-function(object){
	
	connection<-getConnection(object)
	flag3Check(connection)

	sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_vector_table,
					" SELECT ",max_vector_id_value,
					         ",a.",object@variables$rowId,
					         ",CAST(a.", object@variables$value,
					                "  AS NUMBER) 
	                  FROM ",remoteTable(object)," a
					  WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,"
					  AND a.",object@variables$rowId," = ",object@variables$colId,";")

	sqlSendUpdate(connection,sqlstr0)

	max_vector_id_value <<- max_vector_id_value + 1
	
	table <- FLTable(connection,
		             result_db_name,
		             result_vector_table,
		             "VECTOR_ID",
		             "VECTOR_INDEX",
                         "VECTOR_VALUE",
                         whereconditions=equalityConstraint(
                             paste0(
                                 remoteTable(result_db_name,result_vector_table),
                                 ".VECTOR_ID"),
                             max_vector_id_value-1))

	new("FLVector", table)
}

diag.FLVector <- function(object)
{
	connection <- getConnection(object)

	if(length(object)==1)
	{
		flag1Check(connection)
		value <- sqlQuery(connection,
						  paste("SELECT a.",object@variables$value,
						  		 "FROM ",remoteTable(object)," a",
						  		 "WHERE a.",object@obs_id_colname,"=",object@vector_id_value))[1,1]
        ## gk: refactor to apply, very bad performance
        for (i in 1:value)
		 for (j in 1:value)
		 {
		 	if(i!=j)
		 	sqlSendUpdate(connection,paste0(" INSERT INTO ",result_matrix_table,
		 		                       " SELECT ",max_matrix_id_value,",",
		 		                                 j,",",
		 		                                 i,",",
		 		                                 0))
		 	
		 	else

		 	sqlSendUpdate(connection,paste0(" INSERT INTO ",result_matrix_table,
		 		                       " SELECT ",max_matrix_id_value,",",
		 		                                  j,",",
		 		                                  i,",",
		 		                                  1))
	 	 }
	 	 
	 	 max_matrix_id_value <<- max_matrix_id_value + 1

		 return(FLMatrix( 
		 	        connection = connection, 
		 	        database = result_db_name, 
		 	        matrix_table = result_matrix_table, 
		 	        matrix_id_value = max_matrix_id_value-1,
			        matrix_id_colname = "MATRIX_ID", 
			        row_id_colname = "ROW_ID", 
			        col_id_colname = "COL_ID", 
			        cell_val_colname = "CELL_VAL",
			        nrow = length(object), 
			        ncol = length(object), 
			        dimnames = list(c(),c())))
	
	}
	else if(length(object)>1)
	{
		flag1Check(connection)
        for (i in 1:length(object))
			 for (j in 1:length(object))
			 {
			 	if(i!=j)
			 		sqlSendUpdate(connection,paste0(" INSERT INTO ",result_matrix_table,
			 			                       " SELECT ",max_matrix_id_value,",",
			 			                                  j,",",
			 			                                  i,",",
			 			                                  0))
			 	else 
			 	{
				 	if(object@isDeep)
				 	{
					 	sqlSendUpdate(connection,paste0(" INSERT INTO ",result_matrix_table,
					 		                       " SELECT ",max_matrix_id_value,",",
					 		                                  j,",",
					 		                                  i,
					 							              ",a.",object@var_id_colname,
					 							   " FROM ",remoteTable(object), " a ",
                                                   constructWhere(constraintsSQL(object))))
				 	}
			 	 	else if (!object@isDeep)
			 	 	{
					 	sqlSendUpdate(connection,paste0(" INSERT INTO ",result_matrix_table,
					 		                       " SELECT ",max_matrix_id_value,",",
					 		                                  j,",",
					 		                                  i,
					 							            ",a.",object@var_id_colname,
					 							   " FROM ",remoteTable(object),
					 							   " a WHERE a.",object@obs_id_colname,"=",i))
			 	 	}
		 	 	}
		 	 }
		 	 
		 	 max_matrix_id_value <<- max_matrix_id_value + 1
		 	 
			 return(FLMatrix( 
			 	        connection = connection, 
			 	        database = result_db_name, 
			 	        matrix_table = result_matrix_table, 
			 	        matrix_id_value = max_matrix_id_value-1,
				        matrix_id_colname = "MATRIX_ID", 
				        row_id_colname = "ROW_ID", 
				        col_id_colname = "COL_ID", 
				        cell_val_colname = "CELL_VAL",
				        nrow = length(object), 
				        ncol = length(object), 
				        dimnames = list(c(),c())))
	}
}
