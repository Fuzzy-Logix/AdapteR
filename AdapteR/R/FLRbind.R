#' @include utilities.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

#' Combine objects by rows.
#'
#' \code{rbind} combines input objects by rows and forms a FLMatrix.
#'
#' \code{rbind} takes a sequence of vector, FLVector, matrix, FLMatrix or data frames arguments,
#' combines them by rows and makes a FLMatrix.
#' @param x can be a sequence of vector, FLVector, matrix, FLMatrix or data frames
#' @param ... any additional arguments
#' @section Constraints:
#' Input matrices, FLMatrices and data frames should have same number of columns.
#' @return \code{rbind} returns a FLMatrix object which is the row-wise combination of input arguments.
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' flmatrix <- FLMatrix("FL_DEMO", 
#' "tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLMatrix <- rbind(flmatrix,1:5,flmatrix)
#' @export
rbind <- function (x, ...){
  UseMethod("rbind", x)
}

#' @export
rbind.default <- base::rbind

#' Combine objects by rows.
#'
#' \code{rbind} combines input objects by rows and forms a FLMatrix.
#'
#' \code{rbind} takes a sequence of vector, FLVector, matrix, FLMatrix or data frames arguments,
#' combines them by rows and makes a FLMatrix.
#' @param x... can be a sequence of vector, FLVector, matrix, FLMatrix or data frames
#' @section Constraints:
#' Input matrices, FLMatrices and data frames should have same number of columns.
#' @return \code{rbind} returns a FLMatrix object which is the row wise combination of input arguments.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLMatrix <- rbind(flmatrix,1:5,flmatrix)
#' @export
rbind.FLMatrix<-function(x,...) FLbind(list(x,...),1)

#' @export
rbind.FLMatrixBind <- rbind.FLMatrix
## {
## 	objectVec<-list(object,...)
## 	connection<-getConnection(object)

## 	ncol <-0

## 	for(j in 1:length(objectVec))
## 	{
## 		if(is.FLMatrix(objectVec[[j]]) || is.matrix(objectVec[[j]]) || is.data.frame(objectVec[[j]]))
## 		{
## 			if(ncol == 0)
## 			{
## 				ncol <- ncol(objectVec[[j]])
## 			}
## 			else if(ncol != ncol(objectVec[[j]]))
## 			stop(" number of columns of matrix arguments must match ")
## 		}
## 		else if(is.FLSparseMatrix(objectVec[[j]]) || class(objectVec[[j]])=="dgCMatrix")
## 		{
## 			stop("input parameter cannot be sparse")
## 		}
## 	}

## 	flag1Check(connection)

## 	rowCount <- 0
## 	ncol <- ncol(object)

## 	if(length(objectVec) == 1) 
## 	{
## 		return(object)
## 	}

## 	else if (length(objectVec) > 1)
## 	{
## 	    sqlstr0<-paste0("INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 		            " SELECT ",max_matrix_id_value,",
## 				              a.",getVariables(object)$rowIdColumn,",
## 				              a.",getVariables(object)$colIdColumn,",
## 				              a.",getVariables(object)$valueColumn," 
## 				    FROM   ",remoteTable(object)," a 
## 				    WHERE  a.",object@matrix_id_colname," = ",object@matrix_id_value)

## 	    sqlSendUpdate(connection,sqlstr0)

## 	    rowCount <- rowCount + nrow(object)

## 	    for (i in 2:length(objectVec))
## 		{
## 			if (class(objectVec[[i]]) == "FLMatrix")
## 			{
## 				object <- objectVec[[i]]

## 				sqlstr0<-paste0("INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 					            " SELECT ",max_matrix_id_value,",
## 							              a.",getVariables(object)$rowIdColumn,"+",rowCount,",
## 							              a.",getVariables(object)$colIdColumn,",
## 							              a.",getVariables(object)$valueColumn," 
## 							    FROM   ",remoteTable(object)," a 
## 							    WHERE  a.",object@matrix_id_colname," = ",object@matrix_id_value)

## 	            sqlSendUpdate(connection,sqlstr0)

##                 rowCount <- rowCount + nrow(object)
## 			}

## 			if (class(objectVec[[i]]) == "FLVector")
## 			{
## 				object <- objectVec[[i]]

## 				if(object@isDeep)
## 				{
## 					sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 						            " SELECT ",max_matrix_id_value,",
## 								             ",rowCount+1,",
## 								             a.",object@var_id_name,",
## 								             a.",object@col_name," 
## 								      FROM   ",remoteTable(object)," a 
## 								      WHERE  a.",object@obs_id_colname," = ",object@vector_id_value," AND 
## 								             a.",object@var_id_name,"<",ncol+1)
	
## 		            sqlSendUpdate(connection,sqlstr0)

## 		            if(length(object) < ncol)
## 		            {
## 		            	for(k in 1:(ncol%/%length(object)))
## 		            	{
## 		            		sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 								            " SELECT ",max_matrix_id_value,",
## 										             ",rowCount+1,",
## 										             (a.",object@var_id_name,"+(",k,"*",length(object),")),
## 										             a.",object@col_name," 
## 										      FROM ",remoteTable(object)," a 
## 										      WHERE a.",object@obs_id_colname," = ",object@vector_id_value," AND 
## 										            (a.",object@var_id_name,"+(",k,"*",length(object),"))<",ncol+1)
	
## 		                    sqlSendUpdate(connection,sqlstr0)
## 		            	}
## 		            }
## 		        }
## 		        else if(!object@isDeep)
## 		        {
## 					sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 						            " SELECT ",max_matrix_id_value,",
## 								             ",rowCount+1,",
## 								             a.",object@obs_id_colname,",
## 								             a.",object@col_name," 
## 								      FROM   ",remoteTable(object)," a 
## 								      WHERE a.",object@obs_id_colname,"<",ncol+1)
	
## 		            sqlSendUpdate(connection,sqlstr0)

## 		            if(length(object) < ncol)
## 		            {
## 		            	for(k in 1:(ncol%/%length(object)))
## 		            	{
## 		            		sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 								            " SELECT ",max_matrix_id_value,",
## 										             ",rowCount+1,",
## 										             (a.",object@obs_id_colname,"+(",k,"*",length(object),")),
## 										              a.",object@col_name," 
## 										      FROM ",remoteTable(object)," a 
## 										      WHERE (a.",object@obs_id_colname,"+(",k,"*",length(object),"))<",ncol+1)
	
## 		                    sqlSendUpdate(connection,sqlstr0)
## 		            	}
## 		            }
## 		        }

## 		        rowCount <- rowCount + 1
## 			}

## 			else if (is.vector(objectVec[[i]]))
## 			{
## 				object <- objectVec[[i]]

## 				j <- 1
## 				for(k in 1:ncol)
## 		        {
## 		            if(j > length(object))
## 		            j <- 1

## 		            sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 								    " SELECT ",max_matrix_id_value,",
## 										     ",rowCount+1,","
## 										      ,k,","
## 										      ,object[j])
	
## 		            sqlSendUpdate(connection,sqlstr0)
## 		            j <- j+1
## 		        }

## 		        rowCount <- rowCount + 1
## 			}

## 			else if(is.matrix(objectVec[[i]]) || is.data.frame(objectVec[[i]]))
## 			{
## 				object <- objectVec[[i]]

## 				for (i in 1:ncol(object))
## 				for (j in (rowCount+1):(rowCount+nrow(object)))
## 				{
				 	
## 			 	 	sqlSendUpdate(connection,paste0(" INSERT INTO ",getOption("ResultMatrixTableFL"),
## 			 	 		                       " SELECT ",max_matrix_id_value,",",
## 			 	 		                                  j,",",
## 			 	 		                                  i,",",
## 			 	 		                                  object[(j-rowCount),i]))
			 	 
## 			 	}

## 			 	rowCount <- rowCount + nrow(object)
## 			}
## 		}


## 		max_matrix_id_value <<- max_matrix_id_value + 1

## 		return(FLMatrix( 
## 			       connection = connection, 
## 			       database = getOption("ResultDatabaseFL"), 
## 			       table_name = getOption("ResultMatrixTableFL"), 
## 				   matrix_id_value = max_matrix_id_value-1,
## 				   matrix_id_colname = "MATRIX_ID", 
## 				   row_id_colname = "rowIdColumn", 
## 				   col_id_colname = "colIdColumn", 
## 				   cell_val_colname = "valueColumn",
## 				   nrow = rowCount, 
## 				   ncol = ncol, 
## 				   dimnames = list(c(),c())))
## 	}
## }

## rbind.FLVector <- function(object,...)
## {
## 	objectVec<-list(object,...)
## 	connection<-getConnection(object)

## 	ncol <-0

## 	for(j in 1:length(objectVec))
## 	{
## 		if(is.FLMatrix(objectVec[[j]]) || is.matrix(objectVec[[j]]) || is.data.frame(objectVec[[j]]))
## 		{
## 			if(ncol ==0)
## 			{
## 				ncol <- ncol(objectVec[[j]])
## 			}
## 			else if(ncol != ncol(objectVec[[j]]))
## 			stop(" number of columns of matrix arguments must match ")
## 		}
## 		else if(is.FLSparseMatrix(objectVec[[j]]) || class(objectVec[[j]])=="dgCMatrix")
## 		{
## 			stop("input parameter cannot be sparse")
## 		}
## 	}

## 	if(ncol == 0)
## 	{
## 		for(j in 1:length(objectVec))
## 		{
## 			if(is.FLVector(objectVec[[j]]) || is.numeric(objectVec[[j]]))
## 			{
## 				if(ncol < length(objectVec[[j]]))
## 				{
## 					ncol <- length(objectVec[[j]])
## 				}
## 			}
## 		}
## 	}

## 	flag1Check(connection)

## 	rowCount <- 0

## 	if(length(objectVec) == 1) 
## 	{
## 		if(object@isDeep)
## 		{
## 			sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 				            " SELECT ",max_matrix_id_value,",
## 						             ",rowCount+1,",
## 						             a.",object@var_id_name,",
## 						             a.",object@col_name," 
## 						      FROM   ",remoteTable(object)," a 
## 						      WHERE  a.",object@obs_id_colname," = ",object@vector_id_value," AND 
## 						             a.",object@var_id_name,"<",ncol+1)

##             sqlSendUpdate(connection,sqlstr0)

##             if(length(object) < ncol)
##             {
##             	for(k in 1:(ncol%/%length(object)))
##             	{
##             		sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 						            " SELECT ",max_matrix_id_value,",
## 								             ",rowCount+1,",
## 								             (a.",object@var_id_name,"+(",k,"*",length(object),")),
## 								             a.",object@col_name," 
## 								      FROM ",remoteTable(object)," a 
## 								      WHERE a.",object@obs_id_colname," = ",object@vector_id_value," AND 
## 								            (a.",object@var_id_name,"+(",k,"*",length(object),"))<",ncol+1)

##                     sqlSendUpdate(connection,sqlstr0)
##             	}
##             }
##         }
##         else if(!object@isDeep)
##         {
## 			sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 				            " SELECT ",max_matrix_id_value,",
## 						             ",rowCount+1,",
## 						             a.",object@obs_id_colname,",
## 						             a.",object@col_name," 
## 						      FROM   ",remoteTable(object)," a 
## 						      WHERE a.",object@obs_id_colname,"<",ncol+1)

##             sqlSendUpdate(connection,sqlstr0)

##             if(length(object) < ncol)
##             {
##             	for(k in 1:(ncol%/%length(object)))
##             	{
##             		sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 						            " SELECT ",max_matrix_id_value,",
## 								             ",rowCount+1,",
## 								             (a.",object@obs_id_colname,"+(",k,"*",length(object),")),
## 								              a.",object@col_name," 
## 								      FROM ",remoteTable(object)," a 
## 								      WHERE (a.",object@obs_id_colname,"+(",k,"*",length(object),"))<",ncol+1)

##                     sqlSendUpdate(connection,sqlstr0)
##             	}
##             }
##         }

##         max_matrix_id_value <<- max_matrix_id_value + 1

## 		return(FLMatrix( 
## 			       connection = connection, 
## 			       database = getOption("ResultDatabaseFL"), 
## 			       table_name = getOption("ResultMatrixTableFL"), 
## 				   matrix_id_value = max_matrix_id_value-1,
## 				   matrix_id_colname = "MATRIX_ID", 
## 				   row_id_colname = "rowIdColumn", 
## 				   col_id_colname = "colIdColumn", 
## 				   cell_val_colname = "valueColumn",
## 				   nrow = 1, 
## 				   ncol = ncol, 
## 				   dimnames = list(c(),c())))
## 	}

## 	else if (length(objectVec) > 1)
## 	{
## 	    if(object@isDeep)
## 		{
## 			sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 				            " SELECT ",max_matrix_id_value,",
## 						             ",rowCount+1,",
## 						             a.",object@var_id_name,",
## 						             a.",object@col_name," 
## 						      FROM   ",remoteTable(object)," a 
## 						      WHERE  a.",object@obs_id_colname," = ",object@vector_id_value," AND 
## 						             a.",object@var_id_name,"<",ncol+1)

##             sqlSendUpdate(connection,sqlstr0)

##             if(length(object) < ncol)
##             {
##             	for(k in 1:(ncol%/%length(object)))
##             	{
##             		sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 						            " SELECT ",max_matrix_id_value,",
## 								             ",rowCount+1,",
## 								             (a.",object@var_id_name,"+(",k,"*",length(object),")),
## 								             a.",object@col_name," 
## 								      FROM ",remoteTable(object)," a 
## 								      WHERE a.",object@obs_id_colname," = ",object@vector_id_value," AND 
## 								            (a.",object@var_id_name,"+(",k,"*",length(object),"))<",ncol+1)

##                     sqlSendUpdate(connection,sqlstr0)
##             	}
##             }
##         }
##         else if(!object@isDeep)
##         {
## 			sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 				            " SELECT ",max_matrix_id_value,",
## 						             ",rowCount+1,",
## 						             a.",object@obs_id_colname,",
## 						             a.",object@col_name," 
## 						      FROM   ",remoteTable(object)," a 
## 						      WHERE a.",object@obs_id_colname,"<",ncol+1)

##             sqlSendUpdate(connection,sqlstr0)

##             if(length(object) < ncol)
##             {
##             	for(k in 1:(ncol%/%length(object)))
##             	{
##             		sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 						            " SELECT ",max_matrix_id_value,",
## 								             ",rowCount+1,",
## 								             (a.",object@obs_id_colname,"+(",k,"*",length(object),")),
## 								              a.",object@col_name," 
## 								      FROM ",remoteTable(object)," a 
## 								      WHERE (a.",object@obs_id_colname,"+(",k,"*",length(object),"))<",ncol+1)

##                     sqlSendUpdate(connection,sqlstr0)
##             	}
##             }
##         }

## 	    rowCount <- rowCount + 1

## 	    for (i in 2:length(objectVec))
## 		{
## 			if (class(objectVec[[i]]) == "FLMatrix")
## 			{
## 				object <- objectVec[[i]]

## 				if(ncol(object) != ncol)
## 				{
## 					stop(" number of columns of matrix arguments must match ")
## 				}
## 				sqlstr0<-paste0("INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 					            " SELECT ",max_matrix_id_value,",
## 							              a.",getVariables(object)$rowIdColumn,"+",rowCount,",
## 							              a.",getVariables(object)$colIdColumn,",
## 							              a.",getVariables(object)$valueColumn," 
## 							    FROM   ",remoteTable(object)," a 
## 							    WHERE  a.",object@matrix_id_colname," = ",object@matrix_id_value)

## 	            sqlSendUpdate(connection,sqlstr0)

##                 rowCount <- rowCount + nrow(object)
## 			}

## 			if (class(objectVec[[i]]) == "FLVector")
## 			{
## 				object <- objectVec[[i]]

## 				if(object@isDeep)
## 				{
## 					sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 						            " SELECT ",max_matrix_id_value,",
## 								             ",rowCount+1,",
## 								             a.",object@var_id_name,",
## 								             a.",object@col_name," 
## 								      FROM   ",remoteTable(object)," a 
## 								      WHERE  a.",object@obs_id_colname," = ",object@vector_id_value," AND 
## 								             a.",object@var_id_name,"<",ncol+1)
	
## 		            sqlSendUpdate(connection,sqlstr0)

## 		            if(length(object) < ncol)
## 		            {
## 		            	for(k in 1:(ncol%/%length(object)))
## 		            	{
## 		            		sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 								            " SELECT ",max_matrix_id_value,",
## 										             ",rowCount+1,",
## 										             (a.",object@var_id_name,"+(",k,"*",length(object),")),
## 										             a.",object@col_name," 
## 										      FROM ",remoteTable(object)," a 
## 										      WHERE a.",object@obs_id_colname," = ",object@vector_id_value," AND 
## 										            (a.",object@var_id_name,"+(",k,"*",length(object),"))<",ncol+1)
	
## 		                    sqlSendUpdate(connection,sqlstr0)
## 		            	}
## 		            }
## 		        }
## 		        else if(!object@isDeep)
## 		        {
## 					sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 						            " SELECT ",max_matrix_id_value,",
## 								             ",rowCount+1,",
## 								             a.",object@obs_id_colname,",
## 								             a.",object@col_name," 
## 								      FROM   ",remoteTable(object)," a 
## 								      WHERE a.",object@obs_id_colname,"<",ncol+1)
	
## 		            sqlSendUpdate(connection,sqlstr0)

## 		            if(length(object) < ncol)
## 		            {
## 		            	for(k in 1:(ncol%/%length(object)))
## 		            	{
## 		            		sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 								            " SELECT ",max_matrix_id_value,",
## 										             ",rowCount+1,",
## 										             (a.",object@obs_id_colname,"+(",k,"*",length(object),")),
## 										              a.",object@col_name," 
## 										      FROM ",remoteTable(object)," a 
## 										      WHERE (a.",object@obs_id_colname,"+(",k,"*",length(object),"))<",ncol+1)
	
## 		                    sqlSendUpdate(connection,sqlstr0)
## 		            	}
## 		            }
## 		        }

## 		        rowCount <- rowCount + 1
## 			}

## 			else if (is.vector(objectVec[[i]]))
## 			{
## 				object <- objectVec[[i]]

## 				for(k in 1:ncol)
## 		        {
## 		            j <- k
## 		            if(j > length(object))
## 		            j <- 1

## 		            sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 								    " SELECT ",max_matrix_id_value,",
## 										     ",rowCount+1,","
## 										      ,k,","
## 										      ,object[j])
	
## 		            sqlSendUpdate(connection,sqlstr0)
## 		            j <- j+1
## 		        }

## 		        rowCount <- rowCount + 1
## 			}

## 			else if(is.matrix(objectVec[[i]]) || is.data.frame(objectVec[[i]]))
## 			{
## 				object <- objectVec[[i]]

## 				for (i in 1:ncol(object))
## 				for (j in (rowCount+1):(rowCount+nrow(object)))
## 				{
				 	
## 			 	 	sqlSendUpdate(connection,paste0(" INSERT INTO ",getOption("ResultMatrixTableFL"),
## 			 	 		                       " SELECT ",max_matrix_id_value,",",
## 			 	 		                                  j,",",
## 			 	 		                                  i,",",
## 			 	 		                                  object[(j-rowCount),i]))
			 	 
## 			 	}

## 			 	rowCount <- rowCount + nrow(object)
## 			}
## 		}


## 		max_matrix_id_value <<- max_matrix_id_value + 1

## 		return(FLMatrix( 
## 			       connection = connection, 
## 			       database = getOption("ResultDatabaseFL"), 
## 			       table_name = getOption("ResultMatrixTableFL"), 
## 				   matrix_id_value = max_matrix_id_value-1,
## 				   matrix_id_colname = "MATRIX_ID", 
## 				   row_id_colname = "rowIdColumn", 
## 				   col_id_colname = "colIdColumn", 
## 				   cell_val_colname = "valueColumn",
## 				   nrow = rowCount, 
## 				   ncol = ncol, 
## 				   dimnames = list(c(),c())))
## 	}
## }

## rbind.matrix <- function(object,...)
## {
## 	objectVec<-list(object,...)
## 	connection <- 0

## 	ncol <-0

## 	for(j in 1:length(objectVec))
## 	{
## 		if(is.FLMatrix(objectVec[[j]]) || is.matrix(objectVec[[j]]) || is.data.frame(objectVec[[j]]))
## 		{
## 			if(ncol == 0)
## 			{
## 				ncol <- ncol(objectVec[[j]])
## 			}
## 			else if(ncol != ncol(objectVec[[j]]))
## 			stop(" number of columns of matrix arguments must match ")
## 		}

## 		if(is.FLMatrix(objectVec[[j]]))
## 		{
## 			connection <- objectVec[[j]]@connection
## 		}
## 		if(is.FLVector(objectVec[[j]]))
## 		{
## 			connection <- objectVec[[j]]@connection
## 		}
## 		else if(is.FLSparseMatrix(objectVec[[j]]) || class(objectVec[[j]])=="dgCMatrix")
## 		{
## 			stop("input parameter cannot be sparse")
## 		}
## 	}

## 	if(connection==0)
## 	{
## 		return(base::rbind(object,...))
## 	}


## 	flag1Check(connection)

## 	rowCount <- 0
## 	ncol <- ncol(object)

## 	if (length(objectVec) > 1)
## 	{
## 	    for (i in 1:ncol(object))
## 		for (j in (rowCount+1):(rowCount+nrow(object)))
## 		{
		 	
## 	 	 	sqlSendUpdate(connection,paste0(" INSERT INTO ",getOption("ResultMatrixTableFL"),
## 	 	 		                       " SELECT ",max_matrix_id_value,",",
## 	 	 		                                  j,",",
## 	 	 		                                  i,",",
## 	 	 		                                  object[(j-rowCount),i]))
	 	 
## 	 	}

## 	    rowCount <- rowCount + nrow(object)

## 	    for (i in 2:length(objectVec))
## 		{
## 			if (class(objectVec[[i]]) == "FLMatrix")
## 			{
## 				object <- objectVec[[i]]

## 				sqlstr0<-paste0("INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 					            " SELECT ",max_matrix_id_value,",
## 							              a.",getVariables(object)$rowIdColumn,"+",rowCount,",
## 							              a.",getVariables(object)$colIdColumn,",
## 							              a.",getVariables(object)$valueColumn," 
## 							    FROM   ",remoteTable(object)," a 
## 							    WHERE  a.",object@matrix_id_colname," = ",object@matrix_id_value)

## 	            sqlSendUpdate(connection,sqlstr0)

##                 rowCount <- rowCount + nrow(object)
## 			}

## 			if (class(objectVec[[i]]) == "FLVector")
## 			{
## 				object <- objectVec[[i]]

## 				if(object@isDeep)
## 				{
## 					sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 						            " SELECT ",max_matrix_id_value,",
## 								             ",rowCount+1,",
## 								             a.",object@var_id_name,",
## 								             a.",object@col_name," 
## 								      FROM   ",remoteTable(object)," a 
## 								      WHERE  a.",object@obs_id_colname," = ",object@vector_id_value," AND 
## 								             a.",object@var_id_name,"<",ncol+1)
	
## 		            sqlSendUpdate(connection,sqlstr0)

## 		            if(length(object) < ncol)
## 		            {
## 		            	for(k in 1:(ncol%/%length(object)))
## 		            	{
## 		            		sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 								            " SELECT ",max_matrix_id_value,",
## 										             ",rowCount+1,",
## 										             (a.",object@var_id_name,"+(",k,"*",length(object),")),
## 										             a.",object@col_name," 
## 										      FROM ",remoteTable(object)," a 
## 										      WHERE a.",object@obs_id_colname," = ",object@vector_id_value," AND 
## 										            (a.",object@var_id_name,"+(",k,"*",length(object),"))<",ncol+1)
	
## 		                    sqlSendUpdate(connection,sqlstr0)
## 		            	}
## 		            }
## 		        }
## 		        else if(!object@isDeep)
## 		        {
## 					sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 						            " SELECT ",max_matrix_id_value,",
## 								             ",rowCount+1,",
## 								             a.",object@obs_id_colname,",
## 								             a.",object@col_name," 
## 								      FROM   ",remoteTable(object)," a 
## 								      WHERE a.",object@obs_id_colname,"<",ncol+1)
	
## 		            sqlSendUpdate(connection,sqlstr0)

## 		            if(length(object) < ncol)
## 		            {
## 		            	for(k in 1:(ncol%/%length(object)))
## 		            	{
## 		            		sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 								            " SELECT ",max_matrix_id_value,",
## 										             ",rowCount+1,",
## 										             (a.",object@obs_id_colname,"+(",k,"*",length(object),")),
## 										              a.",object@col_name," 
## 										      FROM ",remoteTable(object)," a 
## 										      WHERE (a.",object@obs_id_colname,"+(",k,"*",length(object),"))<",ncol+1)
	
## 		                    sqlSendUpdate(connection,sqlstr0)
## 		            	}
## 		            }
## 		        }

## 		        rowCount <- rowCount + 1
## 			}

## 			else if (is.vector(objectVec[[i]]))
## 			{
## 				object <- objectVec[[i]]

## 				for(k in 1:ncol)
## 		        {
## 		            j <- k
## 		            if(j > length(object))
## 		            j <- 1

## 		            sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 								    " SELECT ",max_matrix_id_value,",
## 										     ",rowCount+1,","
## 										      ,k,","
## 										      ,object[j])
	
## 		            sqlSendUpdate(connection,sqlstr0)
## 		            j <- j+1
## 		        }

## 		        rowCount <- rowCount + 1
## 			}

## 			else if(is.matrix(objectVec[[i]]) || is.data.frame(objectVec[[i]]))
## 			{
## 				object <- objectVec[[i]]

## 				for (i in 1:ncol(object))
## 				for (j in (rowCount+1):(rowCount+nrow(object)))
## 				{
				 	
## 			 	 	sqlSendUpdate(connection,paste0(" INSERT INTO ",getOption("ResultMatrixTableFL"),
## 			 	 		                       " SELECT ",max_matrix_id_value,",",
## 			 	 		                                  j,",",
## 			 	 		                                  i,",",
## 			 	 		                                  object[(j-rowCount),i]))
			 	 
## 			 	}

## 			 	rowCount <- rowCount + nrow(object)
## 			}
## 		}


## 		max_matrix_id_value <<- max_matrix_id_value + 1

## 		return(FLMatrix( 
## 			       connection = connection, 
## 			       database = getOption("ResultDatabaseFL"), 
## 			       table_name = getOption("ResultMatrixTableFL"), 
## 				   matrix_id_value = max_matrix_id_value-1,
## 				   matrix_id_colname = "MATRIX_ID", 
## 				   row_id_colname = "rowIdColumn", 
## 				   col_id_colname = "colIdColumn", 
## 				   cell_val_colname = "valueColumn",
## 				   nrow = rowCount, 
## 				   ncol = ncol, 
## 				   dimnames = list(c(),c())))
## 	}
## }

## rbind.numeric <- function(object,...)
## {
## 	objectVec<-list(object,...)
## 	connection <- 0

## 	ncol <-0

## 	for(j in 1:length(objectVec))
## 	{
## 		if(is.FLMatrix(objectVec[[j]]) || is.matrix(objectVec[[j]]) || is.data.frame(objectVec[[j]]))
## 		{
## 			if(ncol ==0)
## 			{
## 				ncol <- ncol(objectVec[[j]])
## 			}
## 			else if(ncol != ncol(objectVec[[j]]))
## 			stop(" number of columns of matrix arguments must match ")
## 		}

## 		if(is.FLMatrix(objectVec[[j]]))
## 		{
## 			connection <- objectVec[[j]]@connection
## 		}
## 		if(is.FLVector(objectVec[[j]]))
## 		{
## 			connection <- objectVec[[j]]@connection
## 		}
## 		else if(is.FLSparseMatrix(objectVec[[j]]) || class(objectVec[[j]])=="dgCMatrix")
## 		{
## 			stop("input parameter cannot be sparse")
## 		}
## 	}

## 	if(connection==0)
## 	{
## 		return(base::rbind(object,...))
## 	}

## 	if(ncol == 0)
## 	{
## 		for(j in 1:length(objectVec))
## 		{
## 			if(is.FLVector(objectVec[[j]]) || is.numeric(objectVec[[j]]))
## 			{
## 				if(ncol < length(objectVec[[j]]))
## 				{
## 					ncol <- length(objectVec[[j]])
## 				}
## 			}
## 		}
## 	}

## 	flag1Check(connection)

## 	rowCount <- 0

## 	if (length(objectVec) > 1)
## 	{
## 	    for(k in 1:ncol)
##         {
##             j <- k
##             if(j > length(object))
##             j <- 1

##             sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 						    " SELECT ",max_matrix_id_value,",
## 								     ",rowCount+1,","
## 								      ,k,","
## 								      ,object[j])

##             sqlSendUpdate(connection,sqlstr0)
##             j <- j+1
##         }

## 	    rowCount <- rowCount + 1

## 	    for (i in 2:length(objectVec))
## 		{
## 			if (class(objectVec[[i]]) == "FLMatrix")
## 			{
## 				object <- objectVec[[i]]

## 				if(ncol(object) != ncol)
## 				{
## 					stop(" number of columns of matrix arguments must match ")
## 				}
## 				sqlstr0<-paste0("INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 					            " SELECT ",max_matrix_id_value,",
## 							              a.",getVariables(object)$rowIdColumn,"+",rowCount,",
## 							              a.",getVariables(object)$colIdColumn,",
## 							              a.",getVariables(object)$valueColumn," 
## 							    FROM   ",remoteTable(object)," a 
## 							    WHERE  a.",object@matrix_id_colname," = ",object@matrix_id_value)

## 	            sqlSendUpdate(connection,sqlstr0)

##                 rowCount <- rowCount + nrow(object)
## 			}

## 			if (class(objectVec[[i]]) == "FLVector")
## 			{
## 				object <- objectVec[[i]]

## 				if(object@isDeep)
## 				{
## 					sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 						            " SELECT ",max_matrix_id_value,",
## 								             ",rowCount+1,",
## 								             a.",object@var_id_name,",
## 								             a.",object@col_name," 
## 								      FROM   ",remoteTable(object)," a 
## 								      WHERE  a.",object@obs_id_colname," = ",object@vector_id_value," AND 
## 								             a.",object@var_id_name,"<",ncol+1)
	
## 		            sqlSendUpdate(connection,sqlstr0)

## 		            if(length(object) < ncol)
## 		            {
## 		            	for(k in 1:(ncol%/%length(object)))
## 		            	{
## 		            		sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 								            " SELECT ",max_matrix_id_value,",
## 										             ",rowCount+1,",
## 										             (a.",object@var_id_name,"+(",k,"*",length(object),")),
## 										             a.",object@col_name," 
## 										      FROM ",remoteTable(object)," a 
## 										      WHERE a.",object@obs_id_colname," = ",object@vector_id_value," AND 
## 										            (a.",object@var_id_name,"+(",k,"*",length(object),"))<",ncol+1)
	
## 		                    sqlSendUpdate(connection,sqlstr0)
## 		            	}
## 		            }
## 		        }
## 		        else if(!object@isDeep)
## 		        {
## 					sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 						            " SELECT ",max_matrix_id_value,",
## 								             ",rowCount+1,",
## 								             a.",object@obs_id_colname,",
## 								             a.",object@col_name," 
## 								      FROM   ",remoteTable(object)," a 
## 								      WHERE a.",object@obs_id_colname,"<",ncol+1)
	
## 		            sqlSendUpdate(connection,sqlstr0)

## 		            if(length(object) < ncol)
## 		            {
## 		            	for(k in 1:(ncol%/%length(object)))
## 		            	{
## 		            		sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 								            " SELECT ",max_matrix_id_value,",
## 										             ",rowCount+1,",
## 										             (a.",object@obs_id_colname,"+(",k,"*",length(object),")),
## 										              a.",object@col_name," 
## 										      FROM ",remoteTable(object)," a 
## 										      WHERE (a.",object@obs_id_colname,"+(",k,"*",length(object),"))<",ncol+1)
	
## 		                    sqlSendUpdate(connection,sqlstr0)
## 		            	}
## 		            }
## 		        }

## 		        rowCount <- rowCount + 1
## 			}

## 			else if (is.vector(objectVec[[i]]))
## 			{
## 				object <- objectVec[[i]]

## 				for(k in 1:ncol)
## 		        {
## 		            j <- k
## 		            if(j > length(object))
## 		            j <- 1

## 		            sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 								    " SELECT ",max_matrix_id_value,",
## 										     ",rowCount+1,","
## 										      ,k,","
## 										      ,object[j])
	
## 		            sqlSendUpdate(connection,sqlstr0)
## 		            j <- j+1
## 		        }

## 		        rowCount <- rowCount + 1
## 			}

## 			else if(is.matrix(objectVec[[i]]) || is.data.frame(objectVec[[i]]))
## 			{
## 				object <- objectVec[[i]]

## 				for (i in 1:ncol(object))
## 				for (j in (rowCount+1):(rowCount+nrow(object)))
## 				{
				 	
## 			 	 	sqlSendUpdate(connection,paste0(" INSERT INTO ",getOption("ResultMatrixTableFL"),
## 			 	 		                       " SELECT ",max_matrix_id_value,",",
## 			 	 		                                  j,",",
## 			 	 		                                  i,",",
## 			 	 		                                  object[(j-rowCount),i]))
			 	 
## 			 	}

## 			 	rowCount <- rowCount + nrow(object)
## 			}
## 		}


## 		max_matrix_id_value <<- max_matrix_id_value + 1

## 		return(FLMatrix( 
## 			       connection = connection, 
## 			       database = getOption("ResultDatabaseFL"), 
## 			       table_name = getOption("ResultMatrixTableFL"), 
## 				   matrix_id_value = max_matrix_id_value-1,
## 				   matrix_id_colname = "MATRIX_ID", 
## 				   row_id_colname = "rowIdColumn", 
## 				   col_id_colname = "colIdColumn", 
## 				   cell_val_colname = "valueColumn",
## 				   nrow = rowCount, 
## 				   ncol = ncol, 
## 				   dimnames = list(c(),c())))
## 	}
## }

## rbind.data.frame <- function(object,...)
## {
## 	objectVec<-list(object,...)
## 	connection <- 0

## 	ncol <-0

## 	for(j in 1:length(objectVec))
## 	{
## 		if(is.FLMatrix(objectVec[[j]]) || is.matrix(objectVec[[j]]) || is.data.frame(objectVec[[j]]))
## 		{
## 			if(ncol == 0)
## 			{
## 				ncol <- ncol(objectVec[[j]])
## 			}
## 			else if(ncol != ncol(objectVec[[j]]))
## 			stop(" number of columns of matrix arguments must match ")
## 		}

## 		if(is.FLMatrix(objectVec[[j]]))
## 		{
## 			connection <- objectVec[[j]]@connection
## 		}
## 		if(is.FLVector(objectVec[[j]]))
## 		{
## 			connection <- objectVec[[j]]@connection
## 		}
## 		else if(is.FLSparseMatrix(objectVec[[j]]) || class(objectVec[[j]])=="dgCMatrix")
## 		{
## 			stop("input parameter cannot be sparse")
## 		}
## 	}

## 	if(connection==0)
## 	{
## 		return(base::rbind(object,...))
## 	}


## 	flag1Check(connection)

## 	rowCount <- 0
## 	ncol <- ncol(object)

## 	if (length(objectVec) > 1)
## 	{
## 	    for (i in 1:ncol(object))
## 		for (j in (rowCount+1):(rowCount+nrow(object)))
## 		{
		 	
## 	 	 	sqlSendUpdate(connection,paste0(" INSERT INTO ",getOption("ResultMatrixTableFL"),
## 	 	 		                       " SELECT ",max_matrix_id_value,",",
## 	 	 		                                  j,",",
## 	 	 		                                  i,",",
## 	 	 		                                  object[(j-rowCount),i]))
	 	 
## 	 	}

## 	    rowCount <- rowCount + nrow(object)

## 	    for (i in 2:length(objectVec))
## 		{
## 			if (class(objectVec[[i]]) == "FLMatrix")
## 			{
## 				object <- objectVec[[i]]

## 				sqlstr0<-paste0("INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 					            " SELECT ",max_matrix_id_value,",
## 							              a.",getVariables(object)$rowIdColumn,"+",rowCount,",
## 							              a.",getVariables(object)$colIdColumn,",
## 							              a.",getVariables(object)$valueColumn," 
## 							    FROM   ",remoteTable(object)," a 
## 							    WHERE  a.",object@matrix_id_colname," = ",object@matrix_id_value)

## 	            sqlSendUpdate(connection,sqlstr0)

##                 rowCount <- rowCount + nrow(object)
## 			}

## 			if (class(objectVec[[i]]) == "FLVector")
## 			{
## 				object <- objectVec[[i]]

## 				if(object@isDeep)
## 				{
## 					sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 						            " SELECT ",max_matrix_id_value,",
## 								             ",rowCount+1,",
## 								             a.",object@var_id_name,",
## 								             a.",object@col_name," 
## 								      FROM   ",remoteTable(object)," a 
## 								      WHERE  a.",object@obs_id_colname," = ",object@vector_id_value," AND 
## 								             a.",object@var_id_name,"<",ncol+1)
	
## 		            sqlSendUpdate(connection,sqlstr0)

## 		            if(length(object) < ncol)
## 		            {
## 		            	for(k in 1:(ncol%/%length(object)))
## 		            	{
## 		            		sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 								            " SELECT ",max_matrix_id_value,",
## 										             ",rowCount+1,",
## 										             (a.",object@var_id_name,"+(",k,"*",length(object),")),
## 										             a.",object@col_name," 
## 										      FROM ",remoteTable(object)," a 
## 										      WHERE a.",object@obs_id_colname," = ",object@vector_id_value," AND 
## 										            (a.",object@var_id_name,"+(",k,"*",length(object),"))<",ncol+1)
	
## 		                    sqlSendUpdate(connection,sqlstr0)
## 		            	}
## 		            }
## 		        }
## 		        else if(!object@isDeep)
## 		        {
## 					sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 						            " SELECT ",max_matrix_id_value,",
## 								             ",rowCount+1,",
## 								             a.",object@obs_id_colname,",
## 								             a.",object@col_name," 
## 								      FROM   ",remoteTable(object)," a 
## 								      WHERE a.",object@obs_id_colname,"<",ncol+1)
	
## 		            sqlSendUpdate(connection,sqlstr0)

## 		            if(length(object) < ncol)
## 		            {
## 		            	for(k in 1:(ncol%/%length(object)))
## 		            	{
## 		            		sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 								            " SELECT ",max_matrix_id_value,",
## 										             ",rowCount+1,",
## 										             (a.",object@obs_id_colname,"+(",k,"*",length(object),")),
## 										              a.",object@col_name," 
## 										      FROM ",remoteTable(object)," a 
## 										      WHERE (a.",object@obs_id_colname,"+(",k,"*",length(object),"))<",ncol+1)
	
## 		                    sqlSendUpdate(connection,sqlstr0)
## 		            	}
## 		            }
## 		        }

## 		        rowCount <- rowCount + 1
## 			}

## 			else if (is.vector(objectVec[[i]]))
## 			{
## 				object <- objectVec[[i]]

## 				for(k in 1:ncol)
## 		        {
## 		            j <- k
## 		            if(j > length(object))
## 		            j <- 1

## 		            sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(tableName=getOption("ResultMatrixTableFL")),
## 								    " SELECT ",max_matrix_id_value,",
## 										     ",rowCount+1,","
## 										      ,k,","
## 										      ,object[j])
	
## 		            sqlSendUpdate(connection,sqlstr0)
## 		            j <- j+1
## 		        }

## 		        rowCount <- rowCount + 1
## 			}

## 			else if(is.matrix(objectVec[[i]]) || is.data.frame(objectVec[[i]]))
## 			{
## 				object <- objectVec[[i]]

## 				for (i in 1:ncol(object))
## 				for (j in (rowCount+1):(rowCount+nrow(object)))
## 				{
				 	
## 			 	 	sqlSendUpdate(connection,paste0(" INSERT INTO ",getOption("ResultMatrixTableFL"),
## 			 	 		                       " SELECT ",max_matrix_id_value,",",
## 			 	 		                                  j,",",
## 			 	 		                                  i,",",
## 			 	 		                                  object[(j-rowCount),i]))
			 	 
## 			 	}

## 			 	rowCount <- rowCount + nrow(object)
## 			}
## 		}


## 		max_matrix_id_value <<- max_matrix_id_value + 1

## 		return(FLMatrix( 
## 			       connection = connection, 
## 			       database = getOption("ResultDatabaseFL"), 
## 			       table_name = getOption("ResultMatrixTableFL"), 
## 				   matrix_id_value = max_matrix_id_value-1,
## 				   matrix_id_colname = "MATRIX_ID", 
## 				   row_id_colname = "rowIdColumn", 
## 				   col_id_colname = "colIdColumn", 
## 				   cell_val_colname = "valueColumn",
## 				   nrow = rowCount, 
## 				   ncol = ncol, 
## 				   dimnames = list(c(),c())))
## 	}
## }
