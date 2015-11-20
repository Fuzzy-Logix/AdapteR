#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
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
#' @param x... can be a sequence of vector, FLVector, matrix, FLMatrix or data frames
#' @section Constraints:
#' Input matrices, FLMatrices and data frames should have same number of columns.
#' @return \code{rbind} returns a FLMatrix object which is the row-wise combination of input arguments.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLMatrix <- rbind(flmatrix,1:5,flmatrix)
#' @export

rbind <- function (x, ...){
  UseMethod("rbind", x)
}

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

rbind.FLMatrix<-function(object,...)
{
	objectVec<-list(object,...)
	connection<-object@odbc_connection

	ncol <-0

	for(j in 1:length(objectVec))
	{
		if(is.FLMatrix(objectVec[[j]]) || is.matrix(objectVec[[j]]) || is.data.frame(objectVec[[j]]))
		{
			if(ncol == 0)
			{
				ncol <- ncol(objectVec[[j]])
			}
			else if(ncol != ncol(objectVec[[j]]))
			stop(" number of columns of matrix arguments must match ")
		}
		else if(is.FLSparseMatrix(objectVec[[j]]) || class(objectVec[[j]])=="dgCMatrix")
		{
			stop("input parameter cannot be sparse")
		}
	}

	flag1Check(connection)

	rowCount <- 0
	ncol <- object@ncol

	if(length(objectVec) == 1) 
	{
		return(object)
	}

	else if (length(objectVec) > 1)
	{
	    sqlstr0<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,
		            " SELECT ",max_matrix_id_value,",
				              a.",object@row_id_colname,",
				              a.",object@col_id_colname,",
				              a.",object@cell_val_colname," 
				    FROM   ",remoteTable(object)," a 
				    WHERE  a.",object@matrix_id_colname," = ",object@matrix_id_value)

	    sqlSendUpdate(connection,sqlstr0)

	    rowCount <- rowCount + object@nrow

	    for (i in 2:length(objectVec))
		{
			if (class(objectVec[[i]]) == "FLMatrix")
			{
				object <- objectVec[[i]]

				sqlstr0<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,
					            " SELECT ",max_matrix_id_value,",
							              a.",object@row_id_colname,"+",rowCount,",
							              a.",object@col_id_colname,",
							              a.",object@cell_val_colname," 
							    FROM   ",remoteTable(object)," a 
							    WHERE  a.",object@matrix_id_colname," = ",object@matrix_id_value)

	            sqlSendUpdate(connection,sqlstr0)

                rowCount <- rowCount + object@nrow
			}

			if (class(objectVec[[i]]) == "FLVector")
			{
				object <- objectVec[[i]]

				if(object@table@isDeep)
				{
					sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             ",rowCount+1,",
								             a.",object@table@var_id_name,",
								             a.",object@col_name," 
								      FROM   ",remoteTable(object@table)," a 
								      WHERE  a.",object@table@primary_key," = ",object@vector_id_value," AND 
								             a.",object@table@var_id_name,"<",ncol+1)
	
		            sqlSendUpdate(connection,sqlstr0)

		            if(object@size < ncol)
		            {
		            	for(k in 1:(ncol%/%object@size))
		            	{
		            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								            " SELECT ",max_matrix_id_value,",
										             ",rowCount+1,",
										             (a.",object@table@var_id_name,"+(",k,"*",object@size,")),
										             a.",object@col_name," 
										      FROM ",remoteTable(object@table)," a 
										      WHERE a.",object@table@primary_key," = ",object@vector_id_value," AND 
										            (a.",object@table@var_id_name,"+(",k,"*",object@size,"))<",ncol+1)
	
		                    sqlSendUpdate(connection,sqlstr0)
		            	}
		            }
		        }
		        else if(!object@table@isDeep)
		        {
					sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             ",rowCount+1,",
								             a.",object@table@primary_key,",
								             a.",object@col_name," 
								      FROM   ",remoteTable(object@table)," a 
								      WHERE a.",object@table@primary_key,"<",ncol+1)
	
		            sqlSendUpdate(connection,sqlstr0)

		            if(object@size < ncol)
		            {
		            	for(k in 1:(ncol%/%object@size))
		            	{
		            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								            " SELECT ",max_matrix_id_value,",
										             ",rowCount+1,",
										             (a.",object@table@primary_key,"+(",k,"*",object@size,")),
										              a.",object@col_name," 
										      FROM ",remoteTable(object@table)," a 
										      WHERE (a.",object@table@primary_key,"+(",k,"*",object@size,"))<",ncol+1)
	
		                    sqlSendUpdate(connection,sqlstr0)
		            	}
		            }
		        }

		        rowCount <- rowCount + 1
			}

			else if (is.vector(objectVec[[i]]))
			{
				object <- objectVec[[i]]

				j <- 1
				for(k in 1:ncol)
		        {
		            if(j > length(object))
		            j <- 1

		            sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								    " SELECT ",max_matrix_id_value,",
										     ",rowCount+1,","
										      ,k,","
										      ,object[j])
	
		            sqlSendUpdate(connection,sqlstr0)
		            j <- j+1
		        }

		        rowCount <- rowCount + 1
			}

			else if(is.matrix(objectVec[[i]]) || is.data.frame(objectVec[[i]]))
			{
				object <- objectVec[[i]]

				for (i in 1:ncol(object))
				for (j in (rowCount+1):(rowCount+nrow(object)))
				{
				 	
			 	 	sqlSendUpdate(connection,paste0(" INSERT INTO ",result_matrix_table,
			 	 		                       " SELECT ",max_matrix_id_value,",",
			 	 		                                  j,",",
			 	 		                                  i,",",
			 	 		                                  object[(j-rowCount),i]))
			 	 
			 	}

			 	rowCount <- rowCount + nrow(object)
			}
		}


		max_matrix_id_value <<- max_matrix_id_value + 1

		return(new("FLMatrix", 
			       odbc_connection = connection, 
			       db_name = result_db_name, 
			       matrix_table = result_matrix_table, 
				   matrix_id_value = max_matrix_id_value-1,
				   matrix_id_colname = "MATRIX_ID", 
				   row_id_colname = "ROW_ID", 
				   col_id_colname = "COL_ID", 
				   cell_val_colname = "CELL_VAL",
				   nrow = rowCount, 
				   ncol = ncol, 
				   dimnames = list(c(),c())))
	}
}

rbind.FLVector <- function(object,...)
{
	objectVec<-list(object,...)
	connection<-object@table@odbc_connection

	ncol <-0

	for(j in 1:length(objectVec))
	{
		if(is.FLMatrix(objectVec[[j]]) || is.matrix(objectVec[[j]]) || is.data.frame(objectVec[[j]]))
		{
			if(ncol ==0)
			{
				ncol <- ncol(objectVec[[j]])
			}
			else if(ncol != ncol(objectVec[[j]]))
			stop(" number of columns of matrix arguments must match ")
		}
		else if(is.FLSparseMatrix(objectVec[[j]]) || class(objectVec[[j]])=="dgCMatrix")
		{
			stop("input parameter cannot be sparse")
		}
	}

	if(ncol == 0)
	{
		for(j in 1:length(objectVec))
		{
			if(is.FLVector(objectVec[[j]]) || is.numeric(objectVec[[j]]))
			{
				if(ncol < length(objectVec[[j]]))
				{
					ncol <- length(objectVec[[j]])
				}
			}
		}
	}

	flag1Check(connection)

	rowCount <- 0

	if(length(objectVec) == 1) 
	{
		if(object@table@isDeep)
		{
			sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
				            " SELECT ",max_matrix_id_value,",
						             ",rowCount+1,",
						             a.",object@table@var_id_name,",
						             a.",object@col_name," 
						      FROM   ",remoteTable(object@table)," a 
						      WHERE  a.",object@table@primary_key," = ",object@vector_id_value," AND 
						             a.",object@table@var_id_name,"<",ncol+1)

            sqlSendUpdate(connection,sqlstr0)

            if(object@size < ncol)
            {
            	for(k in 1:(ncol%/%object@size))
            	{
            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             ",rowCount+1,",
								             (a.",object@table@var_id_name,"+(",k,"*",object@size,")),
								             a.",object@col_name," 
								      FROM ",remoteTable(object@table)," a 
								      WHERE a.",object@table@primary_key," = ",object@vector_id_value," AND 
								            (a.",object@table@var_id_name,"+(",k,"*",object@size,"))<",ncol+1)

                    sqlSendUpdate(connection,sqlstr0)
            	}
            }
        }
        else if(!object@table@isDeep)
        {
			sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
				            " SELECT ",max_matrix_id_value,",
						             ",rowCount+1,",
						             a.",object@table@primary_key,",
						             a.",object@col_name," 
						      FROM   ",remoteTable(object@table)," a 
						      WHERE a.",object@table@primary_key,"<",ncol+1)

            sqlSendUpdate(connection,sqlstr0)

            if(object@size < ncol)
            {
            	for(k in 1:(ncol%/%object@size))
            	{
            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             ",rowCount+1,",
								             (a.",object@table@primary_key,"+(",k,"*",object@size,")),
								              a.",object@col_name," 
								      FROM ",remoteTable(object@table)," a 
								      WHERE (a.",object@table@primary_key,"+(",k,"*",object@size,"))<",ncol+1)

                    sqlSendUpdate(connection,sqlstr0)
            	}
            }
        }

        max_matrix_id_value <<- max_matrix_id_value + 1

		return(new("FLMatrix", 
			       odbc_connection = connection, 
			       db_name = result_db_name, 
			       matrix_table = result_matrix_table, 
				   matrix_id_value = max_matrix_id_value-1,
				   matrix_id_colname = "MATRIX_ID", 
				   row_id_colname = "ROW_ID", 
				   col_id_colname = "COL_ID", 
				   cell_val_colname = "CELL_VAL",
				   nrow = 1, 
				   ncol = ncol, 
				   dimnames = list(c(),c())))
	}

	else if (length(objectVec) > 1)
	{
	    if(object@table@isDeep)
		{
			sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
				            " SELECT ",max_matrix_id_value,",
						             ",rowCount+1,",
						             a.",object@table@var_id_name,",
						             a.",object@col_name," 
						      FROM   ",remoteTable(object@table)," a 
						      WHERE  a.",object@table@primary_key," = ",object@vector_id_value," AND 
						             a.",object@table@var_id_name,"<",ncol+1)

            sqlSendUpdate(connection,sqlstr0)

            if(object@size < ncol)
            {
            	for(k in 1:(ncol%/%object@size))
            	{
            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             ",rowCount+1,",
								             (a.",object@table@var_id_name,"+(",k,"*",object@size,")),
								             a.",object@col_name," 
								      FROM ",remoteTable(object@table)," a 
								      WHERE a.",object@table@primary_key," = ",object@vector_id_value," AND 
								            (a.",object@table@var_id_name,"+(",k,"*",object@size,"))<",ncol+1)

                    sqlSendUpdate(connection,sqlstr0)
            	}
            }
        }
        else if(!object@table@isDeep)
        {
			sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
				            " SELECT ",max_matrix_id_value,",
						             ",rowCount+1,",
						             a.",object@table@primary_key,",
						             a.",object@col_name," 
						      FROM   ",remoteTable(object@table)," a 
						      WHERE a.",object@table@primary_key,"<",ncol+1)

            sqlSendUpdate(connection,sqlstr0)

            if(object@size < ncol)
            {
            	for(k in 1:(ncol%/%object@size))
            	{
            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             ",rowCount+1,",
								             (a.",object@table@primary_key,"+(",k,"*",object@size,")),
								              a.",object@col_name," 
								      FROM ",remoteTable(object@table)," a 
								      WHERE (a.",object@table@primary_key,"+(",k,"*",object@size,"))<",ncol+1)

                    sqlSendUpdate(connection,sqlstr0)
            	}
            }
        }

	    rowCount <- rowCount + 1

	    for (i in 2:length(objectVec))
		{
			if (class(objectVec[[i]]) == "FLMatrix")
			{
				object <- objectVec[[i]]

				if(object@ncol != ncol)
				{
					stop(" number of columns of matrix arguments must match ")
				}
				sqlstr0<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,
					            " SELECT ",max_matrix_id_value,",
							              a.",object@row_id_colname,"+",rowCount,",
							              a.",object@col_id_colname,",
							              a.",object@cell_val_colname," 
							    FROM   ",remoteTable(object)," a 
							    WHERE  a.",object@matrix_id_colname," = ",object@matrix_id_value)

	            sqlSendUpdate(connection,sqlstr0)

                rowCount <- rowCount + object@nrow
			}

			if (class(objectVec[[i]]) == "FLVector")
			{
				object <- objectVec[[i]]

				if(object@table@isDeep)
				{
					sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             ",rowCount+1,",
								             a.",object@table@var_id_name,",
								             a.",object@col_name," 
								      FROM   ",remoteTable(object@table)," a 
								      WHERE  a.",object@table@primary_key," = ",object@vector_id_value," AND 
								             a.",object@table@var_id_name,"<",ncol+1)
	
		            sqlSendUpdate(connection,sqlstr0)

		            if(object@size < ncol)
		            {
		            	for(k in 1:(ncol%/%object@size))
		            	{
		            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								            " SELECT ",max_matrix_id_value,",
										             ",rowCount+1,",
										             (a.",object@table@var_id_name,"+(",k,"*",object@size,")),
										             a.",object@col_name," 
										      FROM ",remoteTable(object@table)," a 
										      WHERE a.",object@table@primary_key," = ",object@vector_id_value," AND 
										            (a.",object@table@var_id_name,"+(",k,"*",object@size,"))<",ncol+1)
	
		                    sqlSendUpdate(connection,sqlstr0)
		            	}
		            }
		        }
		        else if(!object@table@isDeep)
		        {
					sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             ",rowCount+1,",
								             a.",object@table@primary_key,",
								             a.",object@col_name," 
								      FROM   ",remoteTable(object@table)," a 
								      WHERE a.",object@table@primary_key,"<",ncol+1)
	
		            sqlSendUpdate(connection,sqlstr0)

		            if(object@size < ncol)
		            {
		            	for(k in 1:(ncol%/%object@size))
		            	{
		            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								            " SELECT ",max_matrix_id_value,",
										             ",rowCount+1,",
										             (a.",object@table@primary_key,"+(",k,"*",object@size,")),
										              a.",object@col_name," 
										      FROM ",remoteTable(object@table)," a 
										      WHERE (a.",object@table@primary_key,"+(",k,"*",object@size,"))<",ncol+1)
	
		                    sqlSendUpdate(connection,sqlstr0)
		            	}
		            }
		        }

		        rowCount <- rowCount + 1
			}

			else if (is.vector(objectVec[[i]]))
			{
				object <- objectVec[[i]]

				for(k in 1:ncol)
		        {
		            j <- k
		            if(j > length(object))
		            j <- 1

		            sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								    " SELECT ",max_matrix_id_value,",
										     ",rowCount+1,","
										      ,k,","
										      ,object[j])
	
		            sqlSendUpdate(connection,sqlstr0)
		            j <- j+1
		        }

		        rowCount <- rowCount + 1
			}

			else if(is.matrix(objectVec[[i]]) || is.data.frame(objectVec[[i]]))
			{
				object <- objectVec[[i]]

				for (i in 1:ncol(object))
				for (j in (rowCount+1):(rowCount+nrow(object)))
				{
				 	
			 	 	sqlSendUpdate(connection,paste0(" INSERT INTO ",result_matrix_table,
			 	 		                       " SELECT ",max_matrix_id_value,",",
			 	 		                                  j,",",
			 	 		                                  i,",",
			 	 		                                  object[(j-rowCount),i]))
			 	 
			 	}

			 	rowCount <- rowCount + nrow(object)
			}
		}


		max_matrix_id_value <<- max_matrix_id_value + 1

		return(new("FLMatrix", 
			       odbc_connection = connection, 
			       db_name = result_db_name, 
			       matrix_table = result_matrix_table, 
				   matrix_id_value = max_matrix_id_value-1,
				   matrix_id_colname = "MATRIX_ID", 
				   row_id_colname = "ROW_ID", 
				   col_id_colname = "COL_ID", 
				   cell_val_colname = "CELL_VAL",
				   nrow = rowCount, 
				   ncol = ncol, 
				   dimnames = list(c(),c())))
	}
}

rbind.matrix <- function(object,...)
{
	objectVec<-list(object,...)
	connection <- 0

	ncol <-0

	for(j in 1:length(objectVec))
	{
		if(is.FLMatrix(objectVec[[j]]) || is.matrix(objectVec[[j]]) || is.data.frame(objectVec[[j]]))
		{
			if(ncol == 0)
			{
				ncol <- ncol(objectVec[[j]])
			}
			else if(ncol != ncol(objectVec[[j]]))
			stop(" number of columns of matrix arguments must match ")
		}

		if(is.FLMatrix(objectVec[[j]]))
		{
			connection <- objectVec[[j]]@odbc_connection
		}
		if(is.FLVector(objectVec[[j]]))
		{
			connection <- objectVec[[j]]@table@odbc_connection
		}
		else if(is.FLSparseMatrix(objectVec[[j]]) || class(objectVec[[j]])=="dgCMatrix")
		{
			stop("input parameter cannot be sparse")
		}
	}

	if(connection==0)
	{
		return(base::rbind(object,...))
	}


	flag1Check(connection)

	rowCount <- 0
	ncol <- ncol(object)

	if (length(objectVec) > 1)
	{
	    for (i in 1:ncol(object))
		for (j in (rowCount+1):(rowCount+nrow(object)))
		{
		 	
	 	 	sqlSendUpdate(connection,paste0(" INSERT INTO ",result_matrix_table,
	 	 		                       " SELECT ",max_matrix_id_value,",",
	 	 		                                  j,",",
	 	 		                                  i,",",
	 	 		                                  object[(j-rowCount),i]))
	 	 
	 	}

	    rowCount <- rowCount + nrow(object)

	    for (i in 2:length(objectVec))
		{
			if (class(objectVec[[i]]) == "FLMatrix")
			{
				object <- objectVec[[i]]

				sqlstr0<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,
					            " SELECT ",max_matrix_id_value,",
							              a.",object@row_id_colname,"+",rowCount,",
							              a.",object@col_id_colname,",
							              a.",object@cell_val_colname," 
							    FROM   ",remoteTable(object)," a 
							    WHERE  a.",object@matrix_id_colname," = ",object@matrix_id_value)

	            sqlSendUpdate(connection,sqlstr0)

                rowCount <- rowCount + object@nrow
			}

			if (class(objectVec[[i]]) == "FLVector")
			{
				object <- objectVec[[i]]

				if(object@table@isDeep)
				{
					sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             ",rowCount+1,",
								             a.",object@table@var_id_name,",
								             a.",object@col_name," 
								      FROM   ",remoteTable(object@table)," a 
								      WHERE  a.",object@table@primary_key," = ",object@vector_id_value," AND 
								             a.",object@table@var_id_name,"<",ncol+1)
	
		            sqlSendUpdate(connection,sqlstr0)

		            if(object@size < ncol)
		            {
		            	for(k in 1:(ncol%/%object@size))
		            	{
		            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								            " SELECT ",max_matrix_id_value,",
										             ",rowCount+1,",
										             (a.",object@table@var_id_name,"+(",k,"*",object@size,")),
										             a.",object@col_name," 
										      FROM ",remoteTable(object@table)," a 
										      WHERE a.",object@table@primary_key," = ",object@vector_id_value," AND 
										            (a.",object@table@var_id_name,"+(",k,"*",object@size,"))<",ncol+1)
	
		                    sqlSendUpdate(connection,sqlstr0)
		            	}
		            }
		        }
		        else if(!object@table@isDeep)
		        {
					sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             ",rowCount+1,",
								             a.",object@table@primary_key,",
								             a.",object@col_name," 
								      FROM   ",remoteTable(object@table)," a 
								      WHERE a.",object@table@primary_key,"<",ncol+1)
	
		            sqlSendUpdate(connection,sqlstr0)

		            if(object@size < ncol)
		            {
		            	for(k in 1:(ncol%/%object@size))
		            	{
		            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								            " SELECT ",max_matrix_id_value,",
										             ",rowCount+1,",
										             (a.",object@table@primary_key,"+(",k,"*",object@size,")),
										              a.",object@col_name," 
										      FROM ",remoteTable(object@table)," a 
										      WHERE (a.",object@table@primary_key,"+(",k,"*",object@size,"))<",ncol+1)
	
		                    sqlSendUpdate(connection,sqlstr0)
		            	}
		            }
		        }

		        rowCount <- rowCount + 1
			}

			else if (is.vector(objectVec[[i]]))
			{
				object <- objectVec[[i]]

				for(k in 1:ncol)
		        {
		            j <- k
		            if(j > length(object))
		            j <- 1

		            sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								    " SELECT ",max_matrix_id_value,",
										     ",rowCount+1,","
										      ,k,","
										      ,object[j])
	
		            sqlSendUpdate(connection,sqlstr0)
		            j <- j+1
		        }

		        rowCount <- rowCount + 1
			}

			else if(is.matrix(objectVec[[i]]) || is.data.frame(objectVec[[i]]))
			{
				object <- objectVec[[i]]

				for (i in 1:ncol(object))
				for (j in (rowCount+1):(rowCount+nrow(object)))
				{
				 	
			 	 	sqlSendUpdate(connection,paste0(" INSERT INTO ",result_matrix_table,
			 	 		                       " SELECT ",max_matrix_id_value,",",
			 	 		                                  j,",",
			 	 		                                  i,",",
			 	 		                                  object[(j-rowCount),i]))
			 	 
			 	}

			 	rowCount <- rowCount + nrow(object)
			}
		}


		max_matrix_id_value <<- max_matrix_id_value + 1

		return(new("FLMatrix", 
			       odbc_connection = connection, 
			       db_name = result_db_name, 
			       matrix_table = result_matrix_table, 
				   matrix_id_value = max_matrix_id_value-1,
				   matrix_id_colname = "MATRIX_ID", 
				   row_id_colname = "ROW_ID", 
				   col_id_colname = "COL_ID", 
				   cell_val_colname = "CELL_VAL",
				   nrow = rowCount, 
				   ncol = ncol, 
				   dimnames = list(c(),c())))
	}
}

rbind.numeric <- function(object,...)
{
	objectVec<-list(object,...)
	connection <- 0

	ncol <-0

	for(j in 1:length(objectVec))
	{
		if(is.FLMatrix(objectVec[[j]]) || is.matrix(objectVec[[j]]) || is.data.frame(objectVec[[j]]))
		{
			if(ncol ==0)
			{
				ncol <- ncol(objectVec[[j]])
			}
			else if(ncol != ncol(objectVec[[j]]))
			stop(" number of columns of matrix arguments must match ")
		}

		if(is.FLMatrix(objectVec[[j]]))
		{
			connection <- objectVec[[j]]@odbc_connection
		}
		if(is.FLVector(objectVec[[j]]))
		{
			connection <- objectVec[[j]]@table@odbc_connection
		}
		else if(is.FLSparseMatrix(objectVec[[j]]) || class(objectVec[[j]])=="dgCMatrix")
		{
			stop("input parameter cannot be sparse")
		}
	}

	if(connection==0)
	{
		return(base::rbind(object,...))
	}

	if(ncol == 0)
	{
		for(j in 1:length(objectVec))
		{
			if(is.FLVector(objectVec[[j]]) || is.numeric(objectVec[[j]]))
			{
				if(ncol < length(objectVec[[j]]))
				{
					ncol <- length(objectVec[[j]])
				}
			}
		}
	}

	flag1Check(connection)

	rowCount <- 0

	if (length(objectVec) > 1)
	{
	    for(k in 1:ncol)
        {
            j <- k
            if(j > length(object))
            j <- 1

            sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						    " SELECT ",max_matrix_id_value,",
								     ",rowCount+1,","
								      ,k,","
								      ,object[j])

            sqlSendUpdate(connection,sqlstr0)
            j <- j+1
        }

	    rowCount <- rowCount + 1

	    for (i in 2:length(objectVec))
		{
			if (class(objectVec[[i]]) == "FLMatrix")
			{
				object <- objectVec[[i]]

				if(object@ncol != ncol)
				{
					stop(" number of columns of matrix arguments must match ")
				}
				sqlstr0<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,
					            " SELECT ",max_matrix_id_value,",
							              a.",object@row_id_colname,"+",rowCount,",
							              a.",object@col_id_colname,",
							              a.",object@cell_val_colname," 
							    FROM   ",remoteTable(object)," a 
							    WHERE  a.",object@matrix_id_colname," = ",object@matrix_id_value)

	            sqlSendUpdate(connection,sqlstr0)

                rowCount <- rowCount + object@nrow
			}

			if (class(objectVec[[i]]) == "FLVector")
			{
				object <- objectVec[[i]]

				if(object@table@isDeep)
				{
					sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             ",rowCount+1,",
								             a.",object@table@var_id_name,",
								             a.",object@col_name," 
								      FROM   ",remoteTable(object@table)," a 
								      WHERE  a.",object@table@primary_key," = ",object@vector_id_value," AND 
								             a.",object@table@var_id_name,"<",ncol+1)
	
		            sqlSendUpdate(connection,sqlstr0)

		            if(object@size < ncol)
		            {
		            	for(k in 1:(ncol%/%object@size))
		            	{
		            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								            " SELECT ",max_matrix_id_value,",
										             ",rowCount+1,",
										             (a.",object@table@var_id_name,"+(",k,"*",object@size,")),
										             a.",object@col_name," 
										      FROM ",remoteTable(object@table)," a 
										      WHERE a.",object@table@primary_key," = ",object@vector_id_value," AND 
										            (a.",object@table@var_id_name,"+(",k,"*",object@size,"))<",ncol+1)
	
		                    sqlSendUpdate(connection,sqlstr0)
		            	}
		            }
		        }
		        else if(!object@table@isDeep)
		        {
					sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             ",rowCount+1,",
								             a.",object@table@primary_key,",
								             a.",object@col_name," 
								      FROM   ",remoteTable(object@table)," a 
								      WHERE a.",object@table@primary_key,"<",ncol+1)
	
		            sqlSendUpdate(connection,sqlstr0)

		            if(object@size < ncol)
		            {
		            	for(k in 1:(ncol%/%object@size))
		            	{
		            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								            " SELECT ",max_matrix_id_value,",
										             ",rowCount+1,",
										             (a.",object@table@primary_key,"+(",k,"*",object@size,")),
										              a.",object@col_name," 
										      FROM ",remoteTable(object@table)," a 
										      WHERE (a.",object@table@primary_key,"+(",k,"*",object@size,"))<",ncol+1)
	
		                    sqlSendUpdate(connection,sqlstr0)
		            	}
		            }
		        }

		        rowCount <- rowCount + 1
			}

			else if (is.vector(objectVec[[i]]))
			{
				object <- objectVec[[i]]

				for(k in 1:ncol)
		        {
		            j <- k
		            if(j > length(object))
		            j <- 1

		            sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								    " SELECT ",max_matrix_id_value,",
										     ",rowCount+1,","
										      ,k,","
										      ,object[j])
	
		            sqlSendUpdate(connection,sqlstr0)
		            j <- j+1
		        }

		        rowCount <- rowCount + 1
			}

			else if(is.matrix(objectVec[[i]]) || is.data.frame(objectVec[[i]]))
			{
				object <- objectVec[[i]]

				for (i in 1:ncol(object))
				for (j in (rowCount+1):(rowCount+nrow(object)))
				{
				 	
			 	 	sqlSendUpdate(connection,paste0(" INSERT INTO ",result_matrix_table,
			 	 		                       " SELECT ",max_matrix_id_value,",",
			 	 		                                  j,",",
			 	 		                                  i,",",
			 	 		                                  object[(j-rowCount),i]))
			 	 
			 	}

			 	rowCount <- rowCount + nrow(object)
			}
		}


		max_matrix_id_value <<- max_matrix_id_value + 1

		return(new("FLMatrix", 
			       odbc_connection = connection, 
			       db_name = result_db_name, 
			       matrix_table = result_matrix_table, 
				   matrix_id_value = max_matrix_id_value-1,
				   matrix_id_colname = "MATRIX_ID", 
				   row_id_colname = "ROW_ID", 
				   col_id_colname = "COL_ID", 
				   cell_val_colname = "CELL_VAL",
				   nrow = rowCount, 
				   ncol = ncol, 
				   dimnames = list(c(),c())))
	}
}

rbind.data.frame <- function(object,...)
{
	objectVec<-list(object,...)
	connection <- 0

	ncol <-0

	for(j in 1:length(objectVec))
	{
		if(is.FLMatrix(objectVec[[j]]) || is.matrix(objectVec[[j]]) || is.data.frame(objectVec[[j]]))
		{
			if(ncol == 0)
			{
				ncol <- ncol(objectVec[[j]])
			}
			else if(ncol != ncol(objectVec[[j]]))
			stop(" number of columns of matrix arguments must match ")
		}

		if(is.FLMatrix(objectVec[[j]]))
		{
			connection <- objectVec[[j]]@odbc_connection
		}
		if(is.FLVector(objectVec[[j]]))
		{
			connection <- objectVec[[j]]@table@odbc_connection
		}
		else if(is.FLSparseMatrix(objectVec[[j]]) || class(objectVec[[j]])=="dgCMatrix")
		{
			stop("input parameter cannot be sparse")
		}
	}

	if(connection==0)
	{
		return(base::rbind(object,...))
	}


	flag1Check(connection)

	rowCount <- 0
	ncol <- ncol(object)

	if (length(objectVec) > 1)
	{
	    for (i in 1:ncol(object))
		for (j in (rowCount+1):(rowCount+nrow(object)))
		{
		 	
	 	 	sqlSendUpdate(connection,paste0(" INSERT INTO ",result_matrix_table,
	 	 		                       " SELECT ",max_matrix_id_value,",",
	 	 		                                  j,",",
	 	 		                                  i,",",
	 	 		                                  object[(j-rowCount),i]))
	 	 
	 	}

	    rowCount <- rowCount + nrow(object)

	    for (i in 2:length(objectVec))
		{
			if (class(objectVec[[i]]) == "FLMatrix")
			{
				object <- objectVec[[i]]

				sqlstr0<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,
					            " SELECT ",max_matrix_id_value,",
							              a.",object@row_id_colname,"+",rowCount,",
							              a.",object@col_id_colname,",
							              a.",object@cell_val_colname," 
							    FROM   ",remoteTable(object)," a 
							    WHERE  a.",object@matrix_id_colname," = ",object@matrix_id_value)

	            sqlSendUpdate(connection,sqlstr0)

                rowCount <- rowCount + object@nrow
			}

			if (class(objectVec[[i]]) == "FLVector")
			{
				object <- objectVec[[i]]

				if(object@table@isDeep)
				{
					sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             ",rowCount+1,",
								             a.",object@table@var_id_name,",
								             a.",object@col_name," 
								      FROM   ",remoteTable(object@table)," a 
								      WHERE  a.",object@table@primary_key," = ",object@vector_id_value," AND 
								             a.",object@table@var_id_name,"<",ncol+1)
	
		            sqlSendUpdate(connection,sqlstr0)

		            if(object@size < ncol)
		            {
		            	for(k in 1:(ncol%/%object@size))
		            	{
		            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								            " SELECT ",max_matrix_id_value,",
										             ",rowCount+1,",
										             (a.",object@table@var_id_name,"+(",k,"*",object@size,")),
										             a.",object@col_name," 
										      FROM ",remoteTable(object@table)," a 
										      WHERE a.",object@table@primary_key," = ",object@vector_id_value," AND 
										            (a.",object@table@var_id_name,"+(",k,"*",object@size,"))<",ncol+1)
	
		                    sqlSendUpdate(connection,sqlstr0)
		            	}
		            }
		        }
		        else if(!object@table@isDeep)
		        {
					sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             ",rowCount+1,",
								             a.",object@table@primary_key,",
								             a.",object@col_name," 
								      FROM   ",remoteTable(object@table)," a 
								      WHERE a.",object@table@primary_key,"<",ncol+1)
	
		            sqlSendUpdate(connection,sqlstr0)

		            if(object@size < ncol)
		            {
		            	for(k in 1:(ncol%/%object@size))
		            	{
		            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								            " SELECT ",max_matrix_id_value,",
										             ",rowCount+1,",
										             (a.",object@table@primary_key,"+(",k,"*",object@size,")),
										              a.",object@col_name," 
										      FROM ",remoteTable(object@table)," a 
										      WHERE (a.",object@table@primary_key,"+(",k,"*",object@size,"))<",ncol+1)
	
		                    sqlSendUpdate(connection,sqlstr0)
		            	}
		            }
		        }

		        rowCount <- rowCount + 1
			}

			else if (is.vector(objectVec[[i]]))
			{
				object <- objectVec[[i]]

				for(k in 1:ncol)
		        {
		            j <- k
		            if(j > length(object))
		            j <- 1

		            sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								    " SELECT ",max_matrix_id_value,",
										     ",rowCount+1,","
										      ,k,","
										      ,object[j])
	
		            sqlSendUpdate(connection,sqlstr0)
		            j <- j+1
		        }

		        rowCount <- rowCount + 1
			}

			else if(is.matrix(objectVec[[i]]) || is.data.frame(objectVec[[i]]))
			{
				object <- objectVec[[i]]

				for (i in 1:ncol(object))
				for (j in (rowCount+1):(rowCount+nrow(object)))
				{
				 	
			 	 	sqlSendUpdate(connection,paste0(" INSERT INTO ",result_matrix_table,
			 	 		                       " SELECT ",max_matrix_id_value,",",
			 	 		                                  j,",",
			 	 		                                  i,",",
			 	 		                                  object[(j-rowCount),i]))
			 	 
			 	}

			 	rowCount <- rowCount + nrow(object)
			}
		}


		max_matrix_id_value <<- max_matrix_id_value + 1

		return(new("FLMatrix", 
			       odbc_connection = connection, 
			       db_name = result_db_name, 
			       matrix_table = result_matrix_table, 
				   matrix_id_value = max_matrix_id_value-1,
				   matrix_id_colname = "MATRIX_ID", 
				   row_id_colname = "ROW_ID", 
				   col_id_colname = "COL_ID", 
				   cell_val_colname = "CELL_VAL",
				   nrow = rowCount, 
				   ncol = ncol, 
				   dimnames = list(c(),c())))
	}
}
