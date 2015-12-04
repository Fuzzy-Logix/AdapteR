#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

#' Combine objects by columns.
#'
#' \code{cbind} combines input objects by columns and forms a FLMatrix.
#'
#' \code{cbind} takes a sequence of vector, FLVector, matrix, FLMatrix or data frames arguments,
#' combines them by columns and makes a FLMatrix.
#' @param x... can be a sequence of vector, FLVector, matrix, FLMatrix or data frames
#' @section Constraints:
#' Input matrices, FLMatrices and data frames should have same number of rows.
#' @return \code{cbind} returns a FLMatrix object which is the column-wise combination of input arguments.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLMatrix <- cbind(flmatrix,1:5,flmatrix)
#' @export

cbind <- function (x, ...){
  UseMethod("cbind", x)
}

#' Combine objects by columns.
#'
#' \code{cbind} combines input objects by columns and forms a FLMatrix.
#'
#' \code{cbind} takes a sequence of vector, FLVector, matrix, FLMatrix or data frames arguments,
#' combines them by columns and makes a FLMatrix.
#' @param x... can be a sequence of vector, FLVector, matrix, FLMatrix or data frames
#' @section Constraints:
#' Input matrices, FLMatrices and data frames should have same number of rows.
#' @return \code{cbind} returns a FLMatrix object which is the column-wise combination of input arguments.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLMatrix <- cbind(flmatrix,1:5,flmatrix)
#' @export

cbind.FLMatrix<-function(object,...)
{
	objectVec<-list(object,...)
	connection<-object@odbc_connection

	nrow <-0

	for(j in 1:length(objectVec))
	{
		if(is.FLMatrix(objectVec[[j]]) || is.matrix(objectVec[[j]]) || is.data.frame(objectVec[[j]]))
		{
			if(nrow == 0)
			{
				nrow <- nrow(objectVec[[j]])
			}
			else if(nrow != nrow(objectVec[[j]]))
			stop(" number of rows of matrix arguments must match ")
		}
		else if(is.FLSparseMatrix(objectVec[[j]]) || class(objectVec[[j]])=="dgCMatrix")
		{
			stop("input parameter cannot be sparse")
		}
	}

	flag1Check(connection)

	colCount <- 0
	nrow <- nrow(object)

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
				        FROM  ",remoteTable(object)," a 
				        WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value)

	    sqlSendUpdate(connection,sqlstr0)

	    colCount <- colCount + ncol(object)

	    for (i in 2:length(objectVec))
		{
			if (class(objectVec[[i]]) == "FLMatrix")
			{
				object <- objectVec[[i]]

				sqlstr0<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,
					            " SELECT ",max_matrix_id_value,",
							              a.",object@row_id_colname,",
							              a.",object@col_id_colname,"+",colCount,",
							              a.",object@cell_val_colname," 
							    FROM   ",remoteTable(object)," a 
							    WHERE  a.",object@matrix_id_colname," = ",object@matrix_id_value)

	            sqlSendUpdate(connection,sqlstr0)

                colCount <- colCount + ncol(object)
			}

			if (class(objectVec[[i]]) == "FLVector")
			{
				object <- objectVec[[i]]

				if(object@isDeep)
				{
					sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             a.",object@var_id_name,",
								             ",colCount+1,",
								             a.",object@col_name," 
								      FROM   ",remoteTable(object)," a 
								      WHERE  a.",object@obs_id_colname," = ",object@vector_id_value," AND 
								             a.",object@var_id_name,"<",nrow+1)
	
		            sqlSendUpdate(connection,sqlstr0)

		            if(length(object) < nrow)
		            {
		            	for(k in 1:(nrow%/%length(object)))
		            	{
		            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								            " SELECT ",max_matrix_id_value,",
										             (a.",object@var_id_name,"+(",k,"*",length(object),")),
										             ",colCount+1,",
										             a.",object@col_name," 
										      FROM ",remoteTable(object)," a 
										      WHERE a.",object@obs_id_colname," = ",object@vector_id_value," AND 
										            (a.",object@var_id_name,"+(",k,"*",length(object),"))<",nrow+1)
	
		                    sqlSendUpdate(connection,sqlstr0)
		            	}
		            }
		        }
		        else if(!object@isDeep)
		        {
					sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             a.",object@obs_id_colname,",
								             ",colCount+1,",
								             a.",object@col_name," 
								      FROM   ",remoteTable(object)," a 
								      WHERE a.",object@obs_id_colname,"<",nrow+1)
	
		            sqlSendUpdate(connection,sqlstr0)

		            if(length(object) < nrow)
		            {
		            	for(k in 1:(nrow%/%length(object)))
		            	{
		            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								            " SELECT ",max_matrix_id_value,",
										             (a.",object@obs_id_colname,"+(",k,"*",length(object),")),
										             ",colCount+1,",
										              a.",object@col_name," 
										      FROM ",remoteTable(object)," a 
										      WHERE (a.",object@obs_id_colname,"+(",k,"*",length(object),"))<",nrow+1)
	
		                    sqlSendUpdate(connection,sqlstr0)
		            	}
		            }
		        }

		        colCount <- colCount + 1
			}

			else if (is.vector(objectVec[[i]]))
			{
				object <- objectVec[[i]]

				for(k in 1:nrow)
		        {
		            j <- k
		            if(j > length(object))
		            j <- 1

		            sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								    " SELECT ",max_matrix_id_value,",
										      ",k,",",
										      colCount+1,","
										      ,object[j])
	
		            sqlSendUpdate(connection,sqlstr0)
		            j <- j+1
		        }

		        colCount <- colCount + 1
			}

			else if(is.matrix(objectVec[[i]]) || is.data.frame(objectVec[[i]]))
			{
				object <- objectVec[[i]]

				for (i in (colCount+1):(colCount+ncol(object)))
				for (j in 1:nrow(object))
				{
				 	
			 	 	sqlSendUpdate(connection,paste0(" INSERT INTO ",result_matrix_table,
			 	 		                       " SELECT ",max_matrix_id_value,",",
			 	 		                                  j,",",
			 	 		                                  i,",",
			 	 		                                  object[j,(i-colCount)]))
			 	 
			 	}

			 	colCount <- colCount + ncol(object)
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
				   nrow = nrow, 
				   ncol = colCount, 
				   dimnames = list(c(),c())))
	}
}

cbind.FLVector <- function(object,...)
{
	objectVec<-list(object,...)
	connection<-object@odbc_connection

	nrow <-0

	for(j in 1:length(objectVec))
	{
		if(is.FLMatrix(objectVec[[j]]) || is.matrix(objectVec[[j]])  || is.data.frame(objectVec[[j]]))
		{
			if(nrow ==0)
			{
				nrow <- nrow(objectVec[[j]])
			}
			else if(nrow != nrow(objectVec[[j]]))
			stop(" number of rows of matrix arguments must match ")
		}
		else if(is.FLSparseMatrix(objectVec[[j]]) || class(objectVec[[j]])=="dgCMatrix")
		{
			stop("input parameter cannot be sparse")
		}
	}

	if(nrow == 0)
	{
		for(j in 1:length(objectVec))
		{
			if(is.FLVector(objectVec[[j]]) || is.numeric(objectVec[[j]]))
			{
				if(nrow < length(objectVec[[j]]))
				{
					nrow <- length(objectVec[[j]])
				}
			}
		}
	}

	flag1Check(connection)

	colCount <- 0

	if(length(objectVec) == 1) 
	{
		if(object@isDeep)
		{
			sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
				            " SELECT ",max_matrix_id_value,",
						             a.",object@var_id_name,",
						             ",colCount+1,",
						             a.",object@col_name," 
						      FROM   ",remoteTable(object)," a 
						      WHERE  a.",object@obs_id_colname," = ",object@vector_id_value," AND 
						             a.",object@var_id_name,"<",nrow+1)

            sqlSendUpdate(connection,sqlstr0)

            if(length(object) < nrow)
            {
            	for(k in 1:(nrow%/%length(object)))
            	{
            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             (a.",object@var_id_name,"+(",k,"*",length(object),")),
								             ",colCount+1,",
								             a.",object@col_name," 
								      FROM ",remoteTable(object)," a 
								      WHERE a.",object@obs_id_colname," = ",object@vector_id_value," AND 
								            (a.",object@var_id_name,"+(",k,"*",length(object),"))<",nrow+1)

                    sqlSendUpdate(connection,sqlstr0)
            	}
            }
        }
        else if(!object@isDeep)
        {
			sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
				            " SELECT ",max_matrix_id_value,",
						             a.",object@obs_id_colname,",
						             ",colCount+1,",
						             a.",object@col_name," 
						      FROM   ",remoteTable(object)," a 
						      WHERE a.",object@obs_id_colname,"<",nrow+1)

            sqlSendUpdate(connection,sqlstr0)

            if(length(object) < nrow)
            {
            	for(k in 1:(nrow%/%length(object)))
            	{
            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             (a.",object@obs_id_colname,"+(",k,"*",length(object),")),
								             ",colCount+1,",
								              a.",object@col_name," 
								      FROM ",remoteTable(object)," a 
								      WHERE (a.",object@obs_id_colname,"+(",k,"*",length(object),"))<",nrow+1)

                    sqlSendUpdate(connection,sqlstr0)
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
				   nrow = nrow, 
				   ncol = 1, 
				   dimnames = list(c(),c())))
	}

	else if (length(objectVec) > 1)
	{
	    if(object@isDeep)
		{
			sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
				            " SELECT ",max_matrix_id_value,",
						             a.",object@var_id_name,",
						             ",colCount+1,",
						             a.",object@col_name," 
						      FROM   ",remoteTable(object)," a 
						      WHERE  a.",object@obs_id_colname," = ",object@vector_id_value," AND 
						             a.",object@var_id_name,"<",nrow+1)

            sqlSendUpdate(connection,sqlstr0)

            if(length(object) < nrow)
            {
            	for(k in 1:(nrow%/%length(object)))
            	{
            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             (a.",object@var_id_name,"+(",k,"*",length(object),")),
								             ",colCount+1,",
								             a.",object@col_name," 
								      FROM ",remoteTable(object)," a 
								      WHERE a.",object@obs_id_colname," = ",object@vector_id_value," AND 
								            (a.",object@var_id_name,"+(",k,"*",length(object),"))<",nrow+1)

                    sqlSendUpdate(connection,sqlstr0)
            	}
            }
        }
        else if(!object@isDeep)
        {
			sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
				            " SELECT ",max_matrix_id_value,",
						             a.",object@obs_id_colname,",
						             ",colCount+1,",
						             a.",object@col_name," 
						      FROM   ",remoteTable(object)," a 
						      WHERE a.",object@obs_id_colname,"<",nrow+1)

            sqlSendUpdate(connection,sqlstr0)

            if(length(object) < nrow)
            {
            	for(k in 1:(nrow%/%length(object)))
            	{
            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             (a.",object@obs_id_colname,"+(",k,"*",length(object),")),
								             ",colCount+1,",
								              a.",object@col_name," 
								      FROM ",remoteTable(object)," a 
								      WHERE (a.",object@obs_id_colname,"+(",k,"*",length(object),"))<",nrow+1)

                    sqlSendUpdate(connection,sqlstr0)
            	}
            }
        }

	    colCount <- colCount + 1

	    for (i in 2:length(objectVec))
		{
			if (class(objectVec[[i]]) == "FLMatrix")
			{
				object <- objectVec[[i]]

				if(nrow(object) != nrow)
				{
					stop(" number of columns of matrix arguments must match ")
				}

				sqlstr0<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,
					            " SELECT ",max_matrix_id_value,",
							              a.",object@row_id_colname,",
							              a.",object@col_id_colname,"+",colCount,",
							              a.",object@cell_val_colname," 
							    FROM   ",remoteTable(object)," a 
							    WHERE  a.",object@matrix_id_colname," = ",object@matrix_id_value)

	            sqlSendUpdate(connection,sqlstr0)

                colCount <- colCount + ncol(object)
			}

			if (class(objectVec[[i]]) == "FLVector")
			{
				object <- objectVec[[i]]

				if(object@isDeep)
				{
					sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             a.",object@var_id_name,",
								             ",colCount+1,",
								             a.",object@col_name," 
								      FROM   ",remoteTable(object)," a 
								      WHERE  a.",object@obs_id_colname," = ",object@vector_id_value," AND 
								             a.",object@var_id_name,"<",nrow+1)
	
		            sqlSendUpdate(connection,sqlstr0)

		            if(length(object) < nrow)
		            {
		            	for(k in 1:(nrow%/%length(object)))
		            	{
		            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								            " SELECT ",max_matrix_id_value,",
										             (a.",object@var_id_name,"+(",k,"*",length(object),")),
										             ",colCount+1,",
										             a.",object@col_name," 
										      FROM ",remoteTable(object)," a 
										      WHERE a.",object@obs_id_colname," = ",object@vector_id_value," AND 
										            (a.",object@var_id_name,"+(",k,"*",length(object),"))<",nrow+1)
	
		                    sqlSendUpdate(connection,sqlstr0)
		            	}
		            }
		        }
		        else if(!object@isDeep)
		        {
					sqlstr0<-paste0(" INSERT INTO ",getRemoteTableName(result_db_name,result_matrix_table),
						            " SELECT ",max_matrix_id_value,",
								             a.",object@obs_id_colname,",
								             ",colCount+1,",
								             a.",object@col_name," 
								      FROM   ",remoteTable(object)," a 
								      WHERE a.",object@obs_id_colname,"<",nrow+1)
	
		            sqlSendUpdate(connection,sqlstr0)

		            if(length(object) < nrow)
		            {
		            	for(k in 1:(nrow%/%length(object)))
		            	{
		            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								            " SELECT ",max_matrix_id_value,",
										             (a.",object@obs_id_colname,"+(",k,"*",length(object),")),
										             ",colCount+1,",
										              a.",object@col_name," 
										      FROM ",remoteTable(object)," a 
										      WHERE (a.",object@obs_id_colname,"+(",k,"*",length(object),"))<",nrow+1)
	
		                    sqlSendUpdate(connection,sqlstr0)
		            	}
		            }
		        }

		        colCount <- colCount + 1
			}

			else if (is.vector(objectVec[[i]]))
			{
				object <- objectVec[[i]]

				for(k in 1:nrow)
		        {
		            j <- k
		            if(j > length(object))
		            j <- 1

		            sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								    " SELECT ",max_matrix_id_value,",
										      ",k,",",
										      colCount+1,","
										      ,object[j])
	
		            sqlSendUpdate(connection,sqlstr0)
		            j <- j+1
		        }

		        colCount <- colCount + 1
			}

			else if(is.matrix(objectVec[[i]]) || is.data.frame(objectVec[[i]]))
			{
				object <- objectVec[[i]]

				for (i in (colCount+1):(colCount+ncol(object)))
				for (j in 1:nrow(object))
				{
				 	
			 	 	sqlSendUpdate(connection,paste0(" INSERT INTO ",result_matrix_table,
			 	 		                       " SELECT ",max_matrix_id_value,",",
			 	 		                                  j,",",
			 	 		                                  i,",",
			 	 		                                  object[j,(i-colCount)]))
			 	 
			 	}

			 	colCount <- colCount + ncol(object)
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
				   nrow = nrow, 
				   ncol = colCount, 
				   dimnames = list(c(),c())))
	}
}

cbind.matrix <- function(object,...)
{
	objectVec<-list(object,...)
	connection <- 0

	nrow <-0

	for(j in 1:length(objectVec))
	{
		if(is.FLMatrix(objectVec[[j]]) || is.matrix(objectVec[[j]]) || is.data.frame(objectVec[[j]]))
		{
			if(nrow == 0)
			{
				nrow <- nrow(objectVec[[j]])
			}
			else if(nrow != nrow(objectVec[[j]]))
			stop(" number of rows of matrix arguments must match ")
		}

		if(is.FLMatrix(objectVec[[j]]))
		{
			connection <- objectVec[[j]]@odbc_connection
		}
		if(is.FLVector(objectVec[[j]]))
		{
			connection <- objectVec[[j]]@odbc_connection
		}
		else if(is.FLSparseMatrix(objectVec[[j]]) || class(objectVec[[j]])=="dgCMatrix")
		{
			stop("input parameter cannot be sparse")
		}
	}

	if(connection==0)
	{
		return(base::cbind(object,...))
	}


	flag1Check(connection)

	colCount <- 0
	nrow <- nrow(object)

	if (length(objectVec) > 1)
	{
	    for (i in (colCount+1):(colCount+ncol(object)))
		for (j in 1:nrow(object))
		{
		 	
	 	 	sqlSendUpdate(connection,paste0(" INSERT INTO ",result_matrix_table,
	 	 		                       " SELECT ",max_matrix_id_value,",",
	 	 		                                  j,",",
	 	 		                                  i,",",
	 	 		                                  object[j,(i-colCount)]))
	 	 
	 	}

	 	colCount <- colCount + ncol(object)

	    for (i in 2:length(objectVec))
		{
			if (class(objectVec[[i]]) == "FLMatrix")
			{
				object <- objectVec[[i]]

				if(nrow(object) != nrow)
				{
					stop(" number of columns of matrix arguments must match ")
				}

				sqlstr0<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,
					            " SELECT ",max_matrix_id_value,",
							              a.",object@row_id_colname,",
							              a.",object@col_id_colname,"+",colCount,",
							              a.",object@cell_val_colname," 
							    FROM   ",remoteTable(object)," a 
							    WHERE  a.",object@matrix_id_colname," = ",object@matrix_id_value)

	            sqlSendUpdate(connection,sqlstr0)

                colCount <- colCount + ncol(object)
			}

			if (class(objectVec[[i]]) == "FLVector")
			{
				object <- objectVec[[i]]

				if(object@isDeep)
				{
					sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             a.",object@var_id_name,",
								             ",colCount+1,",
								             a.",object@col_name," 
								      FROM   ",remoteTable(object)," a 
								      WHERE  a.",object@obs_id_colname," = ",object@vector_id_value," AND 
								             a.",object@var_id_name,"<",nrow+1)
	
		            sqlSendUpdate(connection,sqlstr0)

		            if(length(object) < nrow)
		            {
		            	for(k in 1:(nrow%/%length(object)))
		            	{
		            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								            " SELECT ",max_matrix_id_value,",
										             (a.",object@var_id_name,"+(",k,"*",length(object),")),
										             ",colCount+1,",
										             a.",object@col_name," 
										      FROM ",remoteTable(object)," a 
										      WHERE a.",object@obs_id_colname," = ",object@vector_id_value," AND 
										            (a.",object@var_id_name,"+(",k,"*",length(object),"))<",nrow+1)
	
		                    sqlSendUpdate(connection,sqlstr0)
		            	}
		            }
		        }
		        else if(!object@isDeep)
		        {
					sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             a.",object@obs_id_colname,",
								             ",colCount+1,",
								             a.",object@col_name," 
								      FROM   ",remoteTable(object)," a 
								      WHERE a.",object@obs_id_colname,"<",nrow+1)
	
		            sqlSendUpdate(connection,sqlstr0)

		            if(length(object) < nrow)
		            {
		            	for(k in 1:(nrow%/%length(object)))
		            	{
		            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								            " SELECT ",max_matrix_id_value,",
										             (a.",object@obs_id_colname,"+(",k,"*",length(object),")),
										             ",colCount+1,",
										              a.",object@col_name," 
										      FROM ",remoteTable(object)," a 
										      WHERE (a.",object@obs_id_colname,"+(",k,"*",length(object),"))<",nrow+1)
	
		                    sqlSendUpdate(connection,sqlstr0)
		            	}
		            }
		        }

		        colCount <- colCount + 1
			}

			else if (is.vector(objectVec[[i]]))
			{
				object <- objectVec[[i]]

				for(k in 1:nrow)
		        {
		            j <- k
		            if(j > length(object))
		            j <- 1

		            sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								    " SELECT ",max_matrix_id_value,",
										      ",k,",",
										      colCount+1,","
										      ,object[j])
	
		            sqlSendUpdate(connection,sqlstr0)
		            j <- j+1
		        }

		        colCount <- colCount + 1
			}

			else if(is.matrix(objectVec[[i]]) || is.data.frame(objectVec[[i]]))
			{
				object <- objectVec[[i]]

				for (i in (colCount+1):(colCount+ncol(object)))
				for (j in 1:nrow(object))
				{
				 	
			 	 	sqlSendUpdate(connection,paste0(" INSERT INTO ",result_matrix_table,
			 	 		                       " SELECT ",max_matrix_id_value,",",
			 	 		                                  j,",",
			 	 		                                  i,",",
			 	 		                                  object[j,(i-colCount)]))
			 	 
			 	}

			 	colCount <- colCount + ncol(object)
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
				   nrow = nrow, 
				   ncol = colCount, 
				   dimnames = list(c(),c())))
	}
}

cbind.numeric <- function(object,...)
{
	objectVec<-list(object,...)
	connection <- 0

	nrow <-0

	for(j in 1:length(objectVec))
	{
		if(is.FLMatrix(objectVec[[j]]) || is.matrix(objectVec[[j]]) || is.data.frame(objectVec[[j]]))
		{
			if(nrow ==0)
			{
				nrow <- nrow(objectVec[[j]])
			}
			else if(nrow != nrow(objectVec[[j]]))
			stop(" number of rows of matrix arguments must match ")
		}

		if(is.FLMatrix(objectVec[[j]]))
		{
			connection <- objectVec[[j]]@odbc_connection
		}
		if(is.FLVector(objectVec[[j]]))
		{
			connection <- objectVec[[j]]@odbc_connection
		}
		else if(is.FLSparseMatrix(objectVec[[j]]) || class(objectVec[[j]])=="dgCMatrix")
		{
			stop("input parameter cannot be sparse")
		}
	}

	if(connection==0)
	{
		return(base::cbind(object,...))
	}

	if(nrow == 0)
	{
		for(j in 1:length(objectVec))
		{
			if(is.FLVector(objectVec[[j]]) || is.numeric(objectVec[[j]]))
			{
				if(nrow < length(objectVec[[j]]))
				{
					nrow <- length(objectVec[[j]])
				}
			}
		}
	}

	flag1Check(connection)

	colCount <- 0

	if (length(objectVec) > 1)
	{
	    for(k in 1:nrow)
        {
            j <- k
            if(j > length(object))
            j <- 1

            sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						    " SELECT ",max_matrix_id_value,",
								      ",k,",",
								      colCount+1,","
								      ,object[j])

            sqlSendUpdate(connection,sqlstr0)
            j <- j+1
        }

        colCount <- colCount + 1

	    for (i in 2:length(objectVec))
		{
			if (class(objectVec[[i]]) == "FLMatrix")
			{
				object <- objectVec[[i]]

				if(nrow(object) != nrow)
				{
					stop(" number of columns of matrix arguments must match ")
				}

				sqlstr0<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,
					            " SELECT ",max_matrix_id_value,",
							              a.",object@row_id_colname,",
							              a.",object@col_id_colname,"+",colCount,",
							              a.",object@cell_val_colname," 
							    FROM   ",remoteTable(object)," a 
							    WHERE  a.",object@matrix_id_colname," = ",object@matrix_id_value)

	            sqlSendUpdate(connection,sqlstr0)

                colCount <- colCount + ncol(object)
			}

			if (class(objectVec[[i]]) == "FLVector")
			{
				object <- objectVec[[i]]

				if(object@isDeep)
				{
					sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             a.",object@var_id_name,",
								             ",colCount+1,",
								             a.",object@col_name," 
								      FROM   ",remoteTable(object)," a 
								      WHERE  a.",object@obs_id_colname," = ",object@vector_id_value," AND 
								             a.",object@var_id_name,"<",nrow+1)
	
		            sqlSendUpdate(connection,sqlstr0)

		            if(length(object) < nrow)
		            {
		            	for(k in 1:(nrow%/%length(object)))
		            	{
		            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								            " SELECT ",max_matrix_id_value,",
										             (a.",object@var_id_name,"+(",k,"*",length(object),")),
										             ",colCount+1,",
										             a.",object@col_name," 
										      FROM ",remoteTable(object)," a 
										      WHERE a.",object@obs_id_colname," = ",object@vector_id_value," AND 
										            (a.",object@var_id_name,"+(",k,"*",length(object),"))<",nrow+1)
	
		                    sqlSendUpdate(connection,sqlstr0)
		            	}
		            }
		        }
		        else if(!object@isDeep)
		        {
					sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             a.",object@obs_id_colname,",
								             ",colCount+1,",
								             a.",object@col_name," 
								      FROM   ",remoteTable(object)," a 
								      WHERE a.",object@obs_id_colname,"<",nrow+1)
	
		            sqlSendUpdate(connection,sqlstr0)

		            if(length(object) < nrow)
		            {
		            	for(k in 1:(nrow%/%length(object)))
		            	{
		            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								            " SELECT ",max_matrix_id_value,",
										             (a.",object@obs_id_colname,"+(",k,"*",length(object),")),
										             ",colCount+1,",
										              a.",object@col_name," 
										      FROM ",remoteTable(object)," a 
										      WHERE (a.",object@obs_id_colname,"+(",k,"*",length(object),"))<",nrow+1)
	
		                    sqlSendUpdate(connection,sqlstr0)
		            	}
		            }
		        }

		        colCount <- colCount + 1
			}

			else if (is.vector(objectVec[[i]]))
			{
				object <- objectVec[[i]]

				for(k in 1:nrow)
		        {
		            j <- k
		            if(j > length(object))
		            j <- 1

		            sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								    " SELECT ",max_matrix_id_value,",
										      ",k,",",
										      colCount+1,","
										      ,object[j])
	
		            sqlSendUpdate(connection,sqlstr0)
		            j <- j+1
		        }

		        colCount <- colCount + 1
			}

			else if(is.matrix(objectVec[[i]]) || is.data.frame(objectVec[[i]]))
			{
				object <- objectVec[[i]]

				for (i in (colCount+1):(colCount+ncol(object)))
				for (j in 1:nrow(object))
				{
				 	
			 	 	sqlSendUpdate(connection,paste0(" INSERT INTO ",result_matrix_table,
			 	 		                       " SELECT ",max_matrix_id_value,",",
			 	 		                                  j,",",
			 	 		                                  i,",",
			 	 		                                  object[j,(i-colCount)]))
			 	 
			 	}

			 	colCount <- colCount + ncol(object)
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
				   nrow = nrow, 
				   ncol = colCount, 
				   dimnames = list(c(),c())))
	}
}

cbind.data.frame <- function(object,...)
{
	objectVec<-list(object,...)
	connection <- 0

	nrow <-0

	for(j in 1:length(objectVec))
	{
		if(is.FLMatrix(objectVec[[j]]) || is.matrix(objectVec[[j]]) || is.data.frame(objectVec[[j]]))
		{
			if(nrow == 0)
			{
				nrow <- nrow(objectVec[[j]])
			}
			else if(nrow != nrow(objectVec[[j]]))
			stop(" number of rows of matrix arguments must match ")
		}

		if(is.FLMatrix(objectVec[[j]]))
		{
			connection <- objectVec[[j]]@odbc_connection
		}
		if(is.FLVector(objectVec[[j]]))
		{
			connection <- objectVec[[j]]@odbc_connection
		}
		else if(is.FLSparseMatrix(objectVec[[j]]) || class(objectVec[[j]])=="dgCMatrix")
		{
			stop("input parameter cannot be sparse")
		}
	}

	if(connection==0)
	{
		return(base::cbind(object,...))
	}


	flag1Check(connection)

	colCount <- 0
	nrow <- nrow(object)

	if (length(objectVec) > 1)
	{
	    for (i in (colCount+1):(colCount+ncol(object)))
		for (j in 1:nrow(object))
		{
		 	
	 	 	sqlSendUpdate(connection,paste0(" INSERT INTO ",result_matrix_table,
	 	 		                       " SELECT ",max_matrix_id_value,",",
	 	 		                                  j,",",
	 	 		                                  i,",",
	 	 		                                  object[j,(i-colCount)]))
	 	 
	 	}

	 	colCount <- colCount + ncol(object)

	    for (i in 2:length(objectVec))
		{
			if (class(objectVec[[i]]) == "FLMatrix")
			{
				object <- objectVec[[i]]

				if(nrow(object) != nrow)
				{
					stop(" number of columns of matrix arguments must match ")
				}

				sqlstr0<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,
					            " SELECT ",max_matrix_id_value,",
							              a.",object@row_id_colname,",
							              a.",object@col_id_colname,"+",colCount,",
							              a.",object@cell_val_colname," 
							    FROM   ",remoteTable(object)," a 
							    WHERE  a.",object@matrix_id_colname," = ",object@matrix_id_value)

	            sqlSendUpdate(connection,sqlstr0)

                colCount <- colCount + ncol(object)
			}

			if (class(objectVec[[i]]) == "FLVector")
			{
				object <- objectVec[[i]]

				if(object@isDeep)
				{
					sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             a.",object@var_id_name,",
								             ",colCount+1,",
								             a.",object@col_name," 
								      FROM   ",remoteTable(object)," a 
								      WHERE  a.",object@obs_id_colname," = ",object@vector_id_value," AND 
								             a.",object@var_id_name,"<",nrow+1)
	
		            sqlSendUpdate(connection,sqlstr0)

		            if(length(object) < nrow)
		            {
		            	for(k in 1:(nrow%/%length(object)))
		            	{
		            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								            " SELECT ",max_matrix_id_value,",
										             (a.",object@var_id_name,"+(",k,"*",length(object),")),
										             ",colCount+1,",
										             a.",object@col_name," 
										      FROM ",remoteTable(object)," a 
										      WHERE a.",object@obs_id_colname," = ",object@vector_id_value," AND 
										            (a.",object@var_id_name,"+(",k,"*",length(object),"))<",nrow+1)
	
		                    sqlSendUpdate(connection,sqlstr0)
		            	}
		            }
		        }
		        else if(!object@isDeep)
		        {
					sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
						            " SELECT ",max_matrix_id_value,",
								             a.",object@obs_id_colname,",
								             ",colCount+1,",
								             a.",object@col_name," 
								      FROM   ",remoteTable(object)," a 
								      WHERE a.",object@obs_id_colname,"<",nrow+1)
	
		            sqlSendUpdate(connection,sqlstr0)

		            if(length(object) < nrow)
		            {
		            	for(k in 1:(nrow%/%length(object)))
		            	{
		            		sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								            " SELECT ",max_matrix_id_value,",
										             (a.",object@obs_id_colname,"+(",k,"*",length(object),")),
										             ",colCount+1,",
										              a.",object@col_name," 
										      FROM ",remoteTable(object)," a 
										      WHERE (a.",object@obs_id_colname,"+(",k,"*",length(object),"))<",nrow+1)
	
		                    sqlSendUpdate(connection,sqlstr0)
		            	}
		            }
		        }

		        colCount <- colCount + 1
			}

			else if (is.vector(objectVec[[i]]))
			{
				object <- objectVec[[i]]

				for(k in 1:nrow)
		        {
		            j <- k
		            if(j > length(object))
		            j <- 1

		            sqlstr0<-paste0(" INSERT INTO ",result_db_name,".",result_matrix_table,
								    " SELECT ",max_matrix_id_value,",
										      ",k,",",
										      colCount+1,","
										      ,object[j])
	
		            sqlSendUpdate(connection,sqlstr0)
		            j <- j+1
		        }

		        colCount <- colCount + 1
			}

			else if(is.matrix(objectVec[[i]]) || is.data.frame(objectVec[[i]]))
			{
				object <- objectVec[[i]]

                                ##gk: refactor to apply!  remove for loops!
				for (i in (colCount+1):(colCount+ncol(object)))
				for (j in 1:nrow(object))
				{
				 	
			 	 	sqlSendUpdate(connection,paste0(" INSERT INTO ",result_matrix_table,
			 	 		                       " SELECT ",max_matrix_id_value,",",
			 	 		                                  j,",",
			 	 		                                  i,",",
			 	 		                                  object[j,(i-colCount)]))
			 	 
			 	}

			 	colCount <- colCount + ncol(object)
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
				   nrow = nrow, 
				   ncol = colCount, 
				   dimnames = list(c(),c())))
	}
}
