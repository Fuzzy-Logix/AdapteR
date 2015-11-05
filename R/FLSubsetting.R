#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLTable.R
#' @include FLIs.R
#' @include FLDims.R
#' @include FLPrint.R
NULL

#' Extract part of FLMatrix object.
#'
#' \code{[]} acts on FLMatrix objects and extracts parts of them.
#'
#' 
#' @param object is a FLMatrix object
#' @param nrow is a vector input corresponding to rows to be extracted
#' @param ncol is a vector input corresponding to columns to be extracted
#' @return \code{[]} returns FLMatrix object after extraction
#' which replicates the equivalent R extraction.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' resultFLmatrix <- flmatrix[1,]
#' @export

`[.FLMatrix`<-function(object,nrow=1,ncol=1)
{
	connection<-object@odbc_connection
	sqlQuery(connection, paste("DATABASE", object@db_name,";
								SET ROLE ALL;"))

	if(nargs()==2 && missing(nrow)) { return(object[,]) }
	if(nargs()==2)
	{
		if(nrow>object@nrow*object@ncol) { stop("subscript_out_of_bounds") }
		return(sqlQuery(connection,paste0(" SELECT ",object@cell_val_colname,
											" FROM ",object@matrix_table,
											" ORDER BY ",object@matrix_id_colname,",",object@col_id_colname,",",object@row_id_colname))[[1]][nrow])
	}
	
	if(is.character(nrow)){ nrow <- sapply(nrow, function(x) which(rownames(object) %in% x),USE.NAMES=FALSE) }  ## transform character index to numeric
	if(is.character(ncol)){ ncol <- sapply(ncol, function(x) which(colnames(object) %in% x),USE.NAMES=FALSE) }
	if(sum(abs(nrow) > object@nrow) > 0 || sum(abs(ncol) > object@ncol) > 0){ stop("subscript_out_of_bounds") }

	flag1Check(connection)

	if(length(nrow)>0 && length(ncol)>0)
	{
		if(length(nrow) == 1 && length(ncol) == 1)
		{
			if(missing(ncol))
			{
				if(missing(nrow))
				{
					sqlstr<-paste("INSERT INTO ",result_db_name,".",result_matrix_table,
								  " SELECT ",max_matrix_id_value,
								           ",",object@row_id_colname,
								           ",",object@col_id_colname,
								           ",",object@cell_val_colname,
								  " FROM ",object@matrix_table,
								  " WHERE ",object@matrix_id_colname," = ",object@matrix_id_value)
					sqlQuery(connection,sqlstr)

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
				               nrow = object@nrow, 
				               ncol = object@ncol, 
				               dimnames = object@dimnames))
					
				}
				else
				{
					
					sqlstr<-paste(" INSERT INTO ",result_db_name,".",result_matrix_table,
								  " SELECT ",max_matrix_id_value,",",object@row_id_colname,",",object@col_id_colname,",",object@cell_val_colname,  
								  " FROM ",object@matrix_table,
								  " WHERE ",object@row_id_colname," = ",nrow,
								  " AND ",object@matrix_id_colname," = ",object@matrix_id_value)
					
					sqlQuery(connection,sqlstr)

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
				              ncol = object@ncol, 
				              dimnames = list(object@dimnames[[1]][nrow],object@dimnames[[2]])))				}
			    }
			else 
			{
				if(missing(nrow))
				{
					sqlstr<-paste(" INSERT INTO ",result_db_name,".",result_matrix_table,
								  " SELECT ",max_matrix_id_value,",",object@row_id_colname,",",object@col_id_colname,",",object@cell_val_colname,  
								  " FROM ",object@matrix_table,
								  " WHERE ",object@col_id_colname," = ",ncol,
								  " AND ",object@matrix_id_colname," = ",object@matrix_id_value)
					
					sqlQuery(connection,sqlstr)

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
				               nrow = object@nrow, 
				               ncol = 1, 
				               dimnames = list(object@dimnames[[1]],object@dimnames[[2]][ncol])))
				}
				else
				{
					sqlstr<-paste(" INSERT INTO ",result_db_name,".",result_matrix_table,
								  " SELECT ",max_matrix_id_value,",",object@row_id_colname,",",object@col_id_colname,",",object@cell_val_colname,  
								  " FROM ",object@matrix_table,
								  " WHERE ",object@col_id_colname," = ",ncol,
								  " AND ",object@row_id_colname," = ",nrow,
								  " AND ",object@matrix_id_colname," = ",object@matrix_id_value)
						
					
					sqlQuery(connection,sqlstr)

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
				              ncol = 1, 
				              dimnames = list(object@dimnames[[1]][nrow],object@dimnames[[2]][ncol])))
				}
			}
		}
		else
		{
			if(missing(ncol))
			{
				temp <- rep(paste0(" or ",object@row_id_colname," = ",nrow[2:length(nrow)]," "),length.out=length(nrow)-1)
				temp<-paste(temp,collapse=" ")
				sqlstr <- paste("SELECT ",object@matrix_id_colname," AS MATRIX_ID, ",object@row_id_colname," AS ROW_ID, ",object@col_id_colname," AS COL_ID, ",
								  object@cell_val_colname," AS CELL_VAL ",  
								  " FROM ",object@db_name,".",object@matrix_table,
							  " WHERE (",object@row_id_colname," = ",nrow[1],temp,
							  " ) AND ", object@matrix_id_colname," = ",object@matrix_id_value,
							  " ORDER BY 1,2,3;")
				retobj<-sqlQuery(connection,sqlstr)
				temp_df <- data.frame()
				for(i in nrow)
				temp_df <- rbind(temp_df,retobj[retobj[,object@row_id_colname]==i,])
				m <- matrix(temp_df$CELL_VAL,length(nrow),byrow=TRUE,dimnames=list(object@dimnames[[1]][nrow],object@dimnames[[2]]))
				return(as.FLMatrix(m,connection))
			}
			else if(missing(nrow))
			{
				temp <- rep(paste0("or ",object@col_id_colname," = ",ncol[2:length(ncol)]," "),length.out=length(ncol)-1)
				temp<-paste(temp,collapse=" ")
				sqlstr<-paste("SELECT ",object@matrix_id_colname," AS MATRIX_ID, ",object@row_id_colname," AS ROW_ID, ",object@col_id_colname," AS COL_ID, ",
								  object@cell_val_colname," AS CELL_VAL ",  
							  " FROM ",object@matrix_table,
							  "WHERE (",object@col_id_colname," = ",ncol[1],temp,
							  " ) AND ", object@matrix_id_colname," = ",object@matrix_id_value,
							  " ORDER BY 1,2,3;")
				retobj<-sqlQuery(connection,sqlstr)
				temp_df <- data.frame()
				for(i in ncol)
				temp_df <- rbind(temp_df,retobj[retobj[,object@col_id_colname]==i,])
				m <- matrix(temp_df$CELL_VAL,ncol=length(ncol),dimnames=list(object@dimnames[[1]],object@dimnames[[2]][ncol]))
				return(as.FLMatrix(m,connection))
			}
			else
			{
				temp1 <- paste("WHERE (",object@row_id_colname," = ",nrow[1])
				temp2 <- character(0)
				if(length(nrow)>1)
				{
					temp2 <- rep(paste0("or ",object@row_id_colname," = ",nrow[2:length(nrow)]," "),length.out=length(nrow)-1)
					temp2 <- paste(temp2,collapse=" ")
				}
				temp3 <- paste(" AND (",object@col_id_colname,"=",ncol[1])
				temp4 <- character(0)
				if(length(ncol)>1)
				{
					temp4 <- rep(paste0("or ",object@col_id_colname," = ",ncol[2:length(ncol)]," "),length.out=length(ncol)-1)
					temp4 <- paste(temp4,collapse=" ")
				}

				sqlstr<-paste("SELECT ",object@matrix_id_colname," AS MATRIX_ID, ",object@row_id_colname," AS ROW_ID, ",object@col_id_colname," AS COL_ID, ",
							   object@cell_val_colname," AS CELL_VAL ",  
							  " FROM ",object@matrix_table,
							   temp1,temp2,")",temp3,temp4,")",
							   " AND ",object@matrix_id_colname," = ",object@matrix_id_value,
							   " ORDER BY 1,2,3;")

				retobj<-sqlQuery(connection,sqlstr)
				temp_df <- data.frame()
				for(i in nrow)
				for(j in ncol)
				temp_df <- rbind(temp_df,retobj[((retobj[,object@row_id_colname]==i) & (retobj[,object@col_id_colname]==j)),])
				m <- matrix(temp_df$CELL_VAL,length(nrow),length(ncol),byrow=TRUE,dimnames=list(object@dimnames[[1]][nrow],object@dimnames[[2]][ncol]))
				return(as.FLMatrix(m,connection))
			}
		}
	}
	else stop("invalid_subscripts")
}

#' Extract part of FLVector object.
#'
#' \code{[]} acts on FLVector objects and extracts parts of them.
#'
#' 
#' @param pObj is a FLVector object
#' @param pSet is a vector representing the indices of elements to extract
#' @return \code{[]} returns FLVector object after extraction
#' which replicates the equivalent R extraction.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' WideTable <- FLTable(connection, "FL_TRAIN", "tblVectorWide","vector_key")
#' flvectorWide <- FLVector(WideTable,"vector_value")
#' resultFLVector <- flvectorWide[1:2]
#' DeepTable <- FLTable(connection, "FL_TRAIN", "tblVectorDeep","vector_id","vector_key","vector_value")
#' flvectorDeep <- FLVector(DeepTable,"vector_value",1)
#' resultFLVector <- flvectorDeep[1:2]
#' @export

`[.FLVector` <- function(pObj,pSet=1:length(pObj))
{
	flag3Check(pObj@table@odbc_connection)

	if(pObj@table@isDeep)
	{
		vTemp <- character(0)
		if(length(pSet)>1)
		{
		vTemp <- rep(paste0(",",pSet[2:length(pSet)]),length.out=length(pSet)-1)
		vTemp<-paste(vTemp,collapse=" ")
		}
		
		sqlstr <- paste0("SELECT * FROM ",pObj@table@db_name,".",pObj@table@table_name, 
                         " WHERE ",pObj@table@primary_key,"=",pObj@vector_id_value," AND ",pObj@table@var_id_name," IN(",pSet[1],vTemp,")")

		vRetObj<-sqlQuery(pObj@table@odbc_connection,sqlstr)
		vResultVec <- c()

		for(vIter in pSet)
		vResultVec <- append(vResultVec,vRetObj[(vRetObj[,pObj@table@var_id_name]==vIter),pObj@col_name])

		return(as.FLVector(vResultVec,pObj@table@odbc_connection))
	}

	if(!pObj@table@isDeep)
	{
		vTemp <- character(0)
		if(length(pSet)>1)
		{
		vTemp <- rep(paste0(",",pSet[2:length(pSet)]),length.out=length(pSet)-1)
		vTemp<-paste(vTemp,collapse=" ")
		}
		
		sqlstr <- paste0("SELECT * 
						 FROM ",pObj@table@db_name,".",pObj@table@table_name, 
                         " WHERE ",pObj@table@primary_key," IN(",pSet[1],vTemp,")")

		vRetObj<-sqlQuery(pObj@table@odbc_connection,sqlstr)
		vResultVec <- c()

		for(vIter in pSet)
		vResultVec <- append(vResultVec,vRetObj[(vRetObj[,pObj@table@primary_key]==vIter),pObj@col_name])

		return(as.FLVector(vResultVec,pObj@table@odbc_connection))
	}
}
