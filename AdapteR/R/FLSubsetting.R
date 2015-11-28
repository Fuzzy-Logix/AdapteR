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
#' @param rows is a vector input corresponding to rows to be extracted
#' @param cols is a vector input corresponding to columns to be extracted
#' @return \code{[]} returns FLMatrix object after extraction
#' which replicates the equivalent R extraction.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' resultFLmatrix <- flmatrix[1,]
#' @export

`[.FLMatrix`<-function(object,rows=1,cols=1)
{
	connection<-object@odbc_connection
	if(nargs()==2 && missing(rows)) { return(object[,]) }
	if(nargs()==2)
	{
		if(rows>nrow(object)*ncol(object)) { stop("subscript_out_of_bounds") }
		return(sqlQuery(connection,paste0(" SELECT ",object@cell_val_colname,
                                          " FROM ",remoteTable(object),
                                          " ORDER BY ",object@matrix_id_colname,",",object@col_id_colname,",",object@row_id_colname))[[1]][rows])
	}


    if(is.numeric(rows))
        newrownames <- object@dimnames[[1]][rows]
    else
        newrownames <- rows

    if(is.numeric(cols))
        newcolnames <- object@dimnames[[2]][cols]
    else
        newcolnames <- cols

    ##browser()
    if(missing(cols)) 
    {
        if (missing(rows)) 
        return(object)

        else return(FLMatrix(
                    connection = object@odbc_connection, 
                    database = object@db_name, 
                    matrix_table = object@matrix_table, 
                    matrix_id_value = object@matrix_id_value,
                    matrix_id_colname = object@matrix_id_colname, 
                    row_id_colname = object@row_id_colname, 
                    col_id_colname = object@col_id_colname, 
                    cell_val_colname = object@cell_val_colname,
                    dimnames = list(newrownames,
                                    object@dimnames[[2]]),
                    conditionDims=c(TRUE,FALSE)))
    }
    else { ## !missing(cols)
        if(missing(rows)) {
            return(FLMatrix(
                connection = object@odbc_connection, 
                database = object@db_name, 
                matrix_table = object@matrix_table, 
                matrix_id_value = object@matrix_id_value,
                matrix_id_colname = object@matrix_id_colname, 
                row_id_colname = object@row_id_colname, 
                col_id_colname = object@col_id_colname, 
                cell_val_colname = object@cell_val_colname, 
                dimnames = list(object@dimnames[[1]],
                                newcolnames),
                conditionDims=c(FALSE,TRUE)))
        } else {  ## !missing(cols) and !missing(rows)
            return(FLMatrix(
                connection = object@odbc_connection, 
                database = object@db_name, 
                matrix_table = object@matrix_table, 
                matrix_id_value = object@matrix_id_value,
                matrix_id_colname = object@matrix_id_colname, 
                row_id_colname = object@row_id_colname, 
                col_id_colname = object@col_id_colname, 
                cell_val_colname = object@cell_val_colname, 
                dimnames = list(newrownames,
                                newcolnames),
                conditionDims=c(TRUE,TRUE)))
        }
    }
}


## `[.FLMatrix`<-function(object,nrow=1,ncol=1)
## {
## 	connection<-object@odbc_connection
## 	if(nargs()==2 && missing(nrow)) { return(object[,]) }
## 	if(nargs()==2)
## 	{
## 		if(nrow>nrow(object)*ncol(object)) { stop("subscript_out_of_bounds") }
## 		return(sqlQuery(connection,paste0(" SELECT ",object@cell_val_colname,
## 											" FROM ",remoteTable(object),
## 											" ORDER BY ",object@matrix_id_colname,",",object@col_id_colname,",",object@row_id_colname))[[1]][nrow])
## 	}
## 	#if(is.character(nrow)){ nrow <- sapply(nrow, function(x) which(rownames(object) %in% x),USE.NAMES=FALSE) }  ## transform character index to numeric
## 	#if(is.character(ncol)){ ncol <- sapply(ncol, function(x) which(colnames(object) %in% x),USE.NAMES=FALSE) }
## 	#if(sum(abs(nrow) > nrow(object)) > 0 || sum(abs(ncol) > ncol(object)) > 0){ stop("subscript_out_of_bounds") }
## 	flag1Check(connection)
## 	if(length(nrow)>0 && length(ncol)>0)
## 	{
## 		if(length(nrow) == 1 && length(ncol) == 1)
## 		{
## 			if(missing(ncol))
## 			{
## 				if(missing(nrow))
## 				{
## 					sqlstr<-paste("INSERT INTO ",result_db_name,".",result_matrix_table,
## 								  " SELECT ",max_matrix_id_value,
## 								           ",",object@row_id_colname,
## 								           ",",object@col_id_colname,
## 								           ",",object@cell_val_colname,
## 								  " FROM ",remoteTable(object),
## 								  " WHERE ",object@matrix_id_colname," = ",object@matrix_id_value)
## 					sqlSendUpdate(connection,sqlstr)
## 					max_matrix_id_value <<- max_matrix_id_value + 1
## 			 		return(FLMatrix( 
## 			 			       connection = connection, 
## 			 			       database = result_db_name, 
## 			 			       matrix_table = result_matrix_table, 
## 			 	               matrix_id_value = max_matrix_id_value-1,
## 				               matrix_id_colname = "MATRIX_ID", 
## 				               row_id_colname = "ROW_ID", 
## 				               col_id_colname = "COL_ID", 
## 				               cell_val_colname = "CELL_VAL",
## 				               nrow = nrow(object), 
## 				               ncol = ncol(object), 
## 				               dimnames = object@dimnames))
## 				}
## 				else
## 				{
##                     newrownames <- object@dimnames[[1]][[nrow]]
## 					sqlstr<-paste(" INSERT INTO ",result_db_name,".",result_matrix_table,
## 								  " SELECT ",max_matrix_id_value,",",object@row_id_colname,",",object@col_id_colname,",",object@cell_val_colname,  
## 								  " FROM ",remoteTable(object),
## 								  " WHERE ",object@row_id_colname," IN (",
##                                   paste0("'",newrownames,"'",collapse= " "),
##                                   ")",
## 								  " AND ",object@matrix_id_colname," = ",object@matrix_id_value)
## 					sqlSendUpdate(connection,sqlstr)
## 					max_matrix_id_value <<- max_matrix_id_value + 1
## 			 		return(FLMatrix( 
## 			 			      connection = connection, 
## 			 			      database = result_db_name, 
## 			 			      matrix_table = result_matrix_table, 
## 			 	    	      matrix_id_value = max_matrix_id_value-1,
## 				              matrix_id_colname = "MATRIX_ID", 
## 				              row_id_colname = "ROW_ID", 
## 				              col_id_colname = "COL_ID", 
## 				              cell_val_colname = "CELL_VAL",
## 				              nrow = 1, 
## 				              ncol = ncol(object), 
## 				              dimnames = list(newrownames,object@dimnames[[2]])))				}
## 			    }
## 			else 
## 			{
## 				if(missing(nrow))
## 				{
## 					sqlstr<-paste(" INSERT INTO ",result_db_name,".",result_matrix_table,
## 								  " SELECT ",max_matrix_id_value,",",object@row_id_colname,",",object@col_id_colname,",",object@cell_val_colname,  
## 								  " FROM ",remoteTable(object),
## 								  " WHERE ",object@col_id_colname," = ",object@dimnames[[2]][[ncol]],
## 								  " AND ",object@matrix_id_colname," = ",object@matrix_id_value)
## 					sqlSendUpdate(connection,sqlstr)
## 					max_matrix_id_value <<- max_matrix_id_value + 1
## 			 		return(FLMatrix( 
## 			 			       connection = connection, 
## 			 			       database = result_db_name, 
## 			 			       matrix_table = result_matrix_table, 
## 			 	               matrix_id_value = max_matrix_id_value-1,
## 				               matrix_id_colname = "MATRIX_ID", 
## 				               row_id_colname = "ROW_ID", 
## 				               col_id_colname = "COL_ID", 
## 				               cell_val_colname = "CELL_VAL",
## 				               nrow = nrow(object), 
## 				               ncol = 1, 
## 				               dimnames = list(object@dimnames[[1]],object@dimnames[[2]][ncol])))
## 				}
## 				else
## 				{
## 					sqlstr<-paste(" INSERT INTO ",result_db_name,".",result_matrix_table,
## 								  " SELECT ",max_matrix_id_value,",",object@row_id_colname,",",object@col_id_colname,",",object@cell_val_colname,  
## 								  " FROM ",remoteTable(object),
## 								  " WHERE ",object@col_id_colname," = ",ncol,
## 								  " AND ",object@row_id_colname," = ",rownames[[nrow]],
## 								  " AND ",object@matrix_id_colname," = ",object@matrix_id_value)
## 					sqlSendUpdate(connection,sqlstr)
## 					max_matrix_id_value <<- max_matrix_id_value + 1
## 			 		return(FLMatrix( 
## 			 			      connection = connection, 
## 			 			      database = result_db_name, 
## 			 			      matrix_table = result_matrix_table, 
## 			 	              matrix_id_value = max_matrix_id_value-1,
## 				              matrix_id_colname = "MATRIX_ID", 
## 				              row_id_colname = "ROW_ID", 
## 				              col_id_colname = "COL_ID", 
## 				              cell_val_colname = "CELL_VAL",
## 				              nrow = 1, 
## 				              ncol = 1, 
## 				              dimnames = list(object@dimnames[[1]][nrow],object@dimnames[[2]][ncol])))
## 				}
## 			}
## 		}
## 		else
## 		{
## 			if(missing(ncol))
## 			{
## 				temp <- rep(paste0(" or ",object@row_id_colname," = ",nrow[2:length(nrow)]," "),length.out=length(nrow)-1)
## 				temp<-paste(temp,collapse=" ")
## 				sqlstr <- paste("SELECT ",object@matrix_id_colname," AS MATRIX_ID, ",object@row_id_colname," AS ROW_ID, ",object@col_id_colname," AS COL_ID, ",
## 								  object@cell_val_colname," AS CELL_VAL ",  
## 								  " FROM ",remoteTable(object),
## 							  " WHERE (",object@row_id_colname," = ",nrow[1],temp,
## 							  " ) AND ", object@matrix_id_colname," = ",object@matrix_id_value,
## 							  " ORDER BY 1,2,3;")
## 				retobj<-sqlQuery(connection,sqlstr)
## 				temp_df <- data.frame()
## 				for(i in nrow)
## 				temp_df <- rbind(temp_df,retobj[retobj[,object@row_id_colname]==i,])
## 				m <- matrix(temp_df$CELL_VAL,length(nrow),byrow=TRUE,dimnames=list(object@dimnames[[1]][nrow],object@dimnames[[2]]))
## 				return(as.FLMatrix(m,connection))
## 			}
## 			else if(missing(nrow))
## 			{
## 				temp <- rep(paste0("or ",object@col_id_colname," = ",ncol[2:length(ncol)]," "),length.out=length(ncol)-1)
## 				temp<-paste(temp,collapse=" ")
## 				sqlstr<-paste("SELECT ",object@matrix_id_colname," AS MATRIX_ID, ",object@row_id_colname," AS ROW_ID, ",object@col_id_colname," AS COL_ID, ",
## 								  object@cell_val_colname," AS CELL_VAL ",  
## 							  " FROM ",remoteTable(object),
## 							  "WHERE (",object@col_id_colname," = ",ncol[1],temp,
## 							  " ) AND ", object@matrix_id_colname," = ",object@matrix_id_value,
## 							  " ORDER BY 1,2,3;")
## 				retobj<-sqlQuery(connection,sqlstr)
## 				temp_df <- data.frame()
## 				for(i in ncol)
## 				temp_df <- rbind(temp_df,retobj[retobj[,object@col_id_colname]==i,])
## 				m <- matrix(temp_df$CELL_VAL,ncol=length(ncol),dimnames=list(object@dimnames[[1]],object@dimnames[[2]][ncol]))
## 				return(as.FLMatrix(m,connection))
## 			}
## 			else
## 			{
## 				temp1 <- paste("WHERE (",object@row_id_colname," = ",nrow[1])
## 				temp2 <- character(0)
## 				if(length(nrow)>1)
## 				{
## 					temp2 <- rep(paste0("or ",object@row_id_colname," = ",nrow[2:length(nrow)]," "),length.out=length(nrow)-1)
## 					temp2 <- paste(temp2,collapse=" ")
## 				}
## 				temp3 <- paste(" AND (",object@col_id_colname,"=",ncol[1])
## 				temp4 <- character(0)
## 				if(length(ncol)>1)
## 				{
## 					temp4 <- rep(paste0("or ",object@col_id_colname," = ",ncol[2:length(ncol)]," "),length.out=length(ncol)-1)
## 					temp4 <- paste(temp4,collapse=" ")
## 				}
## 				sqlstr<-paste("SELECT ",object@matrix_id_colname," AS MATRIX_ID, ",object@row_id_colname," AS ROW_ID, ",object@col_id_colname," AS COL_ID, ",
## 							   object@cell_val_colname," AS CELL_VAL ",  
## 							  " FROM ",remoteTable(object),
## 							   temp1,temp2,")",temp3,temp4,")",
## 							   " AND ",object@matrix_id_colname," = ",object@matrix_id_value,
## 							   " ORDER BY 1,2,3;")
## 				retobj<-sqlQuery(connection,sqlstr)
## 				temp_df <- data.frame()
## 				for(i in nrow)
## 				for(j in ncol)
## 				temp_df <- rbind(temp_df,retobj[((retobj[,object@row_id_colname]==i) & (retobj[,object@col_id_colname]==j)),])
## 				m <- matrix(temp_df$CELL_VAL,length(nrow),length(ncol),byrow=TRUE,dimnames=list(object@dimnames[[1]][nrow],object@dimnames[[2]][ncol]))
## 				return(as.FLMatrix(m,connection))
## 			}
## 		}
## 	}
## 	else stop("invalid_subscripts")
## }

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
		
		sqlstr <- paste0("SELECT * FROM ",remoteTable(pObj@table), 
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
						 FROM ",remoteTable(pObj@table), 
                         " WHERE ",pObj@table@primary_key," IN(",pSet[1],vTemp,")")

		vRetObj<-sqlQuery(pObj@table@odbc_connection,sqlstr)
		vResultVec <- c()

		for(vIter in pSet)
            vResultVec <- append(vResultVec,vRetObj[(vRetObj[,pObj@table@primary_key]==vIter),pObj@col_name])

		return(as.FLVector(vResultVec,pObj@table@odbc_connection))
	}
}
