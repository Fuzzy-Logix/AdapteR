#' @include FLVector.R
NULL
cor <- function (x,y, ...) {
	UseMethod("cor", x)
}
cor.default<-stats::cor
#' Correlation.
#'
#' \code{cor} computes correlation of FLVectors: x and y.
#'
#' The wrapper overloads cor and implicitly calls FLCorrel.
#' @method cor FLVector
#' @param x A numeric vector,matrix or data frame
#' @param y A vector,matrix or data frame with compatible dimensions to x
#' @section Constraints:
#' The number of non-null pairs must be greater than or equal to 2.
#' If number of non-null pairs is less than 2, FLCorrel returns a NULL.
#' @return \code{cor} returns correlation of x and y.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' table <- FLTable(connection, "FL_TRAIN", "tblAbaloneWide", "ObsID")
#' cor(table$Rings,table$Diameter)
#' @export

cor.FLMatrix <- function(x,y=x)
{
	if(is.FLMatrix(y))
	{
		if(nrow(x)==nrow(y))
		{
			vec <- c()
			k <- 1
			for(i in 1:ncol(x))
			for(j in 1:ncol(y))
			{
				sqlstr <- paste0("SELECT ",x@db_name,".FLCorrel(a.",x@cell_val_colname,",b.",y@cell_val_colname,") FROM ",
					             x@db_name,".", x@matrix_table," AS a INNER JOIN ",y@db_name,".",y@matrix_table," AS b ON a.",
					             x@row_id_colname," = b.",y@row_id_colname," WHERE a.",x@matrix_id_colname,"=",x@matrix_id_value,
					             " and b.",y@matrix_id_colname,"=",y@matrix_id_value," and a.",x@col_id_colname,"=",i," and b.",y@col_id_colname,"=",j)
				temp <- sqlQuery(x@odbc_connection,sqlstr)[1,1]
				vec[k] <- temp
				k <- k+1
			}
			correlmat <- matrix(vec,ncol(x),byrow=T)
			correlflmat<-as.FLMatrix(correlmat,x@odbc_connection);
			#print(correlflmat)
			#print(correlmat)
			return(correlflmat)

		}
		else stop("incompatible dimensions")
	}
	if(is.data.frame(y))
	{
		y <- as.matrix(y)
		if(is.numeric(y)) 
		{ 
			y<-as.FLMatrix(y,x@odbc_connection)
			return(cor(x,y))
		}
		else stop("only numeric entries for correlation")
	}
	if(is.vector(y))
	{
		if(nrow(x)!=length(y)) { stop(" invalid dimensions ") }
		else 
		{
			y <- matrix(y,length(y),1)
			y <- as.FLMatrix(y,x@odbc_connection)
			return(cor(x,y))
		}
	}
	if(is.matrix(y))
	{
		if(nrow(x)!=nrow(y)) { stop(" invalid dimensions ") }
		else
		{
			y <- as.FLMatrix(y,x@odbc_connection)
			return(cor(x,y))
		}
	}
	if(is.FLVector(y))
	{
		nrowy <- sqlQuery(y@table@odbc_connection, paste0("SELECT COUNT(a.",y@col_name,") FROM ",y@table@db_name,".",y@table@table_name," a"))
		if(nrowy == nrow(x))
		{
			vec <- c()
			k <- 1
			for(i in 1:ncol(x))
			{
				sqlQuery(x@odbc_connection,paste0("database ",x@db_name,"; set role all;"))
				sqlstr <- paste0("SELECT ",x@db_name,".FLCorrel(a.",x@cell_val_colname,",b.",y@col_name,") FROM ",
					             x@db_name,".", x@matrix_table," AS a INNER JOIN ",y@table@db_name,".",y@table@table_name," AS b ON a.",
					             x@row_id_colname," = b.",y@table@primary_key," WHERE a.",x@matrix_id_colname,"=",x@matrix_id_value,
					             " and a.",x@col_id_colname,"=",i)
				temp <- sqlQuery(x@odbc_connection,sqlstr)[1,1]
				vec[k] <- temp
				k <- k+1
			}
			correlmat <- matrix(vec,ncol(x),byrow=T)
			correlflmat<-as.FLMatrix(correlmat,x@odbc_connection);
			#print(correlflmat)
			#print(correlmat)
			return(correlflmat)

		}
		else stop("incompatible dimensions")
	}
}

cor.numeric <- function(x,y=x)
{
	if(is.FLMatrix(y))
	{
		res<-as.FLMatrix(t(as.matrix(cor(y,x))),y@odbc_connection)
		return (res)
	}
	else if(is.FLVector(y))
	{
		sqlQuery(y@table@odbc_connection, paste("DATABASE",y@table@db_name))
		sqlQuery(y@table@odbc_connection,"SET ROLE ALL")
		nrowy <- sqlQuery(y@table@odbc_connection, paste0("SELECT COUNT(a.",y@col_name,") FROM ",y@table@db_name,".",y@table@table_name," a"))[1,1]
		if(nrowy == length(x))
		{
			x <- matrix(x,nrowy)
			x <- as.FLMatrix(x,y@table@odbc_connection)
			return (cor(x,y))
		}
		else stop(" invalid dimensions ")
	}
	else
	{
		return(cor.default(x,y))
	}
}

cor.matrix <- function(x,y=x)
{
	if(is.FLMatrix(y))
	{
		res<-as.FLMatrix(t(as.matrix(cor(y,x))),y@odbc_connection)
		return (res)
	}
	else if(is.FLVector(y))
	{
		sqlQuery(y@table@odbc_connection, paste("DATABASE",y@table@db_name))
		sqlQuery(y@table@odbc_connection,"SET ROLE ALL")
		nrowy <- sqlQuery(y@table@odbc_connection, paste0("SELECT COUNT(a.",y@col_name,") FROM ",y@table@db_name,".",y@table@table_name," a"))[1,1]
		if(nrowy == nrow(x))
		{
			x <- as.FLMatrix(x,y@table@odbc_connection)
			return (cor(x,y))
		}
		else stop(" invalid dimensions ")
	}
	else
	{
		return(cor.default(x,y))
	}
}

cor.data.frame <- function(x,y=x)
{
	if(is.FLMatrix(y))
	{
		res<-as.FLMatrix(t(as.matrix(cor(y,x))),y@odbc_connection)
		return (res)
	}
	else if(is.FLVector(y))
	{
		sqlQuery(y@table@odbc_connection, paste("DATABASE",y@table@db_name))
		sqlQuery(y@table@odbc_connection,"SET ROLE ALL")
		nrowy <- sqlQuery(y@table@odbc_connection, paste0("SELECT COUNT(a.",y@col_name,") FROM ",y@table@db_name,".",y@table@table_name," a"))[1,1]
		if(nrowy == nrow(x))
		{
			x <- as.matrix(x)
			x <- as.FLMatrix(x,y@table@odbc_connection)
			return (cor(x,y))
		}
		else stop(" invalid dimensions ")
	}
	else
	{
		return(cor.default(x,y))
	}
}

cor.FLVector <- function(x,y=x)
{	
	sqlQuery(x@table@odbc_connection, paste("DATABASE",x@table@db_name))
	sqlQuery(x@table@odbc_connection,"SET ROLE ALL")
	nrowx <- sqlQuery(x@table@odbc_connection, paste0("SELECT COUNT(a.",x@col_name,") FROM ",x@table@db_name,".",x@table@table_name," a"))[1,1]

	if(is.FLVector(y))
	{
		nrowy <- sqlQuery(y@table@odbc_connection, paste0("SELECT COUNT(a.",y@col_name,") FROM ",y@table@db_name,".",y@table@table_name," a"))[1,1]
		if(nrowx == nrowy)
		{
			sqlstr<-paste("SELECT ",x@table@db_name,".FLCorrel(a.",x@col_name,",b.",y@col_name,") FROM ",x@table@db_name,".", x@table@table_name,
				" AS a,",x@table@db_name,".", y@table@table_name," AS b WHERE a.",x@table@primary_key,"=b.",y@table@primary_key, sep="")
			retobj<-sqlQuery(x@table@odbc_connection,sqlstr)
			return(retobj[1,1])
		}
		else stop(" invalid dimensions ")
	}
	if(is.FLMatrix(y))
	{
		res<-as.FLMatrix(t(as.matrix(cor(y,x))),x@table@odbc_connection)
			return (res)
	}
	if(is.matrix(y))
	{
		if(nrowx == nrow(y))
		{
			res<-as.FLMatrix(t(as.matrix(cor(y,x))),x@table@odbc_connection)
			return (res)
		}
		else stop(" invalid dimensions ")
	}
	if(is.vector(y))
	{
		if(nrowx == length(y))
		{
			res<-as.FLMatrix(t(as.matrix(cor(y,x))),x@table@odbc_connection)
			return (res)
		}
		else stop(" invalid dimensions ")
	}
	if(is.data.frame(y))
	{
		if(nrowx == nrow(y))
		{
			res<-as.FLMatrix(t(as.matrix(cor(y,x))),x@table@odbc_connection)
			return (res)
		}
		else stop(" invalid dimensions ")
	}
}