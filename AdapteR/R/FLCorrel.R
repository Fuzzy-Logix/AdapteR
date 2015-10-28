#' @include FLVector.R
#' @include FLMatrix.R
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
#' cor(table,table)
#' @export

cor.FLMatrix <- function(x,y=x)
{
	if(is.FLMatrix(y))
	{
		if(x@nrow==y@nrow)
		{
			sqlstr <- paste0("SELECT a.",x@col_id_colname," AS A ,b.",y@col_id_colname," AS B ,",x@db_name,".FLCorrel(a.",x@cell_val_colname,",b.",
							y@cell_val_colname,") AS C FROM ",x@db_name,".", x@matrix_table," a, ",y@db_name,".",y@matrix_table," b WHERE  a.",
					        x@row_id_colname," = b.",y@row_id_colname," and a.",x@matrix_id_colname,"=",x@matrix_id_value,
					        " and b.",y@matrix_id_colname,"=",y@matrix_id_value," GROUP BY a.",x@col_id_colname,", b.",y@col_id_colname," ORDER BY 1,2 ")
			
			vec <- sqlQuery(x@odbc_connection,sqlstr)[,"C"]
			correlflmat<-as.FLMatrix(matrix(vec,ncol(x),byrow=T,dimnames=list(x@dimnames[[2]],y@dimnames[[2]])),x@odbc_connection);
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
		if(x@nrow!=length(y)) { stop(" invalid dimensions ") }
		else 
		{
			y <- matrix(y,length(y),1)
			y <- as.FLMatrix(y,x@odbc_connection)
			return(cor(x,y))
		}
	}
	if(is.matrix(y))
	{
		if(x@nrow!=nrow(y)) { stop(" invalid dimensions ") }
		else
		{
			y <- as.FLMatrix(y,x@odbc_connection)
			return(cor(x,y))
		}
	}
	if(is.FLVector(y))
	{
		if(nrow(y) == x@nrow)
		{
			sqlQuery(x@odbc_connection,paste0("database ",x@db_name,"; set role all;"))
			sqlstr <- paste0("SELECT a.",x@col_id_colname,", ",x@db_name,".FLCorrel(a.",x@cell_val_colname,",b.",y@col_name,") AS C FROM ",
					             x@db_name,".", x@matrix_table," a, ",y@table@db_name,".",y@table@table_name," b WHERE a.",
					             x@row_id_colname," = b.",y@table@primary_key," and a.",x@matrix_id_colname,"=",x@matrix_id_value,
					             " GROUP BY a.",x@col_id_colname," ORDER BY 1")
			vec <- sqlQuery(x@odbc_connection,sqlstr)[,"C"]
			correlmat <- matrix(vec,ncol(x),byrow=T)
			correlflmat<-as.FLMatrix(correlmat,x@odbc_connection);
			#print(correlflmat)
			#print(correlmat)
			return(correlflmat)

		}
		else stop("incompatible dimensions")
	}
	if(is.FLTable(y))
	{
		if(x@nrow!=nrow(y)) { stop(" invalid dimensions ") }
		else 
		{
			return(t(cor(y,x)))
		}
	}
}

cor.numeric <- function(x,y=x)
{
	if(is.FLMatrix(y))
	{
		res<- t(cor(y,x))
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
	else if(is.FLTable(y))
	{
		if(length(x)!=nrow(y)) { stop(" invalid dimensions ") }
		else 
			{
				return(t(cor(y,x)))
			}
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
		res<- t(cor(y,x))
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
	else if(is.FLTable(y))
	{
		if(nrow(x)!=nrow(y)) { stop(" invalid dimensions ") }
		else 
		{
			return(t(cor(y,x)))
		}
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
		res<- t(cor(y,x))
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
	else if(is.FLTable(y))
	{
		if(nrow(x)!=nrow(y)) { stop(" invalid dimensions ") }
		else 
		{
			return(t(cor(y,x)))
		}
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
		res<- t(cor(y,x))
			return (res)
	}
	if(is.matrix(y))
	{
		if(nrowx == nrow(y))
		{
			res<- t(cor(y,x))
			return (res)
		}
		else stop(" invalid dimensions ")
	}
	if(is.vector(y))
	{
		if(nrowx == length(y))
		{
			res<- t(cor(y,x))
			return (res)
		}
		else stop(" invalid dimensions ")
	}
	if(is.data.frame(y))
	{
		if(nrowx == nrow(y))
		{
			res<- t(cor(y,x))
			return (res)
		}
		else stop(" invalid dimensions ")
	}
	if(is.FLTable(y))
	{
		if(nrow(x)!=nrow(y)) { stop(" invalid dimensions ") }
		else 
		{
			return(t(cor(y,x)))
		}
	}
}

cor.FLTable <- function(x,y=x)
{
	sqlQuery(x@odbc_connection, paste0(" DATABASE ",x@db_name,"; SET ROLE ALL;"))
	nrowx <- nrow(x)
	ncolx <- ncol(x)
	if(is.FLTable(y))
	{
		if(nrowx == nrow(y))
		{
			if(y@isDeep && x@isDeep)
			{
				sqlstr <- paste0("SELECT a.",x@var_id_name," AS A ,b.",y@var_id_name," AS B ,",x@db_name,".FLCorrel(a.",x@num_val_name,",b.",
							y@num_val_name,") AS C FROM ",x@db_name,".", x@table_name," a, ",y@db_name,".",y@table_name," b WHERE  a.",
					        x@primary_key," = b.",y@primary_key," GROUP BY a.",x@var_id_name,", b.",y@var_id_name," ORDER BY 1,2 ")
			
				vec <- sqlQuery(x@odbc_connection,sqlstr)[,"C"]
				correlmat <- matrix(vec,ncolx,byrow=T)
				#correlflmat<-as.FLMatrix(correlmat,x@odbc_connection);
				return(correlmat)
			}
			if(!y@isDeep && !x@isDeep)
			{
				deeptablenamex <- gen_deep_table_name(x@table_name)
				deeptablenamey <- gen_deep_table_name(y@table_name)
				
				sqlstr<-paste0("CALL FLWideToDeep('",x@table_name,"','",x@primary_key,"','",deeptablenamex,
								"','ObsID','VarID','Num_Val',NULL,NULL,NULL,AnalysisID);")
				dataprepIDx <- as.vector(retobj<-sqlQuery(x@odbc_connection,sqlstr)[1,1])

				sqlstr<-paste0("CALL FLWideToDeep('",y@table_name,"','",y@primary_key,"','",deeptablenamey,
								"','ObsID','VarID','Num_Val',NULL,NULL,NULL,AnalysisID);")
				dataprepIDy <- as.vector(retobj<-sqlQuery(y@odbc_connection,sqlstr)[1,1])

				x <- new("FLTable", odbc_connection = x@odbc_connection,db_name = x@db_name, 
							table_name = deeptablenamex,primary_key = "ObsID",var_id_name = "VarID",num_val_name="Num_Val",isDeep = TRUE)
				y <- new("FLTable", odbc_connection = y@odbc_connection,db_name = y@db_name, 
							table_name = deeptablenamey,primary_key = "ObsID",var_id_name = "VarID",num_val_name="Num_Val",isDeep = TRUE)
				
				sqlstr <- paste0("SELECT a.",x@var_id_name," AS A ,b.",y@var_id_name," AS B ,",x@db_name,".FLCorrel(a.",x@num_val_name,",b.",
							y@num_val_name,") AS C FROM ",x@db_name,".", x@table_name," a, ",y@db_name,".",y@table_name," b WHERE  a.",
					        x@primary_key," = b.",y@primary_key," GROUP BY a.",x@var_id_name,", b.",y@var_id_name," ORDER BY 1,2 ")
				vec <- sqlQuery(x@odbc_connection,sqlstr)[,"C"]

				ncolx <- sqlQuery(x@odbc_connection, paste0(" SELECT  COUNT(COLUMN_NAME) FROM fzzlRegrDataPrepMap WHERE AnalysisID = '",dataprepIDx,"' 
					                and Final_VarID IS NOT NULL"))[1,1]
				varnamesx <- sqlQuery(x@odbc_connection, paste0(" SELECT  COLUMN_NAME FROM fzzlRegrDataPrepMap WHERE AnalysisID = '",dataprepIDx,"' 
					                and Final_VarID IS NOT NULL ORDER BY Final_VarID"))[,1]
				varnamesy <- sqlQuery(y@odbc_connection, paste0(" SELECT  COLUMN_NAME FROM fzzlRegrDataPrepMap WHERE AnalysisID = '",dataprepIDy,"' 
					                and Final_VarID IS NOT NULL ORDER BY Final_VarID"))[,1]
				correlmat <- matrix(vec,ncolx,byrow=T,dimnames=list(varnamesx,varnamesy))
				return(correlmat)
			}
			if(y@isDeep && !x@isDeep)
			{
				deeptablenamex <- gen_deep_table_name(x@table_name)
				sqlstr<-paste0("CALL FLWideToDeep('",x@table_name,"','",x@primary_key,"','",deeptablenamex,
								"','ObsID','VarID','Num_Val',NULL,NULL,NULL,AnalysisID);")
				dataprepIDx <- as.vector(retobj<-sqlQuery(x@odbc_connection,sqlstr)[1,1])
				x <- new("FLTable", odbc_connection = x@odbc_connection,db_name = x@db_name, 
							table_name = deeptablenamex,primary_key = "ObsID",var_id_name = "VarID",num_val_name="Num_Val",isDeep = TRUE)
				sqlstr <- paste0("SELECT a.",x@var_id_name," AS A ,b.",y@var_id_name," AS B ,",x@db_name,".FLCorrel(a.",x@num_val_name,",b.",
							y@num_val_name,") AS C FROM ",x@db_name,".", x@table_name," a, ",y@db_name,".",y@table_name," b WHERE  a.",
					        x@primary_key," = b.",y@primary_key," GROUP BY a.",x@var_id_name,", b.",y@var_id_name," ORDER BY 1,2 ")
				vec <- sqlQuery(x@odbc_connection,sqlstr)[,"C"]
				varnamesx <- sqlQuery(x@odbc_connection, paste0(" SELECT  COLUMN_NAME FROM fzzlRegrDataPrepMap WHERE AnalysisID = '",dataprepIDx,"' 
					                and Final_VarID IS NOT NULL ORDER BY Final_VarID"))[,1]
				correlmat <- matrix(vec,ncol(x),byrow=T,dimnames=list(varnamesx,c()))
				return(correlmat)
			}
			if(!y@isDeep && x@isDeep)
			{
				return ( t(cor(y,x)))
			}
		else stop(" incompatible dimensions ")
		}
	}
	
	if(is.FLMatrix(y))
	{
		if(nrowx == y@nrow)
		{
			if(x@isDeep)
			{
				sqlstr <- paste0("SELECT a.",x@var_id_name," AS A ,b.",y@col_id_colname," AS B ,",x@db_name,".FLCorrel(a.",x@num_val_name,",b.",
							y@cell_val_colname,") AS C FROM ",x@db_name,".", x@table_name," a, ",y@db_name,".",y@matrix_table," b WHERE a.",
					        x@primary_key," = b.",y@row_id_colname," and b.",y@matrix_id_colname,"=",y@matrix_id_value,
					        " GROUP BY a.",x@var_id_name,", b.",y@col_id_colname," ORDER BY 1,2 ")
			
				vec <- sqlQuery(x@odbc_connection,sqlstr)[,"C"]
				correlmat <- matrix(vec,ncol(x),byrow=T)
				return(correlmat)
			}
			if(!x@isDeep)
			{
				deeptablenamex <- gen_deep_table_name(x@table_name)
				sqlstr<-paste0("CALL FLWideToDeep('",x@table_name,"','",x@primary_key,"','",deeptablenamex,
								"','ObsID','VarID','Num_Val',NULL,NULL,NULL,AnalysisID);")
				dataprepIDx <- as.vector(retobj<-sqlQuery(x@odbc_connection,sqlstr)[1,1])
				x <- new("FLTable", odbc_connection = x@odbc_connection,db_name = x@db_name, 
							table_name = deeptablenamex,primary_key = "ObsID",var_id_name = "VarID",num_val_name="Num_Val",isDeep = TRUE)
				sqlstr <- paste0("SELECT a.",x@var_id_name," AS A ,b.",y@col_id_colname," AS B ,",x@db_name,".FLCorrel(a.",x@num_val_name,",b.",
							y@cell_val_colname,") AS C FROM ",x@db_name,".", x@table_name," a, ",y@db_name,".",y@matrix_table," b WHERE a.",
					        x@primary_key," = b.",y@row_id_colname," and b.",y@matrix_id_colname,"=",y@matrix_id_value,
					        " GROUP BY a.",x@var_id_name,", b.",y@col_id_colname," ORDER BY 1,2 ")
				vec <- sqlQuery(x@odbc_connection,sqlstr)[,"C"]
				varnamesx <- sqlQuery(x@odbc_connection, paste0(" SELECT  COLUMN_NAME FROM fzzlRegrDataPrepMap WHERE AnalysisID = '",dataprepIDx,"' 
					                and Final_VarID IS NOT NULL ORDER BY Final_VarID"))[,1]
				correlmat <- matrix(vec,ncol(x),byrow=T,dimnames=list(varnamesx,c()))
				return(correlmat)
			}
		}
		else stop(" incompatible dimensions ")
	}

	if(is.FLVector(y))
	{
		if(nrowx == nrow(y))
		{
			if(x@isDeep)
			{
				sqlQuery(x@odbc_connection,paste0("database ",x@db_name,"; set role all;"))
				sqlstr <- paste0("SELECT a.",x@var_id_name,", ",x@db_name,".FLCorrel(a.",x@num_val_name,",b.",y@col_name,") AS C FROM ",
						             x@db_name,".", x@table_name," a, ",y@table@db_name,".",y@table@table_name," b WHERE a.",
						             x@primary_key," = b.",y@table@primary_key," GROUP BY a.",x@var_id_name," ORDER BY 1")
				vec <- sqlQuery(x@odbc_connection,sqlstr)[,"C"]
				correlmat <- matrix(vec,ncol(x),byrow=T)
				return(correlmat)
			}
			if(!x@isDeep)
			{
				deeptablenamex <- gen_deep_table_name(x@table_name)
				sqlstr<-paste0("CALL FLWideToDeep('",x@table_name,"','",x@primary_key,"','",deeptablenamex,
								"','ObsID','VarID','Num_Val',NULL,NULL,NULL,AnalysisID);")
				dataprepIDx <- as.vector(retobj<-sqlQuery(x@odbc_connection,sqlstr)[1,1])
				x <- new("FLTable", odbc_connection = x@odbc_connection,db_name = x@db_name, 
						table_name = deeptablenamex,primary_key = "ObsID",var_id_name = "VarID",num_val_name="Num_Val",isDeep = TRUE)
				sqlstr <- paste0("SELECT a.",x@var_id_name,", ",x@db_name,".FLCorrel(a.",x@num_val_name,",b.",y@col_name,") AS C FROM ",
						             x@db_name,".", x@table_name," a, ",y@table@db_name,".",y@table@table_name," b WHERE a.",
						             x@primary_key," = b.",y@table@primary_key," GROUP BY a.",x@var_id_name," ORDER BY 1")
				vec <- sqlQuery(x@odbc_connection,sqlstr)[,"C"]
				varnamesx <- sqlQuery(x@odbc_connection, paste0(" SELECT  COLUMN_NAME FROM fzzlRegrDataPrepMap WHERE AnalysisID = '",dataprepIDx,"' 
						                and Final_VarID IS NOT NULL ORDER BY Final_VarID"))[,1]
				correlmat <- matrix(vec,ncol(x),byrow=T,dimnames=list(varnamesx,c()))
				return(correlmat)
			}
		}
		else stop(" incompatible dimensions ")
	}
	if(is.matrix(y))
	{
		if(nrowx == nrow(y))
		{
			y <- as.FLMatrix(y,x@odbc_connection)
			return (cor(x,y))
		}
		else stop(" incompatible dimensions ")
	}
	if(is.numeric(y))
	{
		if(nrowx == length(y))
		{
			y <- as.FLMatrix(as.matrix(y),x@odbc_connection)
			return (cor(x,y))
		}
		else stop(" incompatible dimensions ")
	}
	if(is.data.frame(y))
	{
		if(nrowx == nrow(y))
		{
			y <- as.FLMatrix(as.matrix(y),x@odbc_connection)
			return (cor(x,y))
		}
		else stop(" incompatible dimensions ")
	}
}
