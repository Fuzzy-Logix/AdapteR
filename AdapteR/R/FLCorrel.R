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
    ##browser()
    if(is.FLMatrix(y))
    {
        sqlstr <- paste0(" SELECT ",
                         " a.",getVariables(x)$colIdColumn," AS A, ",
                         " b.",getVariables(y)$colIdColumn," AS B, ",
                         "FLCorrel(a.",getVariables(x)$valueColumn,
                         ",b.",getVariables(y)$valueColumn,") AS C ",
                         "FROM ",remoteTable(x)," a, ",
                         remoteTable(y)," b ",
                         constructWhere(c(
                             constraintsSQL(y, "a"),
                             constraintsSQL(x, "b"),
                             paste0("a.", getVariables(x)$rowIdColumn,
                                    " = b.",getVariables(y)$rowIdColumn))),
                         " GROUP BY a.",getVariables(x)$colIdColumn,
                         ", b.", getVariables(y)$colIdColumn,
                         " ORDER BY 1,2 ")
        ##browser()
        vec <- sqlQuery(getConnection(x),sqlstr)
        i <- match(vec[[2]],colnames(x))
        j <- match(vec[[1]],colnames(y))
        if(any(is.na(i)) | any(is.na(j)))
            stop("matrix rowname mapping needs to be implemented")
        m <- sparseMatrix(i = i,
                          j = j,
                          x = vec[[3]],
                          dims = c(ncol(x),
                                   ncol(y)),
                          dimnames = list(
                              colnames(x),
                              colnames(y)))
        return(m)
    }
    if(is.data.frame(y))
	{
		y <- as.matrix(y)
		if(is.numeric(y)) 
		{ 
			y<-as.FLMatrix(y,getConnection(x))
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
			y <- as.FLMatrix(y,getConnection(x))
			return(cor(x,y))
		}
	}
	if(is.matrix(y))
	{
		if(nrow(x)!=nrow(y)) { stop(" invalid dimensions ") }
		else
		{
			y <- as.FLMatrix(y,getConnection(x))
			return(cor(x,y))
		}
	}
	if(is.FLVector(y))
	{
		if(nrow(y) == nrow(x))
		{
			sqlstr <- paste0("SELECT a.",x@variables$colIdColumn,", ",
									 x@db_name,".FLCorrel(a.",x@variables$valueColumn,",
									 					  b.",y@col_name,") AS C 
							  FROM ",remoteTable(x)," a, 
							  	   ",remoteTable(y)," b 
							  WHERE a.",x@variables$rowIdColumn," = b.",y@obs_id_colname," 
							  AND a.",x@matrix_id_colname,"=",x@matrix_id_value," 
							  GROUP BY a.",x@variables$colIdColumn," 
							  ORDER BY 1")
			vec <- sqlQuery(getConnection(x),sqlstr)[,"C"]
			correlmat <- matrix(vec,ncol(x),byrow=T)
			correlflmat<-as.FLMatrix(correlmat,getConnection(x));
			#print(correlflmat)
			#print(correlmat)
			return(correlflmat)

		}
		else stop("incompatible dimensions")
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

cor.numeric <- function(x,y=x)
{
	if(is.FLMatrix(y))
	{
		res<- t(cor(y,x))
		return (res)
	}
	else if(is.FLVector(y))
	{
		nrowy <- sqlQuery(getConnection(y), 
						  paste0("SELECT COUNT(a.",y@col_name,") 
						  		  FROM ",remoteTable(y)," a"))[1,1]
		if(nrowy == length(x))
		{
			x <- matrix(x,nrowy)
			x <- as.FLMatrix(x,getConnection(y))
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
		nrowy <- sqlQuery(getConnection(y),
						  paste0("SELECT COUNT(a.",y@col_name,") 
						  		  FROM ",remoteTable(y)," a"))[1,1]
		if(nrowy == nrow(x))
		{
			x <- as.FLMatrix(x,getConnection(y))
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
		nrowy <- sqlQuery(getConnection(y),
						  paste0("SELECT COUNT(a.",y@col_name,") 
						  		  FROM ",remoteTable(y)," a"))[1,1]
		if(nrowy == nrow(x))
		{
			x <- as.matrix(x)
			x <- as.FLMatrix(x,getConnection(y))
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
	nrowx <- sqlQuery(getConnection(x),
					  paste0("SELECT COUNT(a.",x@col_name,") 
					  		  FROM ",remoteTable(x)," a"))[1,1]

	if(is.FLVector(y))
	{
		nrowy <- sqlQuery(getConnection(y),
						  paste0("SELECT COUNT(a.",y@col_name,") 
						  		  FROM ",remoteTable(y)," a"))[1,1]
		if(nrowx == nrowy)
		{
			sqlstr<-paste("SELECT ",x@db_name,".FLCorrel(a.",x@col_name,",
															   b.",y@col_name,") 
						   FROM ",remoteTable(x)," AS a,",
						   		  remoteTable(x)," AS b 
						   WHERE a.",x@obs_id_colname,"=b.",y@obs_id_colname, sep="")
			retobj<-sqlQuery(getConnection(x),sqlstr)
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
	nrowx <- nrow(x)
	ncolx <- ncol(x)
	if(is.FLTable(y))
	{
		if(nrowx == nrow(y))
		{
			if(y@isDeep && x@isDeep)
			{
				sqlstr <- paste0("SELECT a.",x@var_id_name," AS A ,
										 b.",y@var_id_name," AS B ,",
										 x@db_name,".FLCorrel(a.",x@variables$valueColumn,",
										 					  b.",y@variables$valueColumn,") AS C 
								  FROM ",remoteTable(x)," a,
								  	   ",remoteTable(y)," b 
								  WHERE  a.",x@obs_id_colname," = b.",y@obs_id_colname," 
								  GROUP BY a.",x@var_id_name,", b.",y@var_id_name," 
								  ORDER BY 1,2 ")
			
				vec <- sqlQuery(getConnection(x),sqlstr)[,"C"]
				correlmat <- matrix(vec,ncolx,byrow=T)
				#correlflmat<-as.FLMatrix(correlmat,getConnection(x));
				return(correlmat)
			}
			if(!y@isDeep && !x@isDeep)
			{
				deeptablenamex <- gen_deep_table_name(x@table_name)
				deeptablenamey <- gen_deep_table_name(y@table_name)
				
				sqlstr<-paste0("CALL FLWideToDeep('",x@table_name,"','",x@obs_id_colname,"','",deeptablenamex,
								"','ObsID','VarID','Num_Val',NULL,NULL,NULL,AnalysisID);")
				dataprepIDx <- as.vector(retobj<-sqlQuery(getConnection(x),sqlstr)[1,1])

				sqlstr<-paste0("CALL FLWideToDeep('",y@table_name,"','",y@obs_id_colname,"','",deeptablenamey,
								"','ObsID','VarID','Num_Val',NULL,NULL,NULL,AnalysisID);")
				dataprepIDy <- as.vector(retobj<-sqlQuery(getConnection(y),sqlstr)[1,1])

				x <- new("FLTable", 
						  odbc_connection = getConnection(x),
						  db_name = x@db_name, 
						  table_name = deeptablenamex,
						  primary_key = "ObsID",
						  var_id_name = "VarID",
						  cell_val_colname="Num_Val",
						  isDeep = TRUE)
				y <- new("FLTable", 
						  odbc_connection = getConnection(y),
						  db_name = y@db_name, 
						  table_name = deeptablenamey,
						  primary_key = "ObsID",
						  var_id_name = "VarID",
						  cell_val_colname="Num_Val",
						  isDeep = TRUE)
				
				sqlstr <- paste0("SELECT a.",x@var_id_name," AS A ,
										 b.",y@var_id_name," AS B ,
										   ",x@db_name,".FLCorrel(a.",x@variables$valueColumn,",
										   						  b.",y@variables$valueColumn,") AS C 
								  FROM ",remoteTable(x)," a, 
								  	   ",remoteTable(y)," b 
								  WHERE  a.",x@obs_id_colname," = b.",y@obs_id_colname," 
								  GROUP BY a.",x@var_id_name,", b.",y@var_id_name," 
								  ORDER BY 1,2 ")
				vec <- sqlQuery(getConnection(x),sqlstr)[,"C"]

				ncolx <- sqlQuery(getConnection(x),
								  paste0(" SELECT COUNT(COLUMN_NAME) 
								  		   FROM fzzlRegrDataPrepMap 
								  		   WHERE AnalysisID = '",dataprepIDx,"' 
					                	   AND Final_VarID IS NOT NULL"))[1,1]
				varnamesx <- sqlQuery(getConnection(x),
									  paste0(" SELECT COLUMN_NAME 
									  		   FROM fzzlRegrDataPrepMap 
									  		   WHERE AnalysisID = '",dataprepIDx,"' 
					                		   AND Final_VarID IS NOT NULL 
					                		   ORDER BY Final_VarID"))[,1]
				varnamesy <- sqlQuery(getConnection(y),
									  paste0(" 	SELECT COLUMN_NAME 
									  			FROM fzzlRegrDataPrepMap 
									  			WHERE AnalysisID = '",dataprepIDy,"' 
					                			AND Final_VarID IS NOT NULL 
					                			ORDER BY Final_VarID"))[,1]
				correlmat <- matrix(vec,ncolx,byrow=T,dimnames=list(varnamesx,varnamesy))
				return(correlmat)
			}
			if(y@isDeep && !x@isDeep)
			{
				deeptablenamex <- gen_deep_table_name(x@table_name)
				sqlstr<-paste0("CALL FLWideToDeep('",x@table_name,"','",x@obs_id_colname,"','",deeptablenamex,
								"','ObsID','VarID','Num_Val',NULL,NULL,NULL,AnalysisID);")
				dataprepIDx <- as.vector(retobj<-sqlQuery(getConnection(x),sqlstr)[1,1])
				x <- new("FLTable", 
						  odbc_connection = getConnection(x),
						  db_name = x@db_name, 
						  table_name = deeptablenamex,
						  primary_key = "ObsID",
						  var_id_name = "VarID",
						  cell_val_colname="Num_Val",
						  isDeep = TRUE)
				sqlstr <- paste0("SELECT a.",x@var_id_name," AS A ,
										 b.",y@var_id_name," AS B ,
										   ",x@db_name,".FLCorrel(a.",x@variables$valueColumn,",b.",y@variables$valueColumn,") AS C 
								  FROM ",remoteTable(x)," a,
								  	   ",remoteTable(y)," b 
								  WHERE  a.",x@obs_id_colname," = b.",y@obs_id_colname," 
								  GROUP BY a.",x@var_id_name,", b.",y@var_id_name," 
								  ORDER BY 1,2 ")
				vec <- sqlQuery(getConnection(x),sqlstr)[,"C"]
				varnamesx <- sqlQuery(getConnection(x), 
									  paste0(" 	SELECT COLUMN_NAME 
									  			FROM fzzlRegrDataPrepMap 
									  			WHERE AnalysisID = '",dataprepIDx,"' 
					                			AND Final_VarID IS NOT NULL 
					                			ORDER BY Final_VarID"))[,1]
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
		if(nrowx == nrow(y))
		{
			if(x@isDeep)
			{
				sqlstr <- paste0("SELECT a.",x@var_id_name," AS A ,
										 b.",y@variables$colIdColumn," AS B ,
										   ",x@db_name,".FLCorrel(a.",x@variables$valueColumn,",b.",y@variables$valueColumn,") AS C 
								  FROM ",remoteTable(x)," a,
								  	   ",remoteTable(y)," b 
								  WHERE a.",x@obs_id_colname," = b.",y@variables$rowIdColumn," 
								  AND b.",y@matrix_id_colname,"=",y@matrix_id_value," 
								  GROUP BY a.",x@var_id_name,", b.",y@variables$colIdColumn," 
								  ORDER BY 1,2 ")
			
				vec <- sqlQuery(getConnection(x),sqlstr)[,"C"]
				correlmat <- matrix(vec,ncol(x),byrow=T)
				return(correlmat)
			}
			if(!x@isDeep)
			{
				deeptablenamex <- gen_deep_table_name(x@table_name)
				sqlstr<-paste0("CALL FLWideToDeep('",x@table_name,"','",x@obs_id_colname,"','",deeptablenamex,
								"','ObsID','VarID','Num_Val',NULL,NULL,NULL,AnalysisID);")
				dataprepIDx <- as.vector(retobj<-sqlQuery(getConnection(x),sqlstr)[1,1])
				x <- new("FLTable", 
					      odbc_connection = getConnection(x),
					      db_name = x@db_name, 
						  table_name = deeptablenamex,
						  primary_key = "ObsID",
						  var_id_name = "VarID",
						  cell_val_colname="Num_Val",
						  isDeep = TRUE)
				sqlstr <- paste0("SELECT a.",x@var_id_name," AS A,
										 b.",y@variables$colIdColumn," AS B,
										   ",x@db_name,".FLCorrel(a.",x@variables$valueColumn,",b.",y@variables$valueColumn,") AS C 
								  FROM ",remoteTable(x)," a,
								  	   ",remoteTable(y)," b 
								  WHERE a.",x@obs_id_colname," = b.",y@variables$rowIdColumn," 
								  AND b.",y@matrix_id_colname,"=",y@matrix_id_value," 
								  GROUP BY a.",x@var_id_name,", b.",y@variables$colIdColumn," 
								  ORDER BY 1,2 ")
				vec <- sqlQuery(getConnection(x),sqlstr)[,"C"]
				varnamesx <- sqlQuery(getConnection(x),
									  paste0(" 	SELECT COLUMN_NAME 
									  			FROM fzzlRegrDataPrepMap 
									  			WHERE AnalysisID = '",dataprepIDx,"' 
					                			AND Final_VarID IS NOT NULL 
					                			ORDER BY Final_VarID"))[,1]
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
				sqlstr <- paste0("SELECT a.",x@var_id_name,", 
										   ",x@db_name,".FLCorrel(a.",x@variables$valueColumn,",
										   						  b.",y@col_name,") AS C 
								  FROM ",remoteTable(x)," a, 
								  	   ",remoteTable(y)," b 
								  WHERE a.",x@obs_id_colname," = b.",y@obs_id_colname," 
								  GROUP BY a.",x@var_id_name," 
								  ORDER BY 1")
				vec <- sqlQuery(getConnection(x),sqlstr)[,"C"]
				correlmat <- matrix(vec,ncol(x),byrow=T)
				return(correlmat)
			}
			if(!x@isDeep)
			{
				deeptablenamex <- gen_deep_table_name(x@table_name)
				sqlstr<-paste0("CALL FLWideToDeep('",x@table_name,"','",x@obs_id_colname,"','",deeptablenamex,
								"','ObsID','VarID','Num_Val',NULL,NULL,NULL,AnalysisID);")
				dataprepIDx <- as.vector(retobj<-sqlQuery(getConnection(x),sqlstr)[1,1])
				x <- new("FLTable", 
						  odbc_connection = getConnection(x),
						  db_name = x@db_name, 
						  table_name = deeptablenamex,
						  primary_key = "ObsID",
						  var_id_name = "VarID",
						  cell_val_colname="Num_Val",
						  isDeep = TRUE)
				sqlstr <- paste0("SELECT a.",x@var_id_name,", 
										   ",x@db_name,".FLCorrel(a.",x@variables$valueColumn,",b.",y@col_name,") AS C 
								  FROM ",remoteTable(x)," a, 
								  	   ",remoteTable(y)," b 
								  WHERE a.",x@obs_id_colname," = b.",y@obs_id_colname," 
								  GROUP BY a.",x@var_id_name," 
								  ORDER BY 1")
				vec <- sqlQuery(getConnection(x),sqlstr)[,"C"]
				varnamesx <- sqlQuery(getConnection(x),
									  paste0("  SELECT COLUMN_NAME 
									  			FROM fzzlRegrDataPrepMap 
									  			WHERE AnalysisID = '",dataprepIDx,"' 
						                		AND Final_VarID IS NOT NULL 
						                		ORDER BY Final_VarID"))[,1]
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
			y <- as.FLMatrix(y,getConnection(x))
			return (cor(x,y))
		}
		else stop(" incompatible dimensions ")
	}
	if(is.numeric(y))
	{
		if(nrowx == length(y))
		{
			y <- as.FLMatrix(as.matrix(y),getConnection(x))
			return (cor(x,y))
		}
		else stop(" incompatible dimensions ")
	}
	if(is.data.frame(y))
	{
		if(nrowx == nrow(y))
		{
			y <- as.FLMatrix(as.matrix(y),getConnection(x))
			return (cor(x,y))
		}
		else stop(" incompatible dimensions ")
	}
}
