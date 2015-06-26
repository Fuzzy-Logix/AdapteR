#overloading cor
cor <- function (x,y, ...) {

	UseMethod("cor", x)
 }

#cor performs normally for data frames or vectors
cor.data.frame<-stats::cor
cor.numeric<-stats::cor

cor.FLVector<-function(x,y){
	sqlQuery(x@table@odbc_connection, paste("DATABASE",x@table@db_name))
	sqlQuery(x@table@odbc_connection,"SET ROLE ALL")
	sqlstr<-paste("SELECT ",x@table@db_name,".FLCorrel(a.",x@col_name,",b.",y@col_name,") FROM ",x@table@db_name,".", x@table@table_name," AS a,",x@table@db_name,".", y@table@table_name," AS b WHERE a.",x@table@primary_key,"=b.",y@table@primary_key, sep="")
	retobj<-sqlQuery(x@table@odbc_connection,sqlstr)
	retobj[1,1]
}

