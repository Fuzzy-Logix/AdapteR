#overloading kmeans
cor <- function (x,y, ...) {

	UseMethod("cor", x)
 }

#kmeans performs normally for data frames or vectors
cor.data.frame<-stats::cor
cor.numeric<-stats::cor

cor.FLVector<-function(x,y){
	sqlstr<-paste0("SELECT FLCorrel (a.",x@col_name,",b.",y@col_name,") FROM ", x@table@table_name," AS a,", y@table@table_name," AS b")
	retobj<-sqlQuery(x@table@odbc_connection,sqlstr)
	retobj
}
