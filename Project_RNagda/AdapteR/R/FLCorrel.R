#' @include FLVector.R
NULL
cor <- function (x,y, ...) {
	UseMethod("cor", x)
}
cor.data.frame<-stats::cor
cor.numeric<-stats::cor
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
cor.FLVector<-function(x,y){
	sqlQuery(x@table@odbc_connection, paste("DATABASE",x@table@db_name))
	sqlQuery(x@table@odbc_connection,"SET ROLE ALL")
	sqlstr<-paste("SELECT ",x@table@db_name,".FLCorrel(a.",x@col_name,",b.",y@col_name,") FROM ",x@table@db_name,".", x@table@table_name," AS a,",x@table@db_name,".", y@table@table_name," AS b WHERE a.",x@table@primary_key,"=b.",y@table@primary_key, sep="")
	retobj<-sqlQuery(x@table@odbc_connection,sqlstr)
	retobj[1,1]
}

