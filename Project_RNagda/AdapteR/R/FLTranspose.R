#' @include utilities.R
#' @include FLMatrix.R
NULL
t<-function(x, ...){
	UseMethod("t", x)
}
#' Matrix Transpose.
#'
#' \code{t} returns the transpose of FLMatrix objects.
#'
#' The wrapper overloads t such that given a matrix or data frame of class FLMatrix, t returns the transpose of that object
#' @param table an object of class FLMatrix
#' @section Constraints:
#' Input can be a matrix of dimensions (m x n) where m > n, m < n or m = n.
#' @return \code{t} returns transpose which replicates the equivalent R output.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' table <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' t(table)
#' @export
t.FLMatrix<-function(object){
	connection<-object@odbc_connection
	sqlstr<-paste0("SELECT ",object@matrix_id_colname,", ",object@row_id_colname,", ",object@col_id_colname,", ",object@cell_val_colname," FROM  ",object@matrix_table," WHERE ",object@matrix_id_colname," = ",object@matrix_id_value,"")
	retobj<-sqlQuery(connection,sqlstr)
	nrow<-max(retobj$ROW_ID)
	ncol<-max(retobj$COL_ID)
	data<-matrix(retobj$CELL_VAL,nrow,ncol,byrow=TRUE)
	List<-list()
	if(nrow==ncol)
	test<-ncol
	else if(nrow>ncol)
	test<-nrow
	else
	test<-ncol
	if(test<-ncol){
		for(i in 1:test){
		newdata<-data[,i]
		List[[i]]<-newdata
		}
	transpose<-do.call(rbind, List)
	}
	else{
		for(i in 1:test){
		newdata<-data[i,]
		}
	transpose<-do.call(cbind, newdata)
	}
	transpose
}
