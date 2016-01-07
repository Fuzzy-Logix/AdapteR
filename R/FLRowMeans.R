#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

rowMeans <- function (x, ...){
  UseMethod("rowMeans", x)
}

rowMeans.default <- base::rowMeans


#' row means of a FLMatrix.
#'
#' \code{rowMeans} computes the row-wise average of FLMatrix objects.
#'
#' The wrapper overloads rowMeans and extends it to FLMatrix objects.
#' @param object is of class FLMatrix.
#' @return \code{rowMeans} returns a FLVector object representing the row-wise Means.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLVector <- rowMeans(flmatrix)
#' @export

rowMeans.FLMatrix<-function(object)
{
	connection<-getConnection(object)
	flag3Check(connection)

	sqlstr<-paste0(" SELECT ",getMaxVectorId(connection),
					         ",a.",getVariables(object)$rowIdColumn,
					         ",AVG(a.",getVariables(object)$valueColumn,")  
					FROM ",remoteTable(object)," a ",
					constructWhere(constraintsSQL(object,"a")),
					" GROUP BY a.",getVariables(object)$rowIdColumn)

	return(store(object=sqlstr,
              returnType="VECTOR",
              connection=connection))

}
