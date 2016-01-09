#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

colMeans <- function (x, ...){
  UseMethod("colMeans", x)
}


colMeans.default <- base::colMeans

#' column means of a FLMatrix.
#'
#' \code{colMeans} computes the column-wise average of FLMatrix objects.
#'
#' The wrapper overloads colMeans and extends it to FLMatrix objects.
#' @param object is of class FLMatrix.
#' @return \code{colMeans} returns a FLVector object representing the column-wise Means.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLVector <- colMeans(flmatrix)
#' @export

colMeans.FLMatrix<-function(object)
{
	connection<-getConnection(object)
	flag3Check(connection)

	sqlstr<-paste0( " SELECT ",getMaxVectorId(connection),
			        ",a.",getVariables(object)$colIdColumn,
			        ", AVG(a.",getVariables(object)$valueColumn,") 
					FROM ",remoteTable(object)," a ",
                       constructWhere(constraintsSQL(object,"a")),
					" GROUP BY a.",getVariables(object)$colIdColumn)

	tblfunqueryobj <- new("FLTableFunctionQuery",
                        odbc_connection = connection,
                        variables = list(
			                obs_id_colname = "VECTOR_INDEX",
			                cell_val_colname = "VECTOR_VALUE"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

	flv <- new("FLVector",
				select = tblfunqueryobj,
				dimnames = list(1:nrow(object),
								c("VECTOR_ID",
								  "VECTOR_INDEX",
								  "VECTOR_VALUE")),
				isDeep = FALSE)

	return(store(object=flv))
}
