#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

colSums <- function (x, ...){
  UseMethod("colSums", x)
}

colSums.default <- base::colSums

#' column sums of a FLMatrix.
#'
#' \code{colSums} computes the column-wise sums of FLMatrix objects.
#'
#' The wrapper overloads colSums and extends it to FLMatrix objects.
#' @param object is of class FLMatrix.
#' @return \code{colSums} returns a FLVector object representing the col-wise sums.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLVector <- colSums(flmatrix)
#' @export

colSums.FLMatrix<-function(object)
{
	connection<-getConnection(object)
	flag3Check(connection)

	sqlstr<-paste0(" SELECT ",getMaxVectorId(connection),
					         ",a.",getVariables(object)$colId,
					         ",SUM(a.",getVariables(object)$value,") 
					FROM ",remoteTable(object)," a ",
					constructWhere(constraintsSQL(object,"a")),
					" GROUP BY a.",getVariables(object)$colId)

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
