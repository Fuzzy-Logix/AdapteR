#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

rowSums <- function (x, ...){
  UseMethod("rowSums", x)
}


rowSums.default <- base::rowSums

#' row sums of a FLMatrix.
#'
#' \code{rowSums} computes the row-wise sums of FLMatrix objects.
#'
#' The wrapper overloads rowSums and extends it to FLMatrix objects.
#' @param object is of class FLMatrix.
#' @return \code{rowSums} returns a FLVector object representing the row-wise sums.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLVector <- rowSums(flmatrix)
#' @export

rowSums.FLMatrix<-function(object)
{
	connection<-getConnection(object)
	flag3Check(connection)

	sqlstr<-paste0(" SELECT ",getMaxVectorId(connection),
					         ",a.",getVariables(object)$rowIdColumn,
					         ",SUM(a.",getVariables(object)$valueColumn,") 
					FROM ",remoteTable(object)," a ",
					constructWhere(constraintsSQL(object,"a")),
					" GROUP BY a.",getVariables(object)$rowIdColumn)

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
