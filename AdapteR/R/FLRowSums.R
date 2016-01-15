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
	var <- genRandVarName()

	sqlstr<-paste0( " SELECT '%insertIDhere%' AS vectorIdColumn ",#getMaxVectorId(connection),
			        ",",var,".rowIdColumn AS vectorIndexColumn",
			        ", SUM(",var,".valueColumn) AS vectorValueColumn 
					FROM ",
					"( ",constructSelect(object),
					" ) AS ",var,
					" GROUP BY ",var,".rowIdColumn")

	tblfunqueryobj <- new("FLTableFunctionQuery",
                        odbc_connection = connection,
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

	flv <- new("FLVector",
				select = tblfunqueryobj,
				dimnames = list(1:nrow(object),
								"vectorValueColumn"),
				isDeep = FALSE)
	return(flv)
}
