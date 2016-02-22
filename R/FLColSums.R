#' @include utilities.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

#' column sums of a FLMatrix.
#'
#' \code{colSums} computes the column-wise sums of FLMatrix objects.
#'
#' @param x is of class FLMatrix.
#' @return \code{colSums} returns a FLVector object representing the col-wise sums.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_DEMO", "tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLVector <- colSums(flmatrix)
#' @export
colSums <- function (x, ...){
  UseMethod("colSums", x)
}

colSums.default <- base::colSums

colSums.FLMatrix<-function(object)
{
	connection<-getConnection(object)
	flag3Check(connection)
	var <- genRandVarName()

	sqlstr<-paste0( " SELECT '%insertIDhere%' AS vectorIdColumn ",#getMaxVectorId(connection),
			        ",",var,".colIdColumn AS vectorIndexColumn",
			        ", SUM(",var,".valueColumn) AS vectorValueColumn 
					FROM ",
					"( ",constructSelect(object),
					" ) AS ",var,
					" GROUP BY ",var,".colIdColumn")

	tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = connection,
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

	flv <- new("FLVector",
				select = tblfunqueryobj,
				dimnames = list(1:ncol(object),
								"vectorValueColumn"),
				isDeep = FALSE)
	
	return(ensureQuerySize(pResult=flv,
	            pInput=list(object),
	            pOperator="colSums"))
}
