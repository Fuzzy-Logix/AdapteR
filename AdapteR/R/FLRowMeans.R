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
	var <- genRandVarName()

	sqlstr<-paste0( " SELECT '%insertIDhere%' AS vectorIdColumn ",#getMaxVectorId(connection),
			        ",",var,".rowIdColumn AS vectorIndexColumn",
			        ", AVG(",var,".valueColumn) AS vectorValueColumn 
					FROM ",
					"( ",constructSelect(object),
					" ) AS ",var,
					" GROUP BY ",var,".rowIdColumn")

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
				dimnames = list(1:nrow(object),
								"vectorValueColumn"),
				isDeep = FALSE)

	return(ensureQuerySize(pResult=flv,
	            pInput=list(object),
	            pOperator="rowMeans"))

}
