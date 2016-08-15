#' @include FLMatrix.R
NULL

#' column means of a FLMatrix.
#'
#' \code{colMeans} computes the column-wise average of FLMatrix objects.
#'
#' @param object is of class FLMatrix.
#' @param ... any additional arguments.
#' @return \code{colMeans} returns a FLVector object representing the column-wise Means.
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLVector <- colMeans(flmatrix)
#' @export
colMeans <- function (object, ...){
  UseMethod("colMeans", object)
}

#' @export
colMeans.default <- base::colMeans

#' @export
colMeans.FLMatrix<-function(object,...)
{
	connection<-getConnection(object)
	flag3Check(connection)

	var <- genRandVarName()

	sqlstr<-paste0( " SELECT '%insertIDhere%' AS vectorIdColumn ",#getMaxVectorId(connection),
			        ",",var,".",object@dimColumns[[2]]," AS vectorIndexColumn",
			        ", AVG(",var,".",object@dimColumns[[3]],") AS vectorValueColumn 
					FROM ",
					"( ",constructSelect(object),
					" ) AS ",var,
					" GROUP BY ",var,".",object@dimColumns[[2]])

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
	            pOperator="colMeans"))
}
