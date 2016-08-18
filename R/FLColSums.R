#' @include FLMatrix.R
NULL

#' column sums of a FLMatrix.
#'
#' \code{colSums} computes the column-wise sums of FLMatrix objects.
#'
#' @param object is of class FLMatrix.
#' @return \code{colSums} returns a FLVector object representing the col-wise sums.
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLVector <- colSums(flmatrix)
#' @export
colSums <- function (object, ...){
  UseMethod("colSums", object)
}

#' @export
colSums.default <- base::colSums

#' @export
colSums.FLMatrix<-function(object,...)
{
	connection<-getConnection(object)
	flag3Check(connection)
	var <- genRandVarName()

	sqlstr<-paste0( " SELECT '%insertIDhere%' AS vectorIdColumn ",#getMaxVectorId(connection),
			        ",",var,".",object@dimColumns[[2]]," AS vectorIndexColumn",
			        ", SUM(",var,".",object@dimColumns[[3]],") AS vectorValueColumn 
					FROM ",
                   ## gk: use FLSum here!
					"( ",constructSelect(object), " ) AS ",var,
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
	            pOperator="colSums"))
}
