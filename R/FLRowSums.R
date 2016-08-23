#' @include FLMatrix.R
NULL

#' row sums of a FLMatrix.
#'
#' \code{rowSums} computes the row-wise sums of FLMatrix objects.
#'
#' @param object is of class FLMatrix.
#' @param ... any additional arguments
#' @return \code{rowSums} returns a FLVector object representing the row-wise sums.
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' flmatrix <- FLMatrix("FL_DEMO.tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLVector <- rowSums(flmatrix)
#' @export
rowSums <- function (object, ...){
  UseMethod("rowSums", object)
}

#' @export
rowSums.default <- base::rowSums

#' @export
rowSums.FLMatrix<-function(object,...)
{
	connection<-getConnection(object)
	flag3Check(connection)
	var <- genRandVarName()

	sqlstr<-paste0( " SELECT '%insertIDhere%' AS vectorIdColumn ",#getMaxVectorId(connection),
			        ",",var,".",object@dimColumns[[1]]," AS vectorIndexColumn",
			        ", SUM(",var,".",object@dimColumns[[3]],") AS vectorValueColumn 
					FROM ",
					"( ",constructSelect(object),
					" ) AS ",var,
					" GROUP BY ",var,".",object@dimColumns[[1]])

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
	            pOperator="rowSums"))
}
