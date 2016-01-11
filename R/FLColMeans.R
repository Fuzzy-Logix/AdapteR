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

	var <- genRandVarName()

	sqlstr<-paste0( " SELECT '%insertIDhere%' AS vectorIdColumn ",#getMaxVectorId(connection),
			        ",",var,".colIdColumn AS vectorIndexColumn",
			        ", AVG(",var,".valueColumn) AS vectorValueColumn 
					FROM ",
					"( ",constructSelect(object),
					" ) AS ",var,
					" GROUP BY ",var,".colIdColumn")

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
								c("vectorIdColumn",
								  "vectorIndexColumn",
								  "vectorValueColumn")),
				isDeep = FALSE)
	return(flv)
	#return(store(object=flv))
}
