#' @include utilities.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLTable.R
#' @include FLIs.R
#' @include FLDims.R
#' @include FLPrint.R
NULL

det <- function (x, ...){
  UseMethod("det", x)
}

det.default <- base::det

#' Determinant of a Matrix.
#'
#' \code{det} computes the determinant of FLMatrix objects.
#'
#' The wrapper overloads det and implicitly calls FLMatrixDetUdt.
#' @param object is a FLMatrix object
#' @section Constraints:
#' Input can only be a square matrix (n x n) with maximum dimension limitations
#' of (1000 x 1000).
#' @return \code{det} returns determinant stored in-database as FLVector 
#' which replicates the equivalent R vector output.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' resultFLvector <- det(flmatrix)
#' @export

det.FLMatrix<-function(object)
{
	connection<-getConnection(object)
	flag3Check(connection)
	if(nrow(object) != ncol(object))
	{
		stop("input object must be square matrix")
	}
	sqlstr<-paste0("WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) AS
(SELECT a.",object@matrix_id_colname,", 
							   a.",object@row_id_colname,", 
							   a.",object@col_id_colname,",
							   a.",object@cell_val_colname," 
						FROM  ",remoteTable(object)," a 
						WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,")
SELECT ",max_vector_id_value,
", a.*
FROM TABLE (
FLMatrixDetUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val)
HASH BY z.Matrix_ID
LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID
) AS a
 ");

	sqlQuery(connection,sqlstr)[[3]]
}
