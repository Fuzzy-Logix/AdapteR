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

	sqlstr<-paste0(viewSelectMatrix(object, "a", withName="z"),
                   outputSelectMatrix("FLMatrixDetUdt", 
                   	viewName="z", 
                   	localName="a",
                   	outColNames=list("OutputDetVal"))
                   )
	sqlQuery(connection,sqlstr)[[1]]
}
