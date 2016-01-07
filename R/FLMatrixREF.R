#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

FLMatrixREF <- function (x, ...){
	UseMethod("FLMatrixREF", x)
}

#' Row Echelon form of a Matrix.
#'
#' \code{FLMatrixREF} gives the Row Echelon form of FLMatrix objects.
#'
#' The wrapper overloads FLMatrixREF and implicitly calls FLMatrixREFUdt.
#' @param object is of class FLMatrix
#' @section Constraints:
#' Input can only be a square FLMatrix with maximum dimension limitations of (1000 x 1000).
#' @return \code{FLMatrixREF} returns a FLMatrix object which is the Row Echelon form of input FLMatrix.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLMatrix <- FLMatrixREF(flmatrix)
#' @export


FLMatrixREF.FLMatrix<-function(object)
{

	connection<-getConnection(object)
	flag1Check(connection)

	sqlstr<-paste0(
				   viewSelectMatrix(object,"a",withName="z"),
                   outputSelectMatrix("FLMatrixREFUdt",viewName="z",
                   	localName="a",includeMID=TRUE,vconnection=connection)
                   )

	return(store(object=sqlstr,
              returnType="MATRIX",
              connection=connection))
}
