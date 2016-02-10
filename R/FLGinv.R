#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

ginv <- function (x, ...){
	UseMethod("ginv", x)
}

ginv.default <- MASS::ginv

#' Generalized Inverse of a Matrix.
#'
#' \code{ginv} computes the pseudo-inverse for FLMatrix objects.
#'
#' The wrapper overloads ginv and implicitly calls FLMatrixPseudoInvUdt.
#' @param object is of class FLMatrix
#' @section Constraints:
#' Input can only be with maximum dimension limitations of (500 x 500).
#' @return \code{ginv} returns a FLMatrix object which is the pseudo-inverse 
#' of input FLMatrix object and replicates the equivalent R output.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 1)
#' resultFLMatrix <- ginv(flmatrix)
#' @export

ginv.FLMatrix<-function(object)
{

	connection<-getConnection(object)
	flag1Check(connection)

	sqlstr<-paste0(viewSelectMatrix(object,"a",withName="z"),
                   outputSelectMatrix("FLMatrixPseudoInvUdt",viewName="z",
                        localName="a",includeMID=TRUE,vconnection=connection)
                   )

  tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = connection,
                        variables=list(
                            rowIdColumn="OutputRowNum",
                            colIdColumn="OutputColNum",
                            valueColumn="OutputVal"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

  flm <- new("FLMatrix",
            select= tblfunqueryobj,
            dimnames=dimnames(object))

  return(ensureQuerySize(pResult=flm,
            pInput=list(object),
            pOperator="ginv",
            pStoreResult=TRUE))

}
