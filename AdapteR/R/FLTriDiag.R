#' @include utilities.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

FLTriDiag <- function (x, ...){
	UseMethod("FLTriDiag", x)
}

#' TriDiagonal or Upper Hessenberg matrix of a FLMatrix.
#'
#' \code{FLTriDiag} computes the TriDiagonal or Upper Hessenberg matrix of FLMatrix object.
#'
#' The wrapper overloads FLTriDiag and implicitly calls FLTriDiagUdt.
#' @param object is of class FLMatrix
#' @section Constraints:
#' Input can only be a square matrix (n x n) with maximum dimension limitations
#' of (700 x 700).
#' @return \code{FLTriDiag} returns a FLMatrix object representing the upper Hessenberg or TriDiagonal matrix.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLMatrix <- FLTriDiag(flmatrix)
#' @export

FLTriDiag.FLMatrix<-function(object)
{

	connection<-getConnection(object)
	flag1Check(connection)

	sqlstr<-paste0(viewSelectMatrix(object,"a",withName="z"),
                   outputSelectMatrix("FLTridiagUdt",viewName="z",
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
            pOperator="FLTriDiag",
            pStoreResult=TRUE))
}
