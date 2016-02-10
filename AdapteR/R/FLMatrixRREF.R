#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

FLMatrixRREF <- function (x, ...){
	UseMethod("FLMatrixRREF", x)
}

#' Reduced Row Echelon form of a Matrix.
#'
#' \code{FLMatrixRREF} gives the Reduced Row Echelon form of FLMatrix objects.
#'
#' The wrapper overloads FLMatrixRREF and implicitly calls FLMatrixRREFUdt.
#' @param object is of class FLMatrix
#' @section Constraints:
#' Input can only be a square FLMatrix with maximum dimension limitations of (1000 x 1000).
#' @return \code{FLMatrixRREF} returns a FLMatrix object which is the Reduced Row Echelon form of input FLMatrix.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLMatrix <- FLMatrixRREF(flmatrix)
#' @export

FLMatrixRREF.FLMatrix<-function(object)
{

	# checkSquare(object,"FLMatrixRREF")
	connection<-getConnection(object)
	flag1Check(connection)

	sqlstr<-paste0(viewSelectMatrix(object,"a",withName="z"),
                   outputSelectMatrix("FLMatrixRREFUdt",viewName="z",
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
            pOperator="FLMatrixRREF",
            pStoreResult=TRUE))
}
