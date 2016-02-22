#' @include utilities.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

#' Generalized Inverse of a Matrix.
#'
#' \code{ginv} computes the pseudo-inverse for FLMatrix objects.
#'
#' @param object is of class FLMatrix
#' @param ... any additional arguments
#' @section Constraints:
#' Input can only be with maximum dimension limitations of (500 x 500).
#' @return \code{ginv} returns a FLMatrix object which is the pseudo-inverse 
#' of input FLMatrix object and replicates the equivalent R output.
#' @examples
#' connection <- RODBC::odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_DEMO", 
#' "tblMatrixMulti", 1,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLMatrix <- ginv(flmatrix)
#' @export
ginv <- function (object, ...){
	UseMethod("ginv", object)
}

#' @export
ginv.default <- MASS::ginv

#' @export
ginv.FLMatrix<-function(object,...)
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
