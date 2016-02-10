#' @include utilities.R
#' @include FLMatrix.R
#' @include FLIs.R
#' @include FLDims.R
#' @include FLPrint.R
NULL

solve <- function (x, ...){
	UseMethod("solve", x)
}
#solve.default <- base::solve
# do not define solve.default in this package as it is already defined in base::solve.default.
# It might lead to stack overflow.

#' Inverse of a Matrix.
#'
#' \code{solve} computes the inverse for FLMatrix objects.
#'
#' The wrapper overloads solve and implicitly calls FLMatrixInvUdt.
#' @param object is of class FLMatrix
#' @section Constraints:
#' Input can only be a square matrix (n x n) with maximum dimension limitations
#' of (1000 x 1000).
#' @return \code{solve} returns a FLMatrix object which is the inverse of input FLMatrix object
#' and replicates the equivalent R output.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' resultFLMatrix <- solve(flmatrix)
#' @export
solve.FLMatrix <- function(pObj1)
{
    ## checkSquare(object,"solve")
	## checkSingularity(object)

	connection <- getConnection(pObj1)

	flag1Check(connection)

  sqlstr<-paste0(viewSelectMatrix(pObj1, "a", withName="z"),
                 outputSelectMatrix("FLMatrixInvUdt", viewName="z", localName="a",
                                    includeMID=TRUE,vconnection=connection)
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
            dimnames=dimnames(pObj1))

  return(ensureQuerySize(pResult=flm,
            pInput=list(pObj1),
            pOperator="solve",
            pStoreResult=TRUE))

}


