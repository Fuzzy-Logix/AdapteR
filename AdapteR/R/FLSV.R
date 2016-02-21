#' @include utilities.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

FLSV <- function (x, ...){
  UseMethod("FLSV", x)
}

#' Singular Values of a FLMatrix.
#'
#' \code{FLSV} computes the singular values for FLMatrix objects.
#'
#' The wrapper overloads FLSV and implicitly calls FLSVUdt.
#' @param object is of class FLMatrix
#' @section Constraints:
#' Input can only be a square matrix (n x n) with maximum dimension limitations
#' of (700 x 700).
#' @return \code{FLSV} returns a FLVector object representing the singular values.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLVector <- FLSV(flmatrix)
#' @export

FLSV.FLMatrix<-function(object)
{
	#checkSquare(object)
	connection<-getConnection(object)
	flag3Check(connection)

	sqlstr<-paste0(viewSelectMatrix(object,"a",withName="z"),
                   outputSelectMatrix("FLSVUdt",includeMID=FALSE,
                   	outColNames=list(vectorIdColumn="'%insertIDhere%'",
                                     vectorIndexColumn="OutputID",
                                     vectorValueColumn="OutputSV"),
                    viewName="z",localName="a",vconnection=connection)
                   )

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
              pOperator="FLSV",
              pStoreResult=TRUE))
}
