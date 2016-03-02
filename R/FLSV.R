#' @include utilities.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

#' Singular Values of a FLMatrix.
#'
#' \code{FLSV} computes the singular values for FLMatrix objects.
#'
#' @param object is of class FLMatrix
#' @param ... any additional arguments
#' @section Constraints:
#' Input can only be a square matrix (n x n) with maximum dimension limitations
#' of (700 x 700).
#' @return \code{FLSV} returns a FLVector object representing the singular values.
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' flmatrix <- FLMatrix("FL_DEMO", 
#' "tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLVector <- FLSV(flmatrix)
#' @export
FLSV <- function (object, ...){
  UseMethod("FLSV", object)
}

#' @export
FLSV.FLMatrix<-function(object,...)
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
