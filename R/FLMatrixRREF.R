#' @include FLMatrix.R
NULL

#' Reduced Row Echelon form of a Matrix.
#'
#' \code{FLMatrixRREF} gives the Reduced Row Echelon form of FLMatrix objects.
#'
#' @param object is of class FLMatrix
#' @param ... any additional arguments
#' @section Constraints:
#' Input can only be a square FLMatrix with maximum dimension limitations of (1000 x 1000).
#' @return \code{FLMatrixRREF} returns a FLMatrix object which is the Reduced Row Echelon form of input FLMatrix.
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLMatrix <- FLMatrixRREF(flmatrix)
#' @export
FLMatrixRREF <- function (object, ...){
	UseMethod("FLMatrixRREF", object)
}

#' @export
FLMatrixRREF.FLMatrix<-function(object,...)
{

	# connection<-getFLConnection(object)
 #    ## flag1Check(connection)

	# sqlstr<-paste0(viewSelectMatrix(object,"a",withName="z"),
 #                   outputSelectMatrix("FLMatrixRREFUdt",viewName="z",
 #                   	localName="a",includeMID=TRUE,vconnection=connection)
 #                   )

	# tblfunqueryobj <- new("FLTableFunctionQuery",
 #                        connectionName = attr(connection,"name"),
 #                        variables=list(
 #                            rowIdColumn="OutputRowNum",
 #                            colIdColumn="OutputColNum",
 #                            valueColumn="OutputVal"),
 #                        whereconditions="",
 #                        order = "",
 #                        SQLquery=sqlstr)

 #  	flm <- newFLMatrix(
 #            select= tblfunqueryobj,
 #            dims=dim(object),
 #            Dimnames=dimnames(object))
    
    flm <- constructMatrixUDTSQL(pObject=object,
                                 pFuncName="FLMatrixRREFUdt",
                                 pdims=getDimsSlot(object),
                                 pdimnames=dimnames(object)
                                 )

  	return(ensureQuerySize(pResult=flm,
            pInput=list(object),
            pOperator="FLMatrixRREF",
            pStoreResult=TRUE))
}
