#' @include utilities.R
#' @include FLMatrix.R
#' @include FLVector.R
NULL

#' Matrix Rank.
#'
#' \code{rankMatrix} computes the rank of FLMatrix objects.
#'
#' @param object is of class FLMatrix
#' @param ... any additional arguments
#' @return \code{rankMatrix} returns R vector object of size 1 which replicates the equivalent R output.
#' @section Constraints:
#' Input can have maximum dimension limitations of (1000 x 1000).
#' @examples
#' connection <- RODBC::odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_DEMO", 
#' "tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLVector <- rankMatrix(flmatrix)
#' @export
rankMatrix<-function(object, ...){
	UseMethod("rankMatrix", object)
}

#' @export
rankMatrix.default <- Matrix::rankMatrix

#' @export
rankMatrix.FLMatrix<-function(object,...)
{
	connection<-getConnection(object)

	sqlstr<-paste0(viewSelectMatrix(object,"a",withName="z"),
				   outputSelectMatrix("FLMatrixRankUdt",
				   					outColNames=list("OutputMtxRank"),viewName="z",localName="a")
					)
	sqlstr <- gsub("'%insertIDhere%'",1,sqlstr)

	sqlstr <- (ensureQuerySize(pResult=sqlstr,
            pInput=list(object),
            pOperator="rankMatrix"))

	return(sqlQuery(connection,sqlstr)$"OutputMtxRank"[1])
}
