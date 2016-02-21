#' @include utilities.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLTable.R
#' @include FLIs.R
#' @include FLDims.R
#' @include FLPrint.R
NULL

#' Determinant of a Matrix.
#'
#' \code{det} computes the determinant of FLMatrix objects.
#'
#' @param x is a FLMatrix object
#' @section Constraints:
#' Input can only be a square matrix (n x n) with maximum dimension limitations
#' of (1000 x 1000).
#' @return \code{det} returns determinant as a R vector
#' which replicates the equivalent R vector output.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_DEMO", "tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLvector <- det(flmatrix)
##' @author Phani Srikar <phanisrikar93ume@gmail.com>
#' @export
det <- function (x, ...){
  UseMethod("det", x)
}

det.default <- base::det


det.FLMatrix<-function(object)
{
	connection<-getConnection(object)
	flag3Check(connection)

	sqlstr<-paste0(viewSelectMatrix(object, "a", withName="z"),
                   outputSelectMatrix("FLMatrixDetUdt", 
                   	viewName="z", 
                   	localName="a",
                   	outColNames=list("OutputDetVal"))
                   )
  sqlstr <- gsub("'%insertIDhere%'",1,sqlstr)
  sqlstr <- (ensureQuerySize(pResult=sqlstr,
            pInput=list(object),
            pOperator="det"))

	return(sqlQuery(connection,sqlstr)[[1]])
}
