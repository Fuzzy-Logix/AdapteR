#' @include FLMatrix.R
NULL

#' Determinant of a Matrix.
#'
#' \code{det} computes the determinant of FLMatrix objects.
#'
#' @param object is a FLMatrix object
#' @param ... any additional arguments
#' @section Constraints:
#' Input can only be a square matrix (n x n) with maximum dimension limitations
#' of (1000 x 1000).
#' @return \code{det} returns determinant as a R vector
#' which replicates the equivalent R vector output.
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' flmatrix <- FLMatrix("FL_DEMO.tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL",connection)
#' resultFLvector <- det(flmatrix)
#' @export
det <- function (object, ...){
  UseMethod("det", object)
}

#' @export
det.default <- base::det

#' @export
det.FLMatrix<-function(object,...)
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
