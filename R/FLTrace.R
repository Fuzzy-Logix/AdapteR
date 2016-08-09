#' @include FLMatrix.R
NULL

#' Matrix Trace.
#'
#' \code{tr} computes the trace of FLMatrix objects.
#'
#' \code{tr} computes the trace of input FLMatrix object, stores the result
#' in-database and returns R vector object
#' @param object an object of class FLMatrix
#' @param ... any additional arguments
#' @return \code{tr} returns R Vector object of size 1 which replicates the equivalent R output.
#' @section Constraints:
#' Input can only be with maximum dimension limitations
#' of (1000 x 1000).
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' flmatrix <- FLMatrix("FL_DEMO.tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLVector <- tr(flmatrix)
#' @export

tr<-function(object, ...){
	UseMethod("tr", object)
}

#' @export
tr.default <- psych::tr

#' @export
tr.FLMatrix<-function(object,...){
	connection<-getConnection(object)
	
	flag3Check(connection)

	sqlstr<-paste0( " SELECT 
					  FLMatrixTrace(",getVariables(object)$rowId,
			         			   ",",getVariables(object)$colId,
			              		   ",",getVariables(object)$value,")",
				    " FROM ",tableAndAlias(object),
				    constructWhere(c(constraintsSQL(object),
				    	paste0(getVariables(object)$rowId," <= ",min(nrow(object),ncol(object))),
				    	paste0(getVariables(object)$colId, " <= ", min(nrow(object),ncol(object))))))

	sqlstr <- gsub("'%insertIDhere%'",1,sqlstr)
	sqlstr <- ensureQuerySize(pResult=sqlstr,
            pInput=list(object),
            pOperator="tr")
	return(sqlQuery(connection,sqlstr)[1,1])
}
