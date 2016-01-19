#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

#' Matrix Trace.
#'
#' \code{tr} computes the trace of FLMatrix objects.
#'
#' \code{tr} computes the trace of input FLMatrix object, stores the result
#' in-database and returns FLVector object
#' @param x an object of class FLMatrix
#' @return \code{tr} returns FLVector object of size 1 which replicates the equivalent R output.
#' @section Constraints:
#' Input can only be with maximum dimension limitations
#' of (1000 x 1000).
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' resultFLVector <- tr(flmatrix)
#' @export

tr<-function(x, ...){
	UseMethod("tr", x)
}

tr.default <- psych::tr

tr.FLMatrix<-function(object){
	connection<-getConnection(object)
	
	flag3Check(connection)

	sqlstr<-paste0( " SELECT 
					  FLMatrixTrace(",getVariables(object)$rowId,
			         			   ",",getVariables(object)$colId,
			              		   ",",getVariables(object)$value,")",
				    " FROM ",remoteTable(object),
				    constructWhere(c(constraintsSQL(object),
				    	paste0(getVariables(object)$rowId," <= ",min(nrow(object),ncol(object))),
				    	paste0(getVariables(object)$colId, " <= ", min(nrow(object),ncol(object))))))

	sqlstr <- gsub("'%insertIDhere%'",1,sqlstr)
	sqlstr <- ensureQuerySize(pResult=sqlstr,
            pInput=list(object),
            pOperator="tr")
	return(sqlQuery(connection,sqlstr)[1,1])
}
