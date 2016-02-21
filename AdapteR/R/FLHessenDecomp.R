#' @include utilities.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

#' Hessenberg Decomposition of a Matrix.
#'
#' \code{FLHessen} computes the Hessenberg decomposition for FLMatrix objects.
#'
#' @param x is of class FLMatrix
#' @section Constraints:
#' Input can only be square matrix with maximum dimension limitations of (700 x 700).
#' @return \code{FLHessen} returns a list of two components:
#'       \item{P}{FLMatrix representing P matrix obtained from Hessenberg decomposition}
#'       \item{H}{FLMatrix representing H matrix obtained from Hessenberg decomposition}
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_DEMO", "tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultList <- FLHessen(flmatrix)
#' resultList$P
#' resultList$H
##' @author Phani Srikar <phanisrikar93ume@gmail.com>
#' @export
FLHessen<-function(x, ...){
	UseMethod("FLHessen",x)
}

FLHessen.FLMatrix<-function(object)
{
	#checkSquare(object,"FLHessen")
	connection<-getConnection(object)
	flag1Check(connection)

	tempResultTable <- gen_unique_table_name("tblHessenResult")
	tempDecompTableVector <<- c(tempDecompTableVector,tempResultTable)

    sqlstr <- paste0("CREATE TABLE ",getRemoteTableName(getOption("ResultDatabaseFL"),tempResultTable)," AS(",
                     viewSelectMatrix(object, "a","z"),
                     outputSelectMatrix("FLHessenbergDecompUdt",viewName="z",localName="a",
                    	outColNames=list("OutputMatrixID","OutputRowNum",
                    		"OutputColNum","OutputPVal","OutputHVal"),
                    	whereClause=") WITH DATA;")
                   )

    sqlstr <- ensureQuerySize(pResult=sqlstr,
	            pInput=list(object),
	            pOperator="FLHessen")

    sqlSendUpdate(connection,sqlstr)

	PMatrix <- FLMatrix(
				       connection = connection, 
				       database = getOption("ResultDatabaseFL"), 
				       table_name = tempResultTable, 
					   matrix_id_value = "",
					   matrix_id_colname = "", 
					   row_id_colname = "OutputRowNum", 
					   col_id_colname = "OutputColNum", 
					   cell_val_colname = "OutputPVal",
					   whereconditions=paste0(getRemoteTableName(getOption("ResultDatabaseFL"),tempResultTable),".OutputPVal IS NOT NULL ")
					   )

	HMatrix <- FLMatrix(
				       connection = connection, 
				       database = getOption("ResultDatabaseFL"), 
				       table_name = tempResultTable, 
					   matrix_id_value = "",
					   matrix_id_colname = "", 
					   row_id_colname = "OutputRowNum", 
					   col_id_colname = "OutputColNum", 
					   cell_val_colname = "OutputHVal",
					   whereconditions=paste0(getRemoteTableName(getOption("ResultDatabaseFL"),tempResultTable),".OutputHVal IS NOT NULL ")
		             )

		result<-list(P = PMatrix,
					 H = HMatrix)
		result
}
