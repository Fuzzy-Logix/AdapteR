#' @include FLMatrix.R
NULL



#' Hessenberg Decomposition of a Matrix.
#'
#' \code{FLHessen} computes the Hessenberg decomposition for FLMatrix objects.
#'
#' @param object is of class FLMatrix
#' @param ... any additional arguments
#' @section Constraints:
#' Input can only be square matrix with maximum dimension limitations of (700 x 700).
#' @return \code{FLHessen} returns a list of two components:
#'       \item{P}{FLMatrix representing P matrix obtained from Hessenberg decomposition}
#'       \item{H}{FLMatrix representing H matrix obtained from Hessenberg decomposition}
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' flmatrix <- FLMatrix("FL_DEMO", 
#' "tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultList <- FLHessen(flmatrix)
#' resultList$P
#' resultList$H
#' @export
FLHessen<-function(object, ...){
	UseMethod("FLHessen",object)
}

#' @export
FLHessen.FLMatrix<-function(object,...)
{
	#checkSquare(object,"FLHessen")
	connection<-getConnection(object)
	flag1Check(connection)

	tempResultTable <- gen_unique_table_name("Hessen")

    sqlstr <- paste0(
                     viewSelectMatrix(object, "a","z"),
                     outputSelectMatrix("FLHessenbergDecompUdt",viewName="z",localName="a",
                    	outColNames=list("OutputMatrixID","OutputRowNum",
                    		"OutputColNum","OutputPVal","OutputHVal"),
                    	whereClause=" ")
                   )

    sqlstr <- gsub("'%insertIDhere%'",1,sqlstr)

    sqlstr <- ensureQuerySize(pResult=sqlstr,
	            pInput=list(object),
	            pOperator="FLHessen")

    createTable(pTableName=tempResultTable,
                pSelect=sqlstr)

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
