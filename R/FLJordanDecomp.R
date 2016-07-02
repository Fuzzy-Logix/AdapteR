#' @include utilities.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

#' Jordan Decomposition of a Matrix.
#'
#' \code{FLJordan} computes the Jordan decomposition for FLMatrix objects.
#'
#' @param object is of class FLMatrix
#' @param ... any additional arguments
#' @section Constraints:
#' Input can only be square matrix with maximum dimension limitations of (700 x 700).
#' @return \code{FLJordan} returns a list of two components:
#'       \item{J}{FLVector representing J vector obtained from Jordan decomposition}
#'       \item{P}{FLMatrix representing P matrix obtained from Jordan decomposition}
#'       \item{PInv}{FLMatrix representing PInv matrix obtained from Jordan decomposition}
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' flmatrix <- FLMatrix("FL_DEMO", 
#' "tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultList <- FLJordan(flmatrix)
#' resultList$J
#' resultList$P
#' resultList$PInv
#' @export
FLJordan<-function(object, ...){
	UseMethod("FLJordan",object)
}

#' @export
FLJordan.FLMatrix<-function(object,...)
{
	connection<-getConnection(object)
	flag1Check(connection)
	flag3Check(connection)
	
	tempResultTable <- gen_unique_table_name("Jordon")

        sqlstr <- paste0("CREATE TABLE ",getRemoteTableName(getOption("ResultDatabaseFL"),tempResultTable)," AS(",
                         viewSelectMatrix(object, "a","z"),
                         outputSelectMatrix("FLJordanDecompUdt",viewName="z",localName="a",
                                            outColNames=list("OutputMatrixID","OutputRowNum",
                                                             "OutputColNum","OutPVal","OutJVal","OutPInvVal"),
                                            whereClause=") WITH DATA;")
                   )
        sqlstr <- gsub("'%insertIDhere%'",1,sqlstr)

    sqlstr <- ensureQuerySize(pResult=sqlstr,
	            pInput=list(object),
	            pOperator="FLJordan")

    sqlSendUpdate(connection,sqlstr)

    PMatrix <- FLMatrix(
				       connection = connection, 
				       database = getOption("ResultDatabaseFL"), 
				       table_name = tempResultTable, 
					   matrix_id_value = "",
					   matrix_id_colname = "", 
					   row_id_colname = "OutputRowNum", 
					   col_id_colname = "OutputColNum", 
					   cell_val_colname = "OutPVal",
					   whereconditions=paste0(getRemoteTableName(getOption("ResultDatabaseFL"),tempResultTable),".OutPVal IS NOT NULL "))


    PInvMatrix <- FLMatrix(
			            connection = connection, 
			            database = getOption("ResultDatabaseFL"), 
			            table_name = tempResultTable, 
			            matrix_id_value = "",
			            matrix_id_colname = "", 
			            row_id_colname = "OutputRowNum", 
			            col_id_colname = "OutputColNum", 
			            cell_val_colname = "OutPInvVal",
			            whereconditions=paste0(getRemoteTableName(getOption("ResultDatabaseFL"),tempResultTable),".OutPInvVal IS NOT NULL "))

	table <- FLTable(
		             getOption("ResultDatabaseFL"),
		             tempResultTable,
		             "OutputRowNum",
		             whereconditions=c(paste0(getRemoteTableName(getOption("ResultDatabaseFL"),tempResultTable),".OutJVal IS NOT NULL "),
		             	paste0(getRemoteTableName(getOption("ResultDatabaseFL"),tempResultTable),".OutputRowNum = ",
		             	getRemoteTableName(getOption("ResultDatabaseFL"),tempResultTable),".OutputColNum "))
		             )

	JVector <- table[,"OutJVal"]

	result<-list(J = JVector,
				 P = PMatrix,
				 PInv = PInvMatrix)
	result
}
