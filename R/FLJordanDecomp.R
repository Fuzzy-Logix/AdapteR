#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL


FLJordan<-function(x, ...){
	UseMethod("FLJordan",x)
}

#' Jordan Decomposition of a Matrix.
#'
#' \code{jordan} computes the Jordan decomposition for FLMatrix objects.
#'
#' The wrapper overloads jordan and implicitly calls FLJordanDecompUdt.
#' @param object is of class FLMatrix
#' @section Constraints:
#' Input can only be square matrix with maximum dimension limitations of (700 x 700).
#' @return \code{jordan} returns a list of two components:
#'       \item{J}{FLVector representing J vector obtained from Jordan decomposition}
#'       \item{P}{FLMatrix representing P matrix obtained from Jordan decomposition}
#'       \item{PInv}{FLMatrix representing PInv matrix obtained from Jordan decomposition}
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultList <- jordan(flmatrix)
#' resultList$J
#' resultList$P
#' resultList$PInv
#' @export

FLJordan.FLMatrix<-function(object)
{
	connection<-getConnection(object)
	flag1Check(connection)
	flag3Check(connection)
	
	tempResultTable <- gen_unique_table_name("tblJordonDecompResult")
	tempDecompTableVector <<- c(tempDecompTableVector,tempResultTable)

        sqlstr <- paste0("CREATE TABLE ",getRemoteTableName(getOption("ResultDatabaseFL"),tempResultTable)," AS(",
                         viewSelectMatrix(object, "a","z"),
                         outputSelectMatrix("FLJordanDecompUdt",viewName="z",localName="a",
                                            outColNames=list("OutputMatrixID","OutputRowNum",
                                                             "OutputColNum","OutPVal","OutJVal","OutPInvVal"),
                                            whereClause=") WITH DATA;")
                   )

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

	table <- FLTable(connection,
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
