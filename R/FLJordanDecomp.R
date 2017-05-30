#' @include FLMatrix.R
NULL

#' Jordan Decomposition of a Matrix.
#'
#' \code{FLJordan} computes the Jordan decomposition for FLMatrix objects.
#'
#' @param object is of class FLMatrix
#' @param ... any additional arguments
#' @section Constraints:
#' Input can only be square matrix with maximum dimension limitations of (700 x 700).
#' If the input matrix have repeated eigenvalues, then FLJordan function may give incorrect results.
#' @return \code{FLJordan} returns a list of two components:
#'       \item{J}{FLVector representing J vector obtained from Jordan decomposition}
#'       \item{P}{FLMatrix representing P matrix obtained from Jordan decomposition}
#'       \item{PInv}{FLMatrix representing PInv matrix obtained from Jordan decomposition}
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultList <- FLJordan(flmatrix)
#' resultList$J
#' resultList$P
#' resultList$PInv
#' @seealso Refer to \code{\link{FLMatrix}}, \code{\link{FLVector}} to know more about 
#' FLMatrix and FLVector.
#' @export
FLJordan<-function(object, ...){
	UseMethod("FLJordan",object)
}

#' @export
FLJordan.FLMatrix<-function(object,...)
{
	connection<-getFLConnection(object)
	## flag1Check(connection)
	## flag3Check(connection)
	

        # sqlstr <- paste0(
        #                  viewSelectMatrix(object, "a","z"),
        #                  outputSelectMatrix("FLJordanDecompUdt",viewName="z",localName="a",
        #                                     outColNames=list("OutputMatrixID","OutputRowNum",
        #                                                      "OutputColNum","OutPVal","OutJVal","OutPInvVal"),
        #                                     whereClause="")
        #            )
    
    sqlstr <- constructMatrixUDTSQL(pObject=object,
                                 pFuncName="FLJordanDecompUdt",
                                 pdims=getDimsSlot(object),
                                 pdimnames=dimnames(object),
                                 pReturnQuery=TRUE
                                 )
    sqlstr <- gsub("'%insertIDhere%'",1,sqlstr)

    sqlstr <- ensureQuerySize(pResult=sqlstr,
	            pInput=list(object),
	            pOperator="FLJordan")

    tempResultTable <- cacheDecompResults(pFuncName="FLJordanDecompUdt",
                                          pQuery=sqlstr)

	# tempResultTable <- createTable(pTableName=gen_unique_table_name("Jordon"),
 #                                   pSelect=sqlstr)

    PMatrix <- FLMatrix(connection = connection, 
				       table_name = tempResultTable, 
					   matrix_id_value = "",
					   matrix_id_colname = "", 
					   row_id_colname = "OutputRowNum", 
					   col_id_colname = "OutputColNum", 
					   cell_val_colname = "OutPVal",
					   whereconditions=paste0(tempResultTable,".OutPVal IS NOT NULL "))


    PInvMatrix <- FLMatrix(connection = connection, 
			            table_name = tempResultTable, 
			            matrix_id_value = "",
			            matrix_id_colname = "", 
			            row_id_colname = "OutputRowNum", 
			            col_id_colname = "OutputColNum", 
			            cell_val_colname = "OutPInvVal",
			            whereconditions=paste0(tempResultTable,".OutPInvVal IS NOT NULL "))

	table <- FLTable(tempResultTable,
		             "OutputRowNum",
		             whereconditions=c(paste0(tempResultTable,".OutJVal IS NOT NULL "),
                                       paste0(tempResultTable,".OutputRowNum = ", tempResultTable,".OutputColNum "))
		            )

	JVector <- table[,"OutJVal"]

	result<-list(J = JVector,
				 P = PMatrix,
				 PInv = PInvMatrix)
	result
}

#' @export
FLJordan.FLMatrix.Hadoop<-function(object,...)
{
	stop("This function does not exist in Hadoop")
}