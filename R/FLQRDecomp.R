#' @include utilities.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

library(Matrix)

#' QR Decomposition.
#'
#' The QR decomposition involves factorizing a matrix into QMatrix and RMatrix.
#'
#' \code{qr} replicates the equivalent qr() generic function.\cr
#' @param object is of class FLMatrix
#' @param ... any additional arguments
#' @section Constraints:
#' Input can only be with maximum dimension limitations of (700 x 700).
#' @return \code{qr} returns a list of five components:
#' \item{qr}{a FLMatrix with the same dimensions as \code{object}. The upper triangle contains the R of the decomposition 
#' and the lower triangle contains information on the Q of the decomposition (stored in compact form)}
#' \item{qraux}{a FLVector of length ncol(\code{object}) which contains additional information on Q.}
#' \item{rank}{the FLVector giving rank of \code{object}}
#' \item{QMatrix}{the resulting Q Matrix stored in-database as FLMatrix}
#' \item{RMatrix}{the resulting R Matrix stored in-database as FLMatrix}
#' @examples
#' connection<-RODBC::odbcConnect("Gandalf")
#' flmatrix <- FLMatrix("FL_DEMO", 
#' "tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultList <- qr(flmatrix)
#' resultList$qr
#' resultList$qraux
#' resultList$rank
#' resultList$pivot
#' @export
qr<-function(object, ...){
	UseMethod("qr",object)
}

#' @export
qr.FLMatrix<-function(object,...)
{
	connection<-getConnection(object)
	flag1Check(connection)
	flag3Check(connection)

	tempResultTable <- gen_unique_table_name("tblQRDecompResult")

    sqlstr <- paste0("CREATE TABLE ",getRemoteTableName(getOption("ResultDatabaseFL"),tempResultTable)," AS(",
                     viewSelectMatrix(object, "a","z"),
                     outputSelectMatrix("FLQRDecompUdt",viewName="z",localName="a",
                    	outColNames=list("OutputMatrixID","OutputRowNum",
                    		"OutputColNum","OutputValQ","OutputValR"),
                    	whereClause=") WITH DATA;")
                   )

    sqlstr <- ensureQuerySize(pResult=sqlstr,
	            pInput=list(object),
	            pOperator="qr")

    sqlSendUpdate(connection,sqlstr)
	
	#calculating QRMatrix
    MID1 <- getMaxMatrixId(connection)

    sqlstrQR <-paste0(" SELECT ",MID1,
					         ",OutputRowNum
					          ,OutputColNum
					          ,OutputValQ 
					  FROM ",getRemoteTableName(getOption("ResultDatabaseFL"),tempResultTable),
					 " WHERE OutputRowNum > OutputColNum ",
					 " UNION ALL ",
					 " SELECT ",MID1,
					         ",OutputRowNum
					          ,OutputColNum
					          ,OutputValR 
					  FROM ",getRemoteTableName(getOption("ResultDatabaseFL"),tempResultTable),
					 " WHERE OutputRowNum <= OutputColNum;")

    tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = connection,
                        variables=list(
                            rowIdColumn="OutputRowNum",
                            colIdColumn="OutputColNum",
                            valueColumn="OutputVal"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstrQR)

  	flm <- new("FLMatrix",
            select= tblfunqueryobj,
            dimnames=dimnames(object))

  	QRMatrix <- store(object=flm)

    #calculating qraux
	table <- FLTable(
		             getOption("ResultDatabaseFL"),
		             tempResultTable,
		             "OutputRowNum",
		             whereconditions=paste0(getRemoteTableName(getOption("ResultDatabaseFL"),tempResultTable),".OutputRowNum = ",
		             	getRemoteTableName(getOption("ResultDatabaseFL"),tempResultTable),".OutputColNum ")
		             )

	qraux <- table[,"OutputValQ"]
	
	#calculating rank
	r<-rankMatrix(object)

	

	resultList <- list(qr = QRMatrix,
					   rank = r,
					   qraux = qraux,
					   pivot= 1:ncol(object))

	return(resultList)
}

