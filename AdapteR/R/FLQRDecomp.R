#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

library(Matrix)

qr<-function(x, ...){
	UseMethod("qr",x)
}

#' QR Decomposition.
#'
#' The QR decomposition involves factorizing a matrix into QMatrix and RMatrix.
#'
#' \code{qr} replicates the equivalent qr() generic function.\cr
#' The wrapper overloads qr and implicitly calls FLQRDecompUdt.\cr\cr
#' @param object is of class FLMatrix
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
#' connection<-odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultList <- qr(flmatrix)
#' resultList$qr
#' resultList$qraux
#' resultList$rank
#' resultList$QMatrix
#' resultList$RMatrix
#' @export

qr.FLMatrix<-function(object)
{
	connection<-getConnection(object)
	flag1Check(connection)
	flag3Check(connection)

	tempResultTable <- gen_unique_table_name("tblQRDecompResult")
	tempDecompTableVector <<- c(tempDecompTableVector,tempResultTable)

    sqlstr0 <- paste0("CREATE TABLE ",getRemoteTableName(result_db_name,tempResultTable)," AS(",
    				 viewSelectMatrix(object, "a","z"),
                     outputSelectMatrix("FLQRDecompUdt",viewName="z",localName="a",
                    	outColNames=list("OutputMatrixID","OutputRowNum",
                    		"OutputColNum","OutputValQ","OutputValR"),
                    	whereClause=") WITH DATA;")
                   )

    sqlSendUpdate(connection,sqlstr0)

    #calculating QRMatrix
    # MID1 <- max_matrix_id_value
    # max_matrix_id_value <<- max_matrix_id_value + 1

    # QR1 <- FLMatrix( 
		  #      connection = connection, 
		  #      database = result_db_name, 
		  #      matrix_table = tempResultTable, 
			 #   matrix_id_value = "",
			 #   matrix_id_colname = "", 
			 #   row_id_colname = "OutputRowNum", 
			 #   col_id_colname = "OutputColNum", 
			 #   cell_val_colname = "OutputValQ",
			 #   whereconditions="OutputRowNum > OutputColNum")

    # QR2 <- FLMatrix( 
		  #      connection = connection, 
		  #      database = result_db_name, 
		  #      matrix_table = tempResultTable, 
			 #   matrix_id_value = "",
			 #   matrix_id_colname = "", 
			 #   row_id_colname = "OutputRowNum", 
			 #   col_id_colname = "OutputColNum", 
			 #   cell_val_colname = "OutputValR",
			 #   whereconditions="OutputRowNum <= OutputColNum")

    # QRMatrix <- new("FLUnionTables",
    # 				parts=list(QR1=QR1,QR2=QR2),
    # 				by="rows")
	
	#calculating QRMatrix
    MID1 <- max_matrix_id_value
    max_matrix_id_value <<- max_matrix_id_value + 1

    sqlstrQR1 <-paste0("INSERT INTO ",
					getRemoteTableName(result_db_name,result_matrix_table),
					" SELECT ",MID1,
					         ",OutputRowNum
					          ,OutputColNum
					          ,OutputValQ 
					  FROM ",getRemoteTableName(result_db_name,tempResultTable),
					 " WHERE OutputRowNum > OutputColNum;")

    sqlstrQR2 <-paste0("INSERT INTO ",
					getRemoteTableName(result_db_name,result_matrix_table),
					" SELECT ",MID1,
					         ",OutputRowNum
					          ,OutputColNum
					          ,OutputValR 
					  FROM ",getRemoteTableName(result_db_name,tempResultTable),
					 " WHERE OutputRowNum <= OutputColNum;")

    sqlstr <- paste(sqlstrQR1,sqlstrQR2)
	sqlSendUpdate(connection,sqlstr)

	QRMatrix <- FLMatrix( 
		       connection = connection, 
		       database = result_db_name, 
		       matrix_table = result_matrix_table, 
			   matrix_id_value = MID1,
			   matrix_id_colname = "MATRIX_ID", 
			   row_id_colname = "rowIdColumn", 
			   col_id_colname = "colIdColumn", 
			   cell_val_colname = "valueColumn")

    #calculating qraux
	table <- FLTable(connection,
		             result_db_name,
		             tempResultTable,
		             "OutputRowNum",
		             whereconditions=paste0(getRemoteTableName(result_db_name,tempResultTable),".OutputRowNum = ",
		             	getRemoteTableName(result_db_name,tempResultTable),".OutputColNum ")
		             )

	qraux <- table[,"OutputValQ"]
	
	#calculating rank
	r<-rankMatrix(object)

	

	resultList <- list(qr = QRMatrix,
					   rank = r,
					   qraux = qraux,
					   pivot= 1:ncol(object))

	#sqlSendUpdate(connection,paste0(" DROP TABLE ",getRemoteTableName(result_db_name,tempResultTable)))
	return(resultList)
}

