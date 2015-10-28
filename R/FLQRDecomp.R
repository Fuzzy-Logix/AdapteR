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
	connection<-object@odbc_connection
	flag1Check(connection)
	flag3Check(connection)
	
	# calculating QMatrix
	sqlstr<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,
				   " WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
					AS (SELECT a.",object@matrix_id_colname,", 
							   a.",object@row_id_colname,", 
							   a.",object@col_id_colname,", 
							   a.",object@cell_val_colname," 
						FROM  ",object@matrix_table," a 
						WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") 
					SELECT ",max_matrix_id_value,",
							 a.OutputRowNum,
							 a.OutputColNum,
							 a.OutputValQ
					FROM TABLE (FLQRDecompUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
								HASH BY z.Matrix_ID 
								LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a;")
	
	sqlQuery(connection,sqlstr)

	max_matrix_id_value <<- max_matrix_id_value + 1

	QMatrix <- new("FLMatrix", 
		       odbc_connection = connection, 
		       db_name = result_db_name, 
		       matrix_table = result_matrix_table, 
			   matrix_id_value = max_matrix_id_value-1,
			   matrix_id_colname = "MATRIX_ID", 
			   row_id_colname = "ROW_ID", 
			   col_id_colname = "COL_ID", 
			   cell_val_colname = "CELL_VAL",
			   nrow = object@nrow, 
			   ncol = object@ncol, 
			   dimnames = list(c(),c()))

	# calculating RMatrix
	sqlstr<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,
				   " WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
					AS (SELECT a.",object@matrix_id_colname,", 
							   a.",object@row_id_colname,", 
							   a.",object@col_id_colname,", 
							   a.",object@cell_val_colname," 
						FROM  ",object@matrix_table," a 
						WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") 
					SELECT ",max_matrix_id_value,",
							 a.OutputRowNum,
							 a.OutputColNum,
							 a.OutputValR
					FROM TABLE (FLQRDecompUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
								HASH BY z.Matrix_ID 
								LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a;")
	
	sqlQuery(connection,sqlstr)

	max_matrix_id_value <<- max_matrix_id_value + 1

	RMatrix <- new("FLMatrix", 
		       odbc_connection = connection, 
		       db_name = result_db_name, 
		       matrix_table = result_matrix_table, 
			   matrix_id_value = max_matrix_id_value-1,
			   matrix_id_colname = "MATRIX_ID", 
			   row_id_colname = "ROW_ID", 
			   col_id_colname = "COL_ID", 
			   cell_val_colname = "CELL_VAL",
			   nrow = object@nrow, 
			   ncol = object@ncol, 
			   dimnames = list(c(),c()))

	#calculating qraux
	sqlstr<-paste0("INSERT INTO ",result_db_name,".",result_vector_table,
				   " WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
					AS (SELECT a.",object@matrix_id_colname,", 
							   a.",object@row_id_colname,", 
							   a.",object@col_id_colname,", 
							   a.",object@cell_val_colname," 
						FROM  ",object@matrix_table," a 
						WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") 
					SELECT ",max_vector_id_value,",
							 a.OutputRowNum,
							 CAST(a.OutputValQ AS NUMBER) 
					FROM TABLE (FLQRDecompUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
								HASH BY z.Matrix_ID 
								LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a 
					WHERE a.OutputRowNum = a.OutputColNum;")
	
	sqlQuery(connection,sqlstr)

	max_vector_id_value <<- max_vector_id_value + 1
	
	table <- FLTable(connection,
		             result_db_name,
		             result_vector_table,
		             "VECTOR_ID",
		             "VECTOR_INDEX",
		             "VECTOR_VALUE")

	qraux <- new("FLVector", 
		        table = table, 
				col_name = table@num_val_name, 
				vector_id_value = max_vector_id_value-1, 
				size = object@nrow)
	
	#calculating rank
	r<-rankMatrix(object)

	#calculating QRMatrix
	sqlstr<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,
				   " WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
					AS (SELECT a.",object@matrix_id_colname,", 
							   a.",object@row_id_colname,", 
							   a.",object@col_id_colname,", 
							   a.",object@cell_val_colname," 
						FROM  ",object@matrix_table," a 
						WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") 
					SELECT ",max_matrix_id_value,",
							 a.OutputRowNum,
							 a.OutputColNum,
							 a.OutputValQ
					FROM TABLE (FLQRDecompUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
								HASH BY z.Matrix_ID 
								LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a
	                WHERE a.OutputRowNum > a.OutputColNum;")
	
	sqlQuery(connection,sqlstr)

	sqlstr<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,
				   " WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
					AS (SELECT a.",object@matrix_id_colname,", 
							   a.",object@row_id_colname,", 
							   a.",object@col_id_colname,", 
							   a.",object@cell_val_colname," 
						FROM  ",object@matrix_table," a 
						WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") 
					SELECT ",max_matrix_id_value,",
							 a.OutputRowNum,
							 a.OutputColNum,
							 a.OutputValR
					FROM TABLE (FLQRDecompUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
								HASH BY z.Matrix_ID 
								LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a 
					WHERE a.OutputRowNum <= a.OutputColNum;")
	
	sqlQuery(connection,sqlstr)

	max_matrix_id_value <<- max_matrix_id_value + 1

	QRMatrix <- new("FLMatrix", 
		       odbc_connection = connection, 
		       db_name = result_db_name, 
		       matrix_table = result_matrix_table, 
			   matrix_id_value = max_matrix_id_value-1,
			   matrix_id_colname = "MATRIX_ID", 
			   row_id_colname = "ROW_ID", 
			   col_id_colname = "COL_ID", 
			   cell_val_colname = "CELL_VAL",
			   nrow = object@nrow, 
			   ncol = object@ncol, 
			   dimnames = list(c(),c()))

	resultList <- list(QRMatrix = QRMatrix,
					   rank = r,
					   qraux = qraux,
					   QMatrix = QMatrix,
					   RMatrix = RMatrix)
	resultList
}

