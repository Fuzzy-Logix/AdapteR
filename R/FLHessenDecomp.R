#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL


hessen<-function(x, ...){
	UseMethod("hessen",x)
}

#' Hessenberg Decomposition of a Matrix.
#'
#' \code{hessen} computes the Hessenberg decomposition for FLMatrix objects.
#'
#' The wrapper overloads hessen and implicitly calls FLHessenbergDecompUdt.
#' @param object is of class FLMatrix
#' @section Constraints:
#' Input can only be square matrix with maximum dimension limitations of (700 x 700).
#' @return \code{hessen} returns a list of two components:
#'       \item{P}{FLMatrix representing P matrix obtained from Hessenberg decomposition}
#'       \item{H}{FLMatrix representing H matrix obtained from Hessenberg decomposition}
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultList <- hessen(flmatrix)
#' resultList$P
#' resultList$H
#' @export

hessen.FLMatrix<-function(object)
{
	connection<-object@odbc_connection
	flag1Check(connection)
	
	if(object@nrow == object@ncol)
	{
		sqlstrP<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,"
						WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
						AS (SELECT a.",object@matrix_id_colname,", 
								   a.",object@row_id_colname,", 
								   a.",object@col_id_colname,", 
								   a.",object@cell_val_colname," 
							FROM  ",object@matrix_table," a 
							WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") 
						SELECT ",max_matrix_id_value,",
								a.OutputRowNum,
								a.OutputColNum,
								a.OutputPVal 
						FROM TABLE (FLHessenbergDecompUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
									HASH BY z.Matrix_ID 
									LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a
						WHERE a.OutputPVal IS NOT NULL;")
		
		sqlQuery(connection,sqlstrP)

		max_matrix_id_value <<- max_matrix_id_value + 1

		PMatrix <- new("FLMatrix", 
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

		sqlstrH <- paste0( "INSERT INTO ",result_db_name,".",result_matrix_table,"
							WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
							AS (SELECT a.",object@matrix_id_colname,", 
									   a.",object@row_id_colname,", 
									   a.",object@col_id_colname,", 
									   a.",object@cell_val_colname," 
								FROM  ",object@matrix_table," a 
								WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") 
							SELECT ",max_matrix_id_value,",
									a.OutputRowNum,
									a.OutputColNum,
									a.OutputHVal 
							FROM TABLE (FLHessenbergDecompUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
										HASH BY z.Matrix_ID 
										LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a
							WHERE a.OutputHVal IS NOT NULL;")
		
		sqlQuery(connection,sqlstrH)

		max_matrix_id_value <<- max_matrix_id_value + 1

		HMatrix <- new("FLMatrix", 
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

		result<-list(P = PMatrix,
					 H = HMatrix)
		result
	}
	else
		stop ("Input matrix is non-square")
	# if (is.null(nu) && is.null(nv))
	# {
	# 	result<-list(J = JVector,
	# 				 P = PMatrix,
	# 				 PInv = PInvMatrix[1:object@ncol,1:min(object@nrow,object@ncol)])
	# }

	# else if (is.null(nu))
	# {
	# 	result<-list(d = SVector,
	# 				 u = UMatrix[1:object@nrow,1:min(object@nrow,object@ncol)],
	# 				 v = VMatrix[1:object@ncol,1:min(nv,object@ncol)])
	# }

	# else if (is.null(nv))
	# {
	# 	result<-list(d = SVector,
	# 				 u = UMatrix[1:object@nrow,1:min(object@nrow,nu)],
	# 				 v = VMatrix[1:object@ncol,1:min(object@nrow,object@ncol)])
	# }

	# else
	# {
	# 	result<-list(d = SVector,
	# 				 u = UMatrix[1:object@nrow,1:min(object@nrow,nu)],
	# 				 v = VMatrix[1:object@ncol,1:min(nv,object@ncol)])
	# }
}
