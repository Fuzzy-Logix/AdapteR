#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

chol <- function (x, ...){
  UseMethod("chol", x)
}
#' Cholesky Decomposition.
#'
#' \code{chol} computes the Cholesky factorization of FLMatrix object.\cr
#' The Cholesky decomposition is a decomposition of a positive definite matrix 
#' into the product of a lower triangular matrix and its conjugate transpose.
#'
#' The wrapper overloads chol and implicitly calls FLCholeskyDecompUdt.
#' @param object is of class FLMatrix
#' @section Constraints:
#' Input can only be a Hermitian, positive definite square matrix (n x n)
#' with maximum dimension limitations of (1000 x 1000)
#' @return \code{chol} returns FLMatrix which is the upper triangular factor of the Cholesky decomposition
#' @examples
#' connection<-odbcConnect("Gandalf")
#' flmatrix<-FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLMatrix <- chol(flmatrix)
#' @export

# chol.default <- base::chol

chol.FLMatrix<-function(object)
{
	connection<-object@odbc_connection

	flag1Check(connection)
	
	if (nrow(object) != ncol(object))
	{
		stop ("Input matrix is not a square matrix")
	}
	else
	{
		sqlstr<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,
						" WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
						AS (
							SELECT a.",object@matrix_id_colname,", 
							a.",object@row_id_colname,", 
							a.",object@col_id_colname,", 
							a.",object@cell_val_colname," 
							FROM  ",remoteTable(object)," a 
							WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,"
							) 
						SELECT ",max_matrix_id_value,",
						         a.OutputColNum,
						         a.OutputRowNum,
						         a.OutputVal 
						FROM TABLE (
									FLCholeskyDecompUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
									HASH BY z.Matrix_ID 
									LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID
									) 
						AS a;")

		retobj <- sqlSendUpdate(connection,sqlstr)
		
		max_matrix_id_value <<- max_matrix_id_value + 1

		if (length(retobj) > 0)
		{
			stop ("Input matrix is not positive definite")
		}
		else
		{
			return(FLMatrix( 
				       connection = connection, 
				       database = result_db_name, 
				       matrix_table = result_matrix_table, 
					   matrix_id_value = max_matrix_id_value-1,
					   matrix_id_colname = "MATRIX_ID", 
					   row_id_colname = "ROW_ID", 
					   col_id_colname = "COL_ID", 
					   cell_val_colname = "CELL_VAL",
					   nrow = nrow(object), 
					   ncol = ncol(object), 
					   dimnames = list(c(),c())))
		}
	}
}
