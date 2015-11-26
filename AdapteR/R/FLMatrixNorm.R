#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

FLMatrixNorm <- function (x, ...){
	UseMethod("FLMatrixNorm", x)
}

#' Norm of a Matrix.
#'
#' \code{FLMatrixNorm} gives the value of Norm for FLMatrix objects.
#'
#' The wrapper overloads FLMatrixNorm and implicitly calls FLMatrixNormUdt.
#' 
#' @param object is of class FLMatrix
#' @param NormMethod is an integer from 1-4 representing the type of norm that
#' should be computed.
#' @section Constraints:
#' Input can only be with maximum dimension limitations of (700 x 700).
#' @return \code{FLMatrixNorm} returns a FLVector object which is the Norm of input
#' FLMatrix object calculated using method specified by NormMethod input.
#' There are 4 types of norms of a matrix:
#' \item{1-Norm}{Maximum of the sum of the absolute values for the columns}
#' \item{2-Norm}{Maximum of the sum of the absolute values for the rows}
#' \item{Frobenius Norm}{Square root of the trace of (t(A)A)}
#' \item{Infinity Norm}{Square root of the maximum of the magnitudes of the Eigenvalues of (t(A)A)}
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLVector <- FLMatrixNorm(flmatrix,4)
#' @export

FLMatrixNorm.FLMatrix<-function(object,NormMethod)
{

	connection<-object@odbc_connection
	flag3Check(connection)

	if(NormMethod > 4 || NormMethod < 1)
	stop("NormMethod parameter should be whole number from 1 to 4")
	
	sqlstr<-paste0(" INSERT INTO ",result_db_name,".",result_vector_table,
				   " WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
						AS (SELECT a.",object@matrix_id_colname,", 
								   a.",object@row_id_colname,", 
								   a.",object@col_id_colname,", 
								   a.",object@cell_val_colname,  
							" FROM  ",remoteTable(object)," a 
							WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") 
					SELECT ",max_vector_id_value,
					       ",1, 
					         CAST(a.OutputNorm AS NUMBER)  
					FROM TABLE (FLMatrixNormUdt(z.Matrix_ID,",NormMethod,", z.Row_ID, z.Col_ID, z.Cell_Val) 
					HASH BY z.Matrix_ID 
					LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a;")
	
	sqlSendUpdate(connection,sqlstr)

	max_vector_id_value <<- max_vector_id_value + 1
	
	table <- FLTable(connection,
		             result_db_name,
		             result_vector_table,
		             "VECTOR_ID",
		             "VECTOR_INDEX",
		             "VECTOR_VALUE")

	new("FLVector", 
		table = table, 
		col_name = table@num_val_name, 
		vector_id_value = max_vector_id_value-1, 
		size = 1)
}
