#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

svd<-function(x, ...){
	UseMethod("svd",x)
}

svd.default<-base::svd

#' Singular Value Decomposition of a Matrix.
#'
#' \code{svd} computes the singular value decomposition for FLMatrix objects.
#'
#' The wrapper overloads svd and implicitly calls FLSVDUdt.
#' @param object is of class FLMatrix
#' @param nu number of left singular vectors to be computed.This must between 0 and nrow(object).
#' @param nv number of right singular vectors to be computed.This must between 0 and ncol(object).
#' @section Constraints:
#' Input can only be with maximum dimension limitations of (550 x 550).
#' @return \code{svd} returns a list of three components:
#'       \item{d}{a FLVector containing the singular values of x, of size min(n, p).}
#'       \item{u}{a FLMatrix whose columns contain the left singular vectors of x, present if nu > 0. Dimension c(n, nu).}
#'       \item{v}{a FLMatrix whose columns contain the right singular vectors of x, present if nv > 0. Dimension c(p, nv).}
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 3)
#' resultList <- svd(flmatrix)
#' resultList$d
#' resultList$u
#' resultList$v
#' @export

svd.FLMatrix<-function(object,nu=c(),nv=c())
{
	connection<-object@odbc_connection
	flag1Check(connection)
	flag3Check(connection)
	sqlstrU<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,"
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
							a.OutUVal 
					FROM TABLE (FLSVDUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
								HASH BY z.Matrix_ID 
								LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a
					WHERE a.OutUVal IS NOT NULL;")
	
	sqlQuery(connection,sqlstrU)

	max_matrix_id_value <<- max_matrix_id_value + 1

	UMatrix <- new("FLMatrix", 
			       odbc_connection = connection, 
			       db_name = result_db_name, 
			       matrix_table = result_matrix_table, 
				   matrix_id_value = max_matrix_id_value-1,
				   matrix_id_colname = "MATRIX_ID", 
				   row_id_colname = "ROW_ID", 
				   col_id_colname = "COL_ID", 
				   cell_val_colname = "CELL_VAL",
				   nrow = object@nrow, 
				   ncol = object@nrow, 
				   dimnames = list(c(),c()))

	sqlstrV<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,"
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
							a.OutVVal 
					FROM TABLE (FLSVDUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
								HASH BY z.Matrix_ID 
								LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a
					WHERE a.OutVVal IS NOT NULL;")
	
	sqlQuery(connection,sqlstrV)

	max_matrix_id_value <<- max_matrix_id_value + 1

	VMatrix <- new("FLMatrix", 
			       odbc_connection = connection, 
			       db_name = result_db_name, 
			       matrix_table = result_matrix_table, 
				   matrix_id_value = max_matrix_id_value-1,
				   matrix_id_colname = "MATRIX_ID", 
				   row_id_colname = "ROW_ID", 
				   col_id_colname = "COL_ID", 
				   cell_val_colname = "CELL_VAL",
				   nrow = object@ncol, 
				   ncol = object@ncol, 
				   dimnames = list(c(),c()))

	sqlstrS<-paste0("INSERT INTO ",result_db_name,".",result_vector_table,"
					WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
					AS (SELECT a.",object@matrix_id_colname,", 
							   a.",object@row_id_colname,", 
							   a.",object@col_id_colname,", 
							   a.",object@cell_val_colname," 
						FROM  ",object@matrix_table," a 
						WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") 
					SELECT ",max_vector_id_value,",
							a.OutputRowNum,
							CAST(a.OutSVal AS NUMBER) 
					FROM TABLE (FLSVDUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
								HASH BY z.Matrix_ID 
								LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a
					WHERE a.OutSVal IS NOT NULL
					AND   a.OutputRowNum = a.OutputColNum;")
	
	sqlQuery(connection,sqlstrS)

	max_vector_id_value <<- max_vector_id_value + 1
	
	table <- FLTable(connection,
		             result_db_name,
		             result_vector_table,
		             "VECTOR_ID",
		             "VECTOR_INDEX",
		             "VECTOR_VALUE")

	SVector <- new("FLVector", 
					table = table, 
					col_name = table@num_val_name, 
					vector_id_value = max_vector_id_value-1, 
					size = min(object@nrow,object@ncol))

	if (is.null(nu) && is.null(nv))
	{
		result<-list(d = SVector,
					 u = UMatrix[1:object@nrow,1:min(object@nrow,object@ncol)],
					 v = VMatrix[1:object@ncol,1:min(object@nrow,object@ncol)])
	}

	else if (is.null(nu))
	{
		result<-list(d = SVector,
					 u = UMatrix[1:object@nrow,1:min(object@nrow,object@ncol)],
					 v = VMatrix[1:object@ncol,1:min(nv,object@ncol)])
	}

	else if (is.null(nv))
	{
		result<-list(d = SVector,
					 u = UMatrix[1:object@nrow,1:min(object@nrow,nu)],
					 v = VMatrix[1:object@ncol,1:min(object@nrow,object@ncol)])
	}

	else
	{
		result<-list(d = SVector,
					 u = UMatrix[1:object@nrow,1:min(object@nrow,nu)],
					 v = VMatrix[1:object@ncol,1:min(nv,object@ncol)])
	}
	result
}

