#' @include utilities.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

eigen<-function(x, ...)
{
	UseMethod("eigen", x)
}

eigen.default<-base::eigen

#' Spectral Decomposition of a Matrix.
#'
#' \code{eigen} Computes eigenvalues and eigenvectors of FLMatrices.
#'
#' The wrapper overloads eigen and implicitly calls FLEigenValueUdt and FLEigenVectorUdt.
#' @param object is of class FLMatrix
#' @section Constraints:
#' Input can only be a square matrix (n x n) with maximum dimension limitations
#' of (1000 x 1000).
#' @return \code{eigen} returns a list of FLMatrix object containing the eigen vectors and
#' a FLVector object containing eigen values which replicates the equivalent R output.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 1)
#' resultList <- eigen(flmatrix)
#' resultList$values
#' resultList$vectors
#' @export

eigen.FLMatrix<-function(object)
{
	if(object@nrow != object@ncol) 
	{ 
		stop("eigen function applicable on square matrix only") 
	}

	connection<-object@odbc_connection

	retobj <- list(values = FLEigenValues(object), vectors = FLEigenVectors(object))
	retobj
}


FLEigenValues<-function(x,...)
{
	UseMethod("FLEigenValues", x)
}


FLEigenValues.FLMatrix<-function(object)
{

	connection<-object@odbc_connection
	flag3Check(connection)

	sqlstr0<-paste0("INSERT INTO ",result_db_name,".",result_vector_table,
					" WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) AS
					(
					SELECT a.",object@matrix_id_colname,",
					       a.",object@row_id_colname,",
					       a.",object@col_id_colname,",
					       a.",object@cell_val_colname,"
					FROM   ",object@matrix_table," a
					WHERE  a.",object@matrix_id_colname," = ",object@matrix_id_value,"
					)
					SELECT ",max_vector_id_value,",
							a.OutputRowNum, 
							CAST(a.OutputVal AS NUMBER)  
					FROM   TABLE (
					             FLEigenValueUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val)
					             HASH BY z.Matrix_ID
					             LOCAL ORDER BY z.Matrix_ID
					             ) AS a
					WHERE a.OutputRowNum = a.OutputColNum;")
	
	retobj<- sqlQuery(connection,sqlstr0)
	max_vector_id_value <<- max_vector_id_value + 1

	if(length(retobj) > 0)
	{
		stop ("Please enter a non singular square matrix")
	}
	else
	{
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
			size = object@nrow)
	}
}

FLEigenVectors<-function(x,...)
{
	UseMethod("FLEigenVectors", x)
}

FLEigenVectors.FLMatrix<-function(object)
{
	connection<-object@odbc_connection
	flag1Check(connection)

	sqlstr0<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,
					" WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) AS
					(
					SELECT a.",object@matrix_id_colname,",
					       a.",object@row_id_colname,",
					       a.",object@col_id_colname,",
					       a.",object@cell_val_colname," 
					FROM   ",object@matrix_table," a 
					WHERE  a.",object@matrix_id_colname," = ",object@matrix_id_value,"
					)
					SELECT ",max_matrix_id_value,",
							a.OutputRowNum,
							a.OutputColNum,
							a.OutputVal 
					FROM   TABLE (
					             FLEigenVectorUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val)
					             HASH BY z.Matrix_ID
					             LOCAL ORDER BY z.Matrix_ID
					             ) AS a;")
	
	retobj <- sqlQuery(connection,sqlstr0)

	max_matrix_id_value <<- max_matrix_id_value + 1

	if(length(retobj) > 0)
	{
		stop ("Please enter a non singular square matrix")
	}
	else
	{

		return(new("FLMatrix", 
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
				   dimnames = list(c(),c())))
	}
}