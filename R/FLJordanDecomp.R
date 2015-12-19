#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL


jordan<-function(x, ...){
	UseMethod("jordan",x)
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

jordan.FLMatrix<-function(object)
{
	connection<-getConnection(object)
	flag1Check(connection)
	flag3Check(connection)
	if(nrow(object) == ncol(object))
	{	
		sqlstrP<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,"
						WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
						AS (SELECT a.",object@matrix_id_colname,", 
								   a.",object@row_id_colname,", 
								   a.",object@col_id_colname,", 
								   a.",object@cell_val_colname," 
							FROM  ",remoteTable(object)," a 
							WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") 
						SELECT ",max_matrix_id_value,",
								a.OutputRowNum,
								a.OutputColNum,
								a.OutPVal 
						FROM TABLE (FLJordanDecompUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
									HASH BY z.Matrix_ID 
									LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a
						WHERE a.OutPVal IS NOT NULL;")

		max_matrix_id_value <<- max_matrix_id_value + 1
		
		retobj <- sqlSendUpdate(connection,sqlstrP)

		PMatrix <- FLMatrix(
				       connection = connection, 
				       database = result_db_name, 
				       matrix_table = result_matrix_table, 
					   matrix_id_value = max_matrix_id_value-1,
					   matrix_id_colname = "MATRIX_ID", 
					   row_id_colname = "ROW_ID", 
					   col_id_colname = "COL_ID", 
					   cell_val_colname = "CELL_VAL",
					   dimnames = list(c(),c()))
        ## gk: todo, check this on remote object!
		## if(length(retobj) != 0)
		## {
		## 	stop("Input matrix is singular")
		## }

		sqlstrPInv<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,"
							WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
							AS (SELECT a.",object@matrix_id_colname,", 
									   a.",object@row_id_colname,", 
									   a.",object@col_id_colname,", 
									   a.",object@cell_val_colname," 
								FROM  ",remoteTable(object)," a 
								WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") 
							SELECT ",max_matrix_id_value,",
									a.OutputRowNum,
									a.OutputColNum,
									a.OutPInvVal 
							FROM TABLE (FLJordanDecompUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
										HASH BY z.Matrix_ID 
										LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a
							WHERE a.OutPInvVal IS NOT NULL;")
		

		retobj <- sqlSendUpdate(connection,sqlstrPInv)
		max_matrix_id_value <<- max_matrix_id_value + 1
		
		## if(length(retobj) != 0)
		## {
		## 	stop("Input matrix is singular")
		## }

		PInvMatrix <- FLMatrix(
            connection = connection, 
            database = result_db_name, 
            matrix_table = result_matrix_table, 
            matrix_id_value = max_matrix_id_value-1,
            matrix_id_colname = "MATRIX_ID", 
            row_id_colname = "ROW_ID", 
            col_id_colname = "COL_ID", 
            cell_val_colname = "CELL_VAL",
            dimnames = list(c(),c()))

		sqlstrJ<-paste0("INSERT INTO ",result_db_name,".",result_vector_table,"
						WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
						AS (SELECT a.",object@matrix_id_colname,", 
								   a.",object@row_id_colname,", 
								   a.",object@col_id_colname,", 
								   a.",object@cell_val_colname," 
							FROM  ",remoteTable(object)," a 
							WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") 
						SELECT ",max_vector_id_value," AS VECTOR_ID,
								a.OutputRowNum AS VECTOR_INDEX,
								CAST(a.OutJVal AS NUMBER) AS VECTOR_VALUE 
						FROM TABLE (FLJordanDecompUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
									HASH BY z.Matrix_ID 
									LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a
						WHERE a.OutJVal IS NOT NULL
						AND   a.OutputRowNum = a.OutputColNum;")
		
		sqlSendUpdate(connection,sqlstrJ)

		max_vector_id_value <<- max_vector_id_value + 1
		
		table <- FLTable(connection,
			             result_db_name,
			             result_vector_table,
			             "VECTOR_ID",
			             "VECTOR_INDEX",
			             "VECTOR_VALUE")

		JVector <- new("FLVector", 
						table = table, 
						col_name = table@cell_val_colname, 
						vector_id_value = max_vector_id_value-1, 
						size = min(nrow(object),ncol(object)))

		result<-list(J = JVector,
					 P = PMatrix,
					 PInv = PInvMatrix)
		result
	}
	else
		stop ("Input matrix is non-square ")
}

	# if (is.null(nu) && is.null(nv))
	# {
	# 	result<-list(J = JVector,
	# 				 P = PMatrix,
	# 				 PInv = PInvMatrix[1:ncol(object),1:min(nrow(object),ncol(object))])
	# }

	# else if (is.null(nu))
	# {
	# 	result<-list(d = SVector,
	# 				 u = UMatrix[1:nrow(object),1:min(nrow(object),ncol(object))],
	# 				 v = VMatrix[1:ncol(object),1:min(nv,ncol(object))])
	# }

	# else if (is.null(nv))
	# {
	# 	result<-list(d = SVector,
	# 				 u = UMatrix[1:nrow(object),1:min(nrow(object),nu)],
	# 				 v = VMatrix[1:ncol(object),1:min(nrow(object),ncol(object))])
	# }

	# else
	# {
	# 	result<-list(d = SVector,
	# 				 u = UMatrix[1:nrow(object),1:min(nrow(object),nu)],
	# 				 v = VMatrix[1:ncol(object),1:min(nv,ncol(object))])
	# }

