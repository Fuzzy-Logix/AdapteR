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
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultList <- eigen(flmatrix)
#' resultList$values
#' resultList$vectors
#' @export

eigen.FLMatrix<-function(object)
{
	# if(nrow(object) != ncol(object)) 
	# { 
	# 	stop("eigen function applicable on square matrix only") 
	# }
	# checkSquare(object)
	# checkSingular(object)
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

	sqlstr0<-paste0("INSERT INTO ",
					 getRemoteTableName(result_db_name,result_vector_table),
					 viewSelectMatrix(object,"a",withName="z"),
                   outputSelectMatrix("FLEigenValueUdt",viewName="z",
                   	localName="a",includeMID=FALSE,outColNames=list("OutputRowNum","OutputVal"),
                   	whereClause="WHERE a.OutputRowNum = a.OutputColNum;")
                   )
	
	retobj<- sqlSendUpdate(connection,sqlstr0)
	max_vector_id_value <<- max_vector_id_value + 1

	table <- FLTable(connection,
		             result_db_name,
		             result_vector_table,
		             "VECTOR_INDEX",
		             whereconditions=paste0("VECTOR_ID = ",max_vector_id_value-1)
		             )

	return(table[,"VECTOR_VALUE"])
}

FLEigenVectors<-function(x,...)
{
	UseMethod("FLEigenVectors", x)
}

FLEigenVectors.FLMatrix<-function(object)
{
	connection<-object@odbc_connection
	flag1Check(connection)

	sqlstr0<-paste0("INSERT INTO ",
					getRemoteTableName(result_db_name,result_matrix_table),
                    viewSelectMatrix(object,"a",withName="z"),
                    outputSelectMatrix("FLEigenVectorUdt",viewName="z",localName="a",includeMID=TRUE)
                   )

	retobj <- sqlSendUpdate(connection,sqlstr0)

	max_matrix_id_value <<- max_matrix_id_value + 1

	return(FLMatrix( 
		       connection = connection, 
		       database = result_db_name, 
		       matrix_table = result_matrix_table, 
			   matrix_id_value = max_matrix_id_value-1,
			   matrix_id_colname = "MATRIX_ID", 
			   row_id_colname = "ROW_ID", 
			   col_id_colname = "COL_ID", 
			   cell_val_colname = "CELL_VAL",
			   ))
}
