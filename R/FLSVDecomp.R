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

    ## gk:  you are executing the analysis 3 times!
    ## gk:  we need to store such things more efficiently
	sqlstrU<-paste0("INSERT INTO ",
					getRemoteTableName(result_db_name,result_matrix_table)," ",
                    viewSelectMatrix(object, "a","z"),
                    outputSelectMatrix("FLSVDUdt",viewName="z",localName="a",includeMID=TRUE,
                    	outColNames=list("OutputRowNum","OutputColNum","OutUVal"),
                    	whereClause=" WHERE a.OutUVal IS NOT NULL;")
                   )
   					# " SELECT ",max_matrix_id_value ," a.OutputMatrixID,
					# 		a.OutputRowNum,
					# 		a.OutputColNum,
					# 		a.OutUVal 
					# FROM TABLE (FLSVDUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
					# 			HASH BY z.Matrix_ID 
					# 			LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a
					# WHERE a.OutUVal IS NOT NULL;")
	
	sqlSendUpdate(connection,sqlstrU)

	max_matrix_id_value <<- max_matrix_id_value + 1

	UMatrix <- FLMatrix( 
            connection = connection, 
            database = result_db_name, 
            matrix_table = result_matrix_table, 
            matrix_id_value = max_matrix_id_value-1,
            matrix_id_colname = "MATRIX_ID", 
            row_id_colname = "ROW_ID", 
            col_id_colname = "COL_ID", 
            cell_val_colname = "CELL_VAL"
            )

	sqlstrV<- paste0("INSERT INTO ",
					getRemoteTableName(result_db_name,result_matrix_table)," ",
                    viewSelectMatrix(object, "a","z"),
                    outputSelectMatrix("FLSVDUdt",viewName="z",localName="a",includeMID=TRUE,
                    	outColNames=list("OutputRowNum","OutputColNum","OutVVal"),
                    	whereClause=" WHERE a.OutVVal IS NOT NULL;")
                    )

	# sqlstrV<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table," ",
 #                    viewSelectMatrix(object, "a","z"),
	# 				" SELECT ",max_matrix_id_value,",
	# 						a.OutputRowNum,
	# 						a.OutputColNum,
	# 						a.OutVVal 
	# 				FROM TABLE (FLSVDUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
	# 							HASH BY z.Matrix_ID 
	# 							LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a
	# 				WHERE a.OutVVal IS NOT NULL;")
	
	sqlSendUpdate(connection,sqlstrV)

	max_matrix_id_value <<- max_matrix_id_value + 1

	VMatrix <- FLMatrix( 
            connection = connection, 
            database = result_db_name, 
            matrix_table = result_matrix_table, 
            matrix_id_value = max_matrix_id_value-1,
            matrix_id_colname = "MATRIX_ID", 
            row_id_colname = "ROW_ID", 
            col_id_colname = "COL_ID", 
            cell_val_colname = "CELL_VAL"
            )

	sqlstrS<-paste0("INSERT INTO ",
					getRemoteTableName(result_db_name,result_vector_table)," ",
					viewSelectMatrix(object,"a",withName="z"),
                   outputSelectMatrix("FLSVDUdt",viewName="z",
                   	localName="a",includeMID=FALSE,outColNames=list("OutputRowNum","OutSVal"),
                   	whereClause="WHERE a.OutputRowNum = a.OutputColNum;")
                   )
     #                viewSelectMatrix(object, "a","z"),
					# "SELECT ",max_vector_id_value,",
					# 		a.OutputRowNum,
					# 		CAST(a.OutSVal AS NUMBER) 
					# FROM TABLE (FLSVDUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
					# 			HASH BY z.Matrix_ID 
					# 			LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a
					# WHERE a.OutSVal IS NOT NULL
					# AND   a.OutputRowNum = a.OutputColNum;")
	
	sqlSendUpdate(connection,sqlstrS)

	max_vector_id_value <<- max_vector_id_value + 1

	table <- FLTable(connection,
		             result_db_name,
		             result_vector_table,
		             "VECTOR_INDEX",
		             whereconditions=paste0("VECTOR_ID = ",max_vector_id_value-1)
		             )

	SVector <- table[,"VECTOR_VALUE"]
	
	# table <- FLTable(connection,
	# 	             result_db_name,
	# 	             result_vector_table,
	# 	             "VECTOR_ID",
	# 	             "VECTOR_INDEX",
	# 	             "VECTOR_VALUE")

	# SVector <- new("FLVector", 
 	#                      table = table, 
 	#                      col_name = table@cell_val_colname, 
 	#                      vector_id_value = max_vector_id_value-1, 
 	#                      size = min(nrow(object),ncol(object)))

	if (is.null(nu) && is.null(nv))
	{
		result<-list(d = SVector,
					 u = UMatrix[1:nrow(object),1:min(nrow(object),ncol(object))],
					 v = VMatrix[1:ncol(object),1:min(nrow(object),ncol(object))])
	}

	else if (is.null(nu))
	{
		result<-list(d = SVector,
					 u = UMatrix[1:nrow(object),1:min(nrow(object),ncol(object))],
					 v = VMatrix[1:ncol(object),1:min(nv,ncol(object))])
	}

	else if (is.null(nv))
	{
		result<-list(d = SVector,
					 u = UMatrix[1:nrow(object),1:min(nrow(object),nu)],
					 v = VMatrix[1:ncol(object),1:min(nrow(object),ncol(object))])
	}

	else
	{
		result<-list(d = SVector,
					 u = UMatrix[1:nrow(object),1:min(nrow(object),nu)],
					 v = VMatrix[1:ncol(object),1:min(nv,ncol(object))])
	}
	result
}

