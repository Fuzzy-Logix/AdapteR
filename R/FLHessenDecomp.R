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
	#checkSquare(object,"hessen")
	connection<-object@odbc_connection
	flag1Check(connection)
	MID <- max_matrix_id_value

	tempResultTable <- gen_unique_table_name("tblHessenResult")

    sqlstr0 <- paste0("CREATE TABLE ",tempResultTable," AS(",
    				 viewSelectMatrix(object, "a","z"),
                     outputSelectMatrix("FLHessenbergDecompUdt",viewName="z",localName="a",
                    	outColNames=list("OutputMatrixID","OutputRowNum",
                    		"OutputColNum","OutputPVal","OutputHVal"),
                    	whereClause=") WITH DATA;")
                   )

    sqlSendUpdate(connection,sqlstr0)

    MID1 <- max_matrix_id_value
    max_matrix_id_value <<- max_matrix_id_value + 1
	MID2 <- max_matrix_id_value

		sqlstrP<-paste0("INSERT INTO ",
					getRemoteTableName(result_db_name,result_matrix_table),
					" SELECT ",MID1,
					         ",OutputRowNum
					          ,OutputColNum
					          ,OutputPVal
					  FROM ",tempResultTable,
					 " WHERE OutputPVal IS NOT NULL;")

		# sqlstrP<-paste0("INSERT INTO ",
		# 				getRemoteTableName(result_db_name,result_matrix_table),
  #                  		viewSelectMatrix(object,"a",withName="z"),
  #                  		outputSelectMatrix("FLHessenbergDecompUdt",localName="a",includeMID=TRUE,
  #                  			outColNames=list("OutputRowNum","OutputColNum","OutputPVal"), viewName="z",
  #                  			whereClause=" WHERE a.OutputPVal IS NOT NULL ;")
  #                  		)

		sqlstrH<-paste0("INSERT INTO ",
					getRemoteTableName(result_db_name,result_matrix_table),
					" SELECT ",MID2,
					         ",OutputRowNum
					          ,OutputColNum
					          ,OutputPVal
					  FROM ",tempResultTable,
					 " WHERE OutputHVal IS NOT NULL;")

		# sqlstrH <- paste0("INSERT INTO ",
		# 				getRemoteTableName(result_db_name,result_matrix_table),
  #                  		viewSelectMatrix(object,"a",withName="z"),
  #                  		outputSelectMatrix("FLHessenbergDecompUdt",localName="a",includeMID=TRUE,
  #                  			outColNames=list("OutputRowNum","OutputColNum","OutputHVal"), viewName="z",
  #                  			whereClause=" WHERE a.OutputHVal IS NOT NULL ")
  #                  		)

		sqlstr <- paste0(sqlstrP,sqlstrH)
		sqlSendUpdate(connection,sqlstr)

		PMatrix <- FLMatrix(
				       connection = connection, 
				       database = result_db_name, 
				       matrix_table = result_matrix_table, 
					   matrix_id_value = MID1,
					   matrix_id_colname = "MATRIX_ID", 
					   row_id_colname = "ROW_ID", 
					   col_id_colname = "COL_ID", 
					   cell_val_colname = "CELL_VAL")

		HMatrix <- FLMatrix(
				       connection = connection, 
				       database = result_db_name, 
				       matrix_table = result_matrix_table, 
					   matrix_id_value = MID2,
					   matrix_id_colname = "MATRIX_ID", 
					   row_id_colname = "ROW_ID", 
					   col_id_colname = "COL_ID", 
					   cell_val_colname = "CELL_VAL")

		max_matrix_id_value <<- max_matrix_id_value + 1

		result<-list(P = PMatrix,
					 H = HMatrix)

		sqlSendUpdate(connection,paste0(" DROP TABLE ",tempResultTable))
		result
}
