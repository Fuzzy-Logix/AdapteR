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
	connection<-getConnection(object)
	flag1Check(connection)
	flag3Check(connection)

    ## gk:  you are executing the analysis 3 times!
    ## gk:  we need to store such things more efficiently
    ### Phani-- done by using temp table

    tempResultTable <- gen_unique_table_name("tblSVDResult")
    tempDecompTableVector <<- c(tempDecompTableVector,tempResultTable)

    sqlstr0 <- paste0("CREATE TABLE ",getRemoteTableName(result_db_name,tempResultTable)," AS(",
    				 viewSelectMatrix(object, "a","z"),
                     outputSelectMatrix("FLSVDUdt",viewName="z",localName="a",
                    	outColNames=list("OutputMatrixID","OutputRowNum",
                    		"OutputColNum","OutUVal","OutSVal","OutVVal"),
                    	whereClause=") WITH DATA;")
                   )

    sqlSendUpdate(connection,sqlstr0)

 #    MID1 <- max_matrix_id_value
 #    max_matrix_id_value <<- max_matrix_id_value + 1
	# MID2 <- max_matrix_id_value

	UMatrix <- FLMatrix( 
            connection = connection, 
            database = result_db_name, 
            matrix_table = tempResultTable, 
            matrix_id_value = "",
            matrix_id_colname = "", 
            row_id_colname = "OutputRowNum", 
            col_id_colname = "OutputColNum", 
            cell_val_colname = "OutUVal",
            whereconditions=paste0(getRemoteTableName(result_db_name,tempResultTable),".OutUVal IS NOT NULL ")
            )

	VMatrix <- FLMatrix( 
            connection = connection, 
            database = result_db_name, 
            matrix_table = tempResultTable, 
            matrix_id_value = "",
            matrix_id_colname = "", 
            row_id_colname = "OutputRowNum", 
            col_id_colname = "OutputColNum", 
            cell_val_colname = "OutVVal",
            whereconditions= paste0(getRemoteTableName(result_db_name,tempResultTable),".OutVVal IS NOT NULL ")
            )

	table <- FLTable(connection,
		             result_db_name,
		             tempResultTable,
		             "OutputRowNum",
		             whereconditions=paste0(getRemoteTableName(result_db_name,tempResultTable),".OutputRowNum = ",
		             	getRemoteTableName(result_db_name,tempResultTable),".OutputColNum ")
		             )

	SVector <- table[,"OutSVal"]
	# sqlstrU<-paste0("INSERT INTO ",
	# 				getRemoteTableName(result_db_name,result_matrix_table),
	# 				" SELECT ",MID1,
	# 				         ",OutputRowNum
	# 				          ,OutputColNum
	# 				          ,OutUVal
	# 				  FROM ",getRemoteTableName(result_db_name,tempResultTable),
	# 				 " WHERE OutUVal IS NOT NULL;")

	# sqlstrV<-paste0("INSERT INTO ",
	# 					getRemoteTableName(result_db_name,result_matrix_table),
	# 					" SELECT ",MID2,
	# 					         ",OutputRowNum
	# 					          ,OutputColNum
	# 					          ,OutVVal
	# 					  FROM ",getRemoteTableName(result_db_name,tempResultTable),
	# 					 " WHERE OutVVal IS NOT NULL;")


	# sqlstrS<-paste0("INSERT INTO ",
	# 					getRemoteTableName(result_db_name,result_vector_table),
	# 					" SELECT ",max_vector_id_value,
	# 					         ",OutputRowNum
	# 					          ,OutSVal
	# 					  FROM ",getRemoteTableName(result_db_name,tempResultTable),
	# 					 " WHERE OutputColNum=OutputRowNum;")
    
 #    sqlstr <- paste0(sqlstrU,sqlstrV,sqlstrS)
	
	# sqlSendUpdate(connection,sqlstr)

	# max_matrix_id_value <<- max_matrix_id_value + 1

	# max_vector_id_value <<- max_vector_id_value + 1

	

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

	#sqlSendUpdate(connection,paste0(" DROP TABLE ",getRemoteTableName(result_db_name,tempResultTable)))
	result
}

