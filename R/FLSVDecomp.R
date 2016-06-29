#' @include FLMatrix.R
NULL

#' Singular Value Decomposition of a Matrix.
#'
#' \code{svd} computes the singular value decomposition for FLMatrix objects.
#'
#' @param object is of class FLMatrix
#' @param ... has nu number of left singular vectors to be computed.This must between 0 and nrow(object).
#' nv number of right singular vectors to be computed.This must between 0 and ncol(object).
#' @section Constraints:
#' Input can only be with maximum dimension limitations of (550 x 550).
#' @return \code{svd} returns a list of three components:
#'       \item{d}{a FLVector containing the singular values of x, of size min(n, p).}
#'       \item{u}{a FLMatrix whose columns contain the left singular vectors of x, present if nu > 0. Dimension c(n, nu).}
#'       \item{v}{a FLMatrix whose columns contain the right singular vectors of x, present if nv > 0. Dimension c(p, nv).}
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' flmatrix <- FLMatrix("FL_DEMO", 
#' "tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultList <- svd(flmatrix)
#' resultList$d
#' resultList$u
#' resultList$v
#' @export
svd<-function(object, ...){
	UseMethod("svd",object)
}

#' @export
svd.default<-base::svd

#' @export
svd.FLMatrix<-function(object,nu=c(),nv=c())
{
	connection<-getConnection(object)
	flag1Check(connection)
	flag3Check(connection)

    ## gk:  you are executing the analysis 3 times!
    ## gk:  we need to store such things more efficiently
    ### Phani-- done by using temp table

    tempResultTable <- gen_unique_table_name("SVD")
        
    sqlstr <- paste0("CREATE TABLE ",getRemoteTableName(getOption("ResultDatabaseFL"),tempResultTable)," AS(",
                     viewSelectMatrix(object, "a","z"),
                     outputSelectMatrix("FLSVDUdt",viewName="z",localName="a",
                    	outColNames=list("OutputMatrixID","OutputRowNum",
                    		"OutputColNum","OutUVal","OutSVal","OutVVal"),
                    	whereClause=") WITH DATA;")
                   )

    sqlstr <- ensureQuerySize(pResult=sqlstr,
	            pInput=list(object,nu,nv),
	            pOperator="svd")

    sqlSendUpdate(connection,sqlstr)

	UMatrix <- FLMatrix( 
            connection = connection, 
            database = getOption("ResultDatabaseFL"), 
            table_name = tempResultTable, 
            matrix_id_value = "",
            matrix_id_colname = "", 
            row_id_colname = "OutputRowNum", 
            col_id_colname = "OutputColNum", 
            cell_val_colname = "OutUVal",
            whereconditions=paste0(getRemoteTableName(getOption("ResultDatabaseFL"),tempResultTable),".OutUVal IS NOT NULL ")
            )

	VMatrix <- FLMatrix( 
            connection = connection, 
            database = getOption("ResultDatabaseFL"), 
            table_name = tempResultTable, 
            matrix_id_value = "",
            matrix_id_colname = "", 
            row_id_colname = "OutputRowNum", 
            col_id_colname = "OutputColNum", 
            cell_val_colname = "OutVVal",
            whereconditions= paste0(getRemoteTableName(getOption("ResultDatabaseFL"),tempResultTable),".OutVVal IS NOT NULL ")
            )

	table <- FLTable(
		             getOption("ResultDatabaseFL"),
		             tempResultTable,
		             "OutputRowNum",
		             whereconditions=c(paste0(getRemoteTableName(getOption("ResultDatabaseFL"),tempResultTable),".OutputRowNum = ",
		             	getRemoteTableName(getOption("ResultDatabaseFL"),tempResultTable),".OutputColNum "),
                        paste0(getRemoteTableName(getOption("ResultDatabaseFL"),tempResultTable),".OutSVal IS NOT NULL "))
		             )

	SVector <- table[,"OutSVal"]

	

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

