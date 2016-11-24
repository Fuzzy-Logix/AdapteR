#' @include FLMatrix.R
NULL

#' Spectral Decomposition of a Matrix.
#'
#' \code{eigen} Computes eigenvalues and eigenvectors of FLMatrices.
#'
#' @param object is of class FLMatrix
#' @param ... any additional arguments
#' @section Constraints:
#' Input can only be a square matrix (n x n) with maximum dimension limitations
#' of (1000 x 1000).
#' Complex Eigen values and vectors are not Supported.
#' @return \code{eigen} returns a list of FLMatrix object containing the eigen vectors and
#' a FLVector object containing eigen values which replicates the equivalent R output.
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultList <- eigen(flmatrix)
#' resultList$values
#' resultList$vectors
#' @export
eigen<-function(x,symmetric=FALSE,
                only.values = FALSE, 
                EISPACK = FALSE)
{
	UseMethod("eigen", x)
}

#' @export
eigen.default<-base::eigen
#' @export
eigen.FLMatrix<-function(x,symmetric=FALSE,
                only.values = FALSE, 
                EISPACK = FALSE)
{
    connection<-getFLConnection(x)
    if(only.values)
    retobj <- list(values = FLEigenValues(x),
                   vectors = NULL)
    else
    retobj <- list(values = FLEigenValues(x),
                   vectors = FLEigenVectors(x))
    return(retobj)
}

#' @export
FLEigenValues<-function(object,...)
{
	UseMethod("FLEigenValues", object)
}

#' @export
FLEigenValues.FLMatrix<-function(object,...)
{
	
	# connection<-getFLConnection(object)
 #    ## flag3Check(connection)

	# sqlstr <-paste0(viewSelectMatrix(object,"a",withName="z"),
 #                   outputSelectMatrix("FLEigenValueUdt",viewName="z",
 #                   	                  localName="a",
 #                                      includeMID=FALSE,
 #                                      outColNames=list(vectorIdColumn="'%insertIDhere%'",
 #                                                      vectorIndexColumn="OutputRowNum",
 #                                                      vectorValueColumn="OutputVal"),
 #                   	whereClause="WHERE a.OutputRowNum = a.OutputColNum;",
 #                   	vconnection=connection)
 #                   )
	
    sqlstr <- constructMatrixUDTSQL(pObject=object,
                                 pFuncName="FLEigenValueUdt",
                                 pdims=getDimsSlot(object),
                                 pdimnames=dimnames(object),
                                 pWhereConditions=list(...)[["pWhereConditions"]],
                                 pReturnQuery=TRUE
                                 )

	tblfunqueryobj <- new("FLTableFunctionQuery",
                        connectionName = attr(connection,"name"),
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

	flv <- newFLVector(
				select = tblfunqueryobj,
				Dimnames = list(1:nrow(object),
								"vectorValueColumn"),
				isDeep = FALSE)

	return(ensureQuerySize(pResult=flv,
	            pInput=list(object),
	            pOperator="FLEigenValues",
	            pStoreResult=TRUE))
}

#' @export
FLEigenValues.FLMatrix.TD <- function(object,...)
{
    pWhereConditions <- " OutputRowNum = OutputColNum "
    return(FLEigenValues.FLMatrix(object=object,
                                 pWhereConditions=pWhereConditions,
                                 ...))
}

#' @export
FLEigenValues.FLMatrix.Hadoop <- function(object,...)
{
    pWhereConditions <- " row_id = col_id "
    return(FLEigenValues.FLMatrix(object=object,
                                 pWhereConditions=pWhereConditions,
                                 ...))
}

#' @export
FLEigenValues.FLMatrix.TDAster <- FLEigenValues.FLMatrix.Hadoop

#' @export
FLEigenVectors<-function(object,...)
{
	UseMethod("FLEigenVectors", object)
}

#' @export
FLEigenVectors.FLMatrix<-function(object,...)
{
	# connection<-getFLConnection(object)
	## flag1Check(connection)

	# sqlstr <-paste0(viewSelectMatrix(object,"a",withName="z"),
 #                    outputSelectMatrix("FLEigenVectorUdt",viewName="z",localName="a",includeMID=TRUE,
 #                    	vconnection=connection)
 #                   )

    flm <- constructMatrixUDTSQL(pObject=object,
                                 pFuncName="FLEigenVectorUdt",
                                 pdims=getDimsSlot(object),
                                 pdimnames=dimnames(object)
                                 )

	# tblfunqueryobj <- new("FLTableFunctionQuery",
 #                        connectionName = attr(connection,"name"),
 #                        variables=list(
 #                            rowIdColumn="OutputRowNum",
 #                            colIdColumn="OutputColNum",
 #                            valueColumn="OutputVal"),
 #                        whereconditions="",
 #                        order = "",
 #                        SQLquery=sqlstr)

 #  	flm <- newFLMatrix(
 #            select= tblfunqueryobj,
 #            dims=dim(object),
 #            Dimnames=dimnames(object))

  	return(ensureQuerySize(pResult=flm,
            pInput=list(object),
            pOperator="FLEigenVectors",
            pStoreResult=TRUE))
}
