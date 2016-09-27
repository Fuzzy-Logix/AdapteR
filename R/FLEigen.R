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
    connection<-getConnection(x)
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
	
	connection<-getConnection(object)
    ## flag3Check(connection)

	sqlstr <-paste0(viewSelectMatrix(object,"a",withName="z"),
                   outputSelectMatrix("FLEigenValueUdt",viewName="z",
                   	                  localName="a",
                                      includeMID=FALSE,
                                      outColNames=list(vectorIdColumn="'%insertIDhere%'",
                                                      vectorIndexColumn="OutputRowNum",
                                                      vectorValueColumn="OutputVal"),
                   	whereClause="WHERE a.OutputRowNum = a.OutputColNum;",
                   	vconnection=connection)
                   )
	
	tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = connection,
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

	flv <- newFLVector(
				select = tblfunqueryobj,
				dimnames = list(1:nrow(object),
								"vectorValueColumn"),
				isDeep = FALSE)

	return(ensureQuerySize(pResult=flv,
	            pInput=list(object),
	            pOperator="FLEigenValues",
	            pStoreResult=TRUE))
}

#' @export
FLEigenVectors<-function(object,...)
{
	UseMethod("FLEigenVectors", object)
}

#' @export
FLEigenVectors.FLMatrix<-function(object,...)
{
	connection<-getConnection(object)
	## flag1Check(connection)

	sqlstr <-paste0(viewSelectMatrix(object,"a",withName="z"),
                    outputSelectMatrix("FLEigenVectorUdt",viewName="z",localName="a",includeMID=TRUE,
                    	vconnection=connection)
                   )

	tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = connection,
                        variables=list(
                            rowIdColumn="OutputRowNum",
                            colIdColumn="OutputColNum",
                            valueColumn="OutputVal"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

  	flm <- newFLMatrix(
            select= tblfunqueryobj,
            dim=dim(object),
            dimnames=dimnames(object))

  	return(ensureQuerySize(pResult=flm,
            pInput=list(object),
            pOperator="FLEigenVectors",
            pStoreResult=TRUE))
}
