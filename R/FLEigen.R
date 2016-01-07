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
#' resultList$valueColumns
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
    connection<-getConnection(object)
    retobj <- list(values = FLEigenValues(object),
                   vectors = FLEigenVectors(object))
    retobj
}


FLEigenValues<-function(x,...)
{
	UseMethod("FLEigenValues", x)
}


FLEigenValues.FLMatrix<-function(object)
{
	
	connection<-getConnection(object)
	flag3Check(connection)

	sqlstr <-paste0(viewSelectMatrix(object,"a",withName="z"),
                   outputSelectMatrix("FLEigenValueUdt",viewName="z",
                   	localName="a",includeMID=FALSE,outColNames=list("OutputRowNum","OutputVal"),
                   	whereClause="WHERE a.OutputRowNum = a.OutputColNum;",
                   	vconnection=connection)
                   )
	
	return(store(object=sqlstr,
              returnType="VECTOR",
              connection=connection))
}

FLEigenVectors<-function(x,...)
{
	UseMethod("FLEigenVectors", x)
}

FLEigenVectors.FLMatrix<-function(object)
{
	connection<-getConnection(object)
	flag1Check(connection)

	sqlstr <-paste0(viewSelectMatrix(object,"a",withName="z"),
                    outputSelectMatrix("FLEigenVectorUdt",viewName="z",localName="a",includeMID=TRUE,
                    	vconnection=connection)
                   )

	return(store(object=sqlstr,
              returnType="MATRIX",
              connection=connection))
}
