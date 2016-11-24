#' @include FLMatrix.R
NULL

#' Cholesky Decomposition.
#'
#' \code{chol} computes the Cholesky factorization of FLMatrix object.\cr
#' The Cholesky decomposition is a decomposition of a positive definite matrix 
#' into the product of a lower triangular matrix and its conjugate transpose.
#'
#' @param object is of class FLMatrix
#' @param ... any additional arguments
#' @section Constraints:
#' Input can only be a Hermitian, positive definite square matrix (n x n)
#' with maximum dimension limitations of (1000 x 1000)
#' @return \code{chol} returns FLMatrix which is the 
#' upper triangular factor of the Cholesky decomposition
#' @examples
#' flmatrix<-FLMatrix("tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLMatrix <- chol(flmatrix)
#' @export
chol <- function (object, ...){
    UseMethod("chol", object)
}

                                        # chol.default <- base::chol
#' @export
chol.FLMatrix<-function(object,...)
{
    # connection<-getFLConnection(object)
    # ##flag1Check(connection)

    # sqlstr<-paste0(
    #     viewSelectMatrix(object,"a",withName="z"),
    #     outputSelectMatrix("FLCholeskyDecompUdt",viewName="z",
    #                        localName="a",includeMID=TRUE,vconnection=connection)
    # )
    
    # tblfunqueryobj <- new("FLTableFunctionQuery",
    #                       connectionName = attr(connection,"name"),
    #                       variables=list(
    #                           rowIdColumn="OutputColNum",
    #                           colIdColumn="OutputRowNum",
    #                           valueColumn="OutputVal"),
    #                       whereconditions="",
    #                       order = "",
    #                       SQLquery=sqlstr)

    # flm <- newFLMatrix(
    #            select= tblfunqueryobj,
    #            dims=dim(object),
    #            Dimnames=dimnames(object))
    
    flm <- constructMatrixUDTSQL(pObject=object,
                                 pFuncName="FLCholeskyDecompUdt",
                                 pdims=getDimsSlot(object),
                                 pdimnames=dimnames(object)
                                 )
    ## gk: todo: a JIRA ticket should investigate, because A %*% t(A) is definition of A=chol(object), not t(A) %*% A
    return(t(flm))
}
