#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

FLSolveExcl <- function (x, ...){
	UseMethod("FLSolveExcl", x)
}

#' Inverse of a Matrix excluding a dimension.
#'
#' \code{solveExcl} computes the inverse for FLMatrix objects by excluding 
#' the specified row and column from the matrix.
#'
#' The wrapper overloads solveExcl and implicitly calls FLMatrixInvExclUdt.
#' @param object is of class FLMatrix
#' @param ExclIdx is a positive integer specifying row or column id to be excluded.
#' @section Constraints:
#' Input can only be a square matrix (n x n) with maximum dimension limitations
#' of (1000 x 1000).
#' @return \code{solveExcl} returns a FLMatrix object which is the inverse of input FLMatrix object
#' after excluding given dimension.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' resultFLMatrix <- solveExcl(flmatrix,3)
#' @export
FLSolveExcl.FLMatrix<-function(object,ExclIdx)
{

    connection<-getConnection(object)
    flag1Check(connection)

    MID <- getMaxMatrixId(connection)

    
    
    sqlstr<-paste0(" WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val, ExclIdx) 
						AS (SELECT 1, 
								   ",
                   getVariables(object)$rowId,", 
								   ",getVariables(object)$colId,", 
								   ",getVariables(object)$value,",",
								   ExclIdx, 
							" FROM  ",remoteTable(object),
							constructWhere(c(constraintsSQL(object))),") 
					SELECT ",MID,
					       ",a.OutputRowNum,
					        a.OutputColNum,
					        a.OutputVal 
					FROM TABLE (FLMatrixInvExclUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val, z.ExclIdx) 
						HASH BY z.Matrix_ID 
						LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a;")

	tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = connection,
                        variables=list(
                            rowIdColumn="OutputRowNum",
                            colIdColumn="OutputColNum",
                            valueColumn="OutputVal"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

  	flm <- new("FLMatrix",
            select= tblfunqueryobj,
            dimnames=list(dimnames(object)[[1]][(-1*ExclIdx)],
            			  dimnames(object)[[2]][(-1*ExclIdx)]))

  	return(ensureQuerySize(pResult=flm,
            pInput=list(object,ExclIdx),
            pOperator="FLSolveExcl",
            pStoreResult=TRUE))
}
