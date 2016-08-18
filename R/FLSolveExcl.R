#' @include FLMatrix.R
NULL

#' Inverse of a Matrix excluding a dimension.
#'
#' \code{FLSolveExcl} computes the inverse for FLMatrix objects by excluding 
#' the specified row and column from the matrix.
#'
#' @param x is of class FLMatrix
#' @param ExclIdx is a positive integer specifying row or column id to be excluded.
#' @param ... any additional arguments
#' @section Constraints:
#' Input can only be a square matrix (n x n) with maximum dimension limitations
#' of (1000 x 1000).
#' @return \code{solveExcl} returns a FLMatrix object which is the inverse of input FLMatrix object
#' after excluding given dimension.
#' @examples
#' flmatrix <- FLMatrix("tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLMatrix <- solveExcl(flmatrix,3)
#' @export

FLSolveExcl <- function (x,ExclIdx,...){
	UseMethod("FLSolveExcl", x)
}

#' @export
FLSolveExcl.FLMatrix<-function(object,ExclIdx,...)
{

    connection<-getConnection(object)
    flag1Check(connection)

    MID <- "'%insertIDhere%'"

    sqlstr<-paste0(" WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val, ExclIdx) 
						AS (SELECT 1, 
								   ",
                   getVariables(object)$rowId,", 
								   ",getVariables(object)$colId,", 
								   ",getVariables(object)$value,",",
								   ExclIdx, 
							" FROM  ",tableAndAlias(object),
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

    ifelse(ExclIdx>nrow(object),
            vdim <- dim(object),
            vdim <- dim(object)-1)

  	flm <- new("FLMatrix",
               select= tblfunqueryobj,
               dimnames=list(dimnames(object)[[1]][(-1*ExclIdx)],
                             dimnames(object)[[2]][(-1*ExclIdx)]),
               dim=vdim,
               dimColumns=c("OutputRowNum","OutputColNum","OutputVal"))

  	return(ensureQuerySize(pResult=flm,
            pInput=list(object,ExclIdx),
            pOperator="FLSolveExcl",
            pStoreResult=TRUE))
}
