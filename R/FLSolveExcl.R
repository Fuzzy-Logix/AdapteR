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
#' resultFLMatrix <- FLSolveExcl(flmatrix,3)
#' @export

FLSolveExcl <- function (x,ExclIdx,...){
	UseMethod("FLSolveExcl", x)
}

#' @export
FLSolveExcl.FLMatrix<-function(object,ExclIdx,...)
{

    # connection<-getFLConnection(object)
    ## flag1Check(connection)

    # MID <- "'%insertIDhere%'"

    # sqlstr<-paste0(" WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val, ExclIdx) 
				# 		AS (SELECT 1, 
				# 				   ",
    #                getVariables(object)$rowId,", 
				# 				   ",getVariables(object)$colId,", 
				# 				   ",getVariables(object)$value,",",
				# 				   ExclIdx, 
				# 			" \n FROM  ",tableAndAlias(object),
				# 			constructWhere(c(constraintsSQL(object))),") \n ",
				# 	" SELECT ",MID," AS MATRIX_ID, \n ",
				# 	       "a.OutputRowNum AS rowIdColumn, \n ",
				# 	        "a.OutputColNum AS colIdColumn, \n ",
				# 	        "a.OutputVal AS valueColumn \n ",
				# 	" FROM TABLE (FLMatrixInvExclUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val, z.ExclIdx) 
				# 		 \n HASH BY z.Matrix_ID 
				# 		 \n LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a;")

    vdimcolumns <- getDimColumnsSlot(object)
    pViewColnames <- as.list(c(vdimcolumns,ExclIdx))
    names(pViewColnames) <- c(vdimcolumns,"ExclIdx")

    pViewColnames <- pViewColnames[c(vdimcolumns,"ExclIdx")]

    sqlstr <- constructMatrixUDTSQL(pObject=object,
                                    pFuncName="FLMatrixInvExclUdt",
                                    pdims=getDimsSlot(object),
                                    pdimnames=dimnames(object),
                                    pNest=TRUE,
                                    pViewColnames=pViewColnames,
                                    pReturnQuery=TRUE,
                                    pExtraArgs=list(ArgNames=c("EXCL"),
                                                    ArgValues=ExclIdx,
                                                    ArgRefNames="ExclIdx")
                                    )
    # sqlstr <- gsub("'%insertIDhere%'",1,sqlstr)

	tblfunqueryobj <- new("FLTableFunctionQuery",
                        connectionName = attr(connection,"name"),
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

  	flm <- newFLMatrix(
               select= tblfunqueryobj,
               Dimnames=list(dimnames(object)[[1]][(-1*ExclIdx)],
                             dimnames(object)[[2]][(-1*ExclIdx)]),
               dims=as.integer(vdim)
               #dimColumns=c("OutputRowNum","OutputColNum","OutputVal")
               )

  	return(ensureQuerySize(pResult=flm,
            pInput=list(object,ExclIdx),
            pOperator="FLSolveExcl",
            pStoreResult=TRUE))
}
