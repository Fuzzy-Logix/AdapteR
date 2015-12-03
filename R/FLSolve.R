#' @include utilities.R
#' @include FLMatrix.R
#' @include FLIs.R
#' @include FLDims.R
#' @include FLPrint.R
NULL

solve <- function (x, ...){
	UseMethod("solve", x)
}

#' Inverse of a Matrix.
#'
#' \code{solve} computes the inverse for FLMatrix objects.
#'
#' The wrapper overloads solve and implicitly calls FLMatrixInvUdt.
#' @param object is of class FLMatrix
#' @section Constraints:
#' Input can only be a square matrix (n x n) with maximum dimension limitations
#' of (1000 x 1000).
#' @return \code{solve} returns a FLMatrix object which is the inverse of input FLMatrix object
#' and replicates the equivalent R output.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' resultFLMatrix <- solve(flmatrix)
#' @export

#solve.default <- base::solve
# do not define solve.default in this package as it is already defined in base::solve.default.
# It might lead to stack overflow.

solve.FLMatrix<-function(object)
{
    ## gk: I do not see why we need these, see below!
    ## checkSquare(object,"solve")
    ## checkSingularity(object)

    connection <- object@odbc_connection

    flag1Check(connection)
    MID <- max_matrix_id_value
    sqlstr<-paste0(" INSERT INTO ",
                   getRemoteTableName(result_db_name,result_matrix_table),
                   viewSelectMatrix(object,"a"),
                   outputSelectMatrix("FLMatrixInvUdt")
                   )
	
	t <- sqlSendUpdate(connection,sqlstr)

	### Phani-- If the input matrix is singular, sqlSendUpdate in above line returns the error message
	###         thrown by teradata. Is it sufficient or shall we include a query which checks singulairty
###         before finding inverse?
        ### gk: TD error is sufficient!

    ## browser()
	## if(length(t) > 0) 
	## { 
	## 	stop(" Error Inverting Matrix - Matrix might be exactly singular ") 
	## }
	
	max_matrix_id_value <<- max_matrix_id_value + 1

	return(FLMatrix(
            connection = connection, 
            database = result_db_name, 
            matrix_table = result_matrix_table, 
            matrix_id_value = MID,
            matrix_id_colname = "MATRIX_ID", 
            row_id_colname = "ROW_ID", 
            col_id_colname = "COL_ID", 
            cell_val_colname = "CELL_VAL")
           )
}


