#' @include utilities.R
#' @include FLMatrix.R
#' @include FLIs.R
#' @include FLDims.R
#' @include FLPrint.R
NULL

solve <- function (x, ...){
	UseMethod("solve", x)
}
#solve.default <- base::solve
# do not define solve.default in this package as it is already defined in base::solve.default.
# It might lead to stack overflow.

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
solve.FLMatrix <- function(object)
{
    ## checkSquare(object,"solve")
	## checkSingularity(object)

	connection <- getConnection(object)

	flag1Check(connection)
    MID <- max_matrix_id_value

	sqlstr<-paste0(" INSERT INTO ",
                   getRemoteTableName(result_db_name, result_matrix_table),
                   viewSelectMatrix(object, "a", withName="z"),
                   outputSelectMatrix("FLMatrixInvUdt", viewName="z", localName="a", includeMID=TRUE)
                   )
	
	sqlSendUpdate(connection,sqlstr)
    ##
	max_matrix_id_value <<- max_matrix_id_value + 1

	return(FLMatrix(
            connection = connection, 
            database = result_db_name, 
            matrix_table = result_matrix_table, 
            matrix_id_value = MID,
            matrix_id_colname = "MATRIX_ID", 
            row_id_colname = "rowIdColumn", 
            col_id_colname = "colIdColumn", 
            cell_val_colname = "valueColumn")
           )
}


