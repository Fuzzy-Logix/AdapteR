#' Insert R matrix object in database
#'
#' This function takes an R matrix object and inserts into an existing database
#' table with the appropriate schema 
#'
#' @param rmatrix object of class \code{matrix} 
#' @param rmatrix object of class \code{FLMatrix}
#' 
#'@export
FLInsertMatrix <- function( rmatrix,
                            flmatrix)
{
  for(i in 1:nrow(rmatrix))
  {
    for(j in 1:ncol(rmatrix))
    {      
      sqlParameters <- list(  tableName = flmatrix@matrix_table,             
                              matrixID = flmatrix@matrix_id,
                              rowID = flmatrix@row_id,
                              columnID = flmatrix@column_id,
                              cellValue = flmatrix@cell_value,
                              matrixNum = flmatrix@matrix_id_value,
                              i = i,
                              j = j,
                              rmatrixij = rmatrix[i,j])
      run_sql(connection, "FLInsertMatrix.sql", sqlParameters)
    }
  }   
}