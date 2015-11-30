
#' @include utilities.R
NULL
setOldClass("RODBC")
#' An S4 class to represent FLMatrix
#'
#' @slot odbc_connection ODBC connectivity for R
#' @slot db_name character
#' @slot matrix_table character
#' @slot matrix_id_value numeric id value
#' @slot matrix_id_colname character
#' @slot row_id_colname character
#' @slot col_id_colname character
#' @slot cell_val_colname character
#' @slot nrow numeric number of rows of FLMatrix object
#' @slot ncol numeric number of cols of FLMatrix object
#' @slot dimnames list dimension names of FLMatrix object
#'

# Creating FLMatrix datatype
setClass(
	"FLMatrix",
	slots = list(
		odbc_connection = "ANY",
		db_name = "character",
		matrix_table = "character",
		matrix_id_value	= "ANY",
		matrix_id_colname = "character",
		row_id_colname = "character",
		col_id_colname = "character",
		cell_val_colname = "character",
		dimnames = "list",
    whereconditions="character"
	)
)

#' Constructor function for FLMatrix.
#'
#' \code{FLMatrix} constructs an object of class \code{FLMatrix}.
#'
#' \code{FLMatrix} object is an in-database equivalent to matrix object.
#' This object is used as input for matrix operation functions.
#' @param connection ODBC connection handle as returned by \code{\link[RODBC]{odbcConnect}}
#' @param database name of the database
#' @param matrix_table name of the matrix table
#' @param matrix_id_value identifier for the input matrix
#' @param matrix_id_colname matrix id value in \code{matrix_table}
#' @param row_id_colname column name in \code{matrix_table} where row numbers are stored
#' @param col_id_colname column name in \code{matrix_table} where column numbers are stored
#' @param cell_val_colname column name in \code{matrix_table} where matrix elements are stored
#' @return \code{FLMatrix} returns an object of class FLMatrix mapped
#' to an in-database matrix.
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' @export
FLMatrix <- function(connection, 
					 database=result_db_name, 
					 matrix_table, 
					 matrix_id_value,
					 matrix_id_colname = "MATRIX_ID", 
					 row_id_colname = "ROW_ID", 
					 col_id_colname = "COL_ID", 
					 cell_val_colname = "CELL_VAL", 
                     nrow=0,
                     ncol=0,
					 dimnames = NULL,
                     conditionDims=c(FALSE,FALSE),
                     whereconditions=c(""))
{
    RESULT <- new("FLMatrix",
                  odbc_connection = connection, 
                  db_name = database, 
                  matrix_table = matrix_table, 
                  matrix_id_value = as.character(matrix_id_value), 
                  matrix_id_colname = matrix_id_colname, 
                  row_id_colname = row_id_colname, 
                  col_id_colname = col_id_colname, 
                  cell_val_colname = cell_val_colname,
                  dimnames = list(),
                  whereconditions=whereconditions)
    if(is.null(dimnames)){
        dimnames <- list(
            sort(sqlQuery(connection, 
                     paste0("SELECT unique(",row_id_colname,") as rownames
							 FROM ",getRemoteTableName(database,matrix_table),
                            constructWhere(constraintsSQL(RESULT))))$rownames),
            sort(sqlQuery(connection, 
                     paste0("SELECT unique(",col_id_colname,") as colnames
							 FROM ",getRemoteTableName(database,matrix_table),
                             constructWhere(constraintsSQL(RESULT))))$colnames))
    }
    if(conditionDims[[1]])
        whereconditions <- c(whereconditions,
                         inCondition(paste0(database,".",matrix_table,".",row_id_colname),
                                     dimnames[[1]]))
    if(conditionDims[[2]])
        whereconditions <- c(whereconditions,
                         inCondition(paste0(database,".",matrix_table,".",col_id_colname),
                                     dimnames[[2]]))
    ##browser()
    ## if(length(dimnames)!=0 && ((length(dimnames[[1]])!=0 && length(dimnames[[1]])!=nrow) ||
    ##                            (length(dimnames[[2]])!=0 && length(dimnames[[2]])!=nrow)))
    ## {
    ##     stop(" ERROR in dimnames: length of dimnames not equal to array extent ")
    ## }
    RESULT@dimnames <- dimnames
    RESULT@whereconditions <- whereconditions
    return(RESULT)
    
}

inCondition <- function(col,vals){
    if(length(vals)>0)
        paste0(col," IN (", paste0("'",vals,"'",collapse= ", "), ")")
    else
        ""
}

setGeneric("constraintsSQL", function(object, localName) {
    standardGeneric("constraintsSQL")
})
setMethod("constraintsSQL", signature(object = "FLMatrix",localName="character"),
          function(object,localName="") {
              conditions <- object@whereconditions
              ## todo: generalize!
              if(""!=object@matrix_id_value){
                  if(""!=localName)
                      tableCondition <-
                      paste0(
                          remoteTable(object),
                          ".",
                          object@matrix_id_colname,
                          "=",object@matrix_id_value)
                  else
                      tableCondition <-
                      paste0(object@matrix_id_colname,
                          "=",object@matrix_id_value)
                  conditions <- c(conditions,
                                  tableCondition)
              }
              if(localName!="")
                  conditions <- gsub(object@matrix_table,
                                     localName,
                                     conditions)
              return(conditions)
          })

setMethod("constraintsSQL", signature(object = "FLMatrix",localName="missing"),
          function(object) constraintsSQL(object,""))

constructWhere <- function(conditions) {
    if(!is.character(conditions))
        stop("Provide constraints as character vector")
    conditions <- setdiff(conditions,c(NA,""))
    if(length(conditions)>0)
        paste0(" WHERE ",paste0("(",
                                conditions,")", collapse=" AND "))
    else
        ""
}



setMethod("show","FLMatrix",print.FLMatrix)

setMethod("show","FLSparseMatrix",print.FLSparseMatrix)

setGeneric("checkSquare", function(object,func_name) {
    standardGeneric("checkSquare")
})
setMethod("checkSquare", signature(object = "FLMatrix",func_name="character"),
          function(object,func_name="") {
              if(nrow(object) != ncol(object)) 
        stop(paste0(func_name," function is applicable on square matrix only"))
          })

setMethod("checkSquare", signature(object = "FLMatrix",func_name="missing"),
          function(object) checkSquare(object,""))


setGeneric("viewSelectMatrix", function(object,localName) {
    standardGeneric("viewSelectMatrix")
})
setMethod("viewSelectMatrix", signature(object = "FLMatrix",localName="character"),
          function(object,localName) {
              return(paste0(" WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
              AS (SELECT ",localName,".",object@matrix_id_colname,", 
                     ",localName,".",object@row_id_colname,", 
                     ",localName,".",object@col_id_colname,", 
                     ",localName,".",object@cell_val_colname))
          })


setGeneric("outputSelectMatrix", function(func_name,includeMID,outColNames) {
    standardGeneric("outputSelectMatrix")
})
setMethod("outputSelectMatrix", signature(func_name="character",includeMID="logical",outColNames="list"),
          function(func_name,includeMID=TRUE,
            outColNames=list("OutputRowNum","OutputColNum","OutputVal"))
          {
            return(paste0(" SELECT ",ifelse(includeMID,max_matrix_id_value,paste0("a.OutputMatrixID")),
                    paste0(",a.",outColNames,collapse="")," 
          FROM TABLE (",func_name,"(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
          HASH BY z.Matrix_ID 
          LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a;"))
          })
setMethod("outputSelectMatrix", signature(func_name="character",includeMID="missing",outColNames="list"),
          function(func_name,includeMID=TRUE,outColNames=list("OutputRowNum","OutputColNum","OutputVal"))
          {
            return(outputSelectMatrix(func_name,includeMID=TRUE,outColNames))
          })
setMethod("outputSelectMatrix", signature(func_name="character",includeMID="missing",outColNames="missing"),
          function(func_name,includeMID=TRUE,outColNames=list("OutputRowNum","OutputColNum","OutputVal"))
          {
            return(outputSelectMatrix(func_name,includeMID=TRUE,outColNames=list("OutputRowNum","OutputColNum","OutputVal")))
          })
setMethod("outputSelectMatrix", signature(func_name="character",includeMID="logical",outColNames="missing"),
          function(func_name,includeMID,outColNames=list("OutputRowNum","OutputColNum","OutputVal"))
          {
            return(outputSelectMatrix(func_name,includeMID,outColNames))
          })


setGeneric("checkSingularity", function(object) {
    standardGeneric("checkSingularity")
})
setMethod("checkSingularity", signature(object="FLMatrix"),
          function(object) {
          rank <- rankMatrix(object)
          if(rank < base::min(nrow(object),ncol(object)))
          stop("input matrix is exactly singular")
          })


setGeneric("checkHermitianPositiveDefinite", function(object) {
    standardGeneric("checkHermitianPositiveDefinite")
})
setMethod("checkHermitianPositiveDefinite", signature(object="FLMatrix"),
          function(object) {
          ### Phani-- check if matrix = its conjugated transpose (hermitian)
          ### Phani-- check if eigenvalues of matrix are all +ve.
          return(TRUE)
          })