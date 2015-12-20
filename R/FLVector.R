#' @include utilities.R
#' @include FLMatrix.R
#' @include FLTable.R
NULL


#' Constructor function for FLVector, representing a vector in database, either a deep or a wide matrix.
#' gk: how can we support row vectors?
#'
#' \code{FLVector} constructs an object of class \code{FLVector}.
#'
#' \code{FLVector} object is an in-database equivalent to a vector.
#' This object is used as input for matrix and arithmetic operation functions.
#' @param table FLTable object as returned by \code{\link[AdapteR]{FLTable}}
#'   where the vector is stored
#' @param colname name of the column in \code{table} where vector elements are stored
#' @param vector_id_value unique identifier for the vector if stored in deep table
#' @return \code{FLVector} returns an object of class FLVector mapped to an in-database vector.
#' @seealso \code{\link{FLTable}}
#' @examples
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' WideTable <- FLTable(connection, "FL_TRAIN", "tblVectorWide","vector_key")
#' flvectorWide <- FLVector(WideTable,"vector_value")
#' DeepTable <- FLTable(connection, "FL_TRAIN", "tblVectorDeep","vector_id","vector_key","vector_value")
#' flvectorDeep <- FLVector(DeepTable,"vector_value",1)
#' @export
FLVector <- function(table,
                     val_col_name = character(),
                     val_row_name = character(),
                     whereconditions=character()) {
    stop("Please use subsetting to create vectors")
    V <- NULL
	if(table@isDeep) {
        if(length(val_col_name)) { ## column vector deep table
            V <- new("FLVector",
                     table = table, 
                     val_col_name = table@variables$value,
                     whereconditions = c(whereconditions,
                                         equalityConstraint(
                                             table@var_id_colname,val_col_name)))
            ##V <- V[val_row_name]
        } else if(length(val_row_name)) { ## column vector deep table
            V <- new("FLVector",
                     table = table, 
                     val_col_name = table@variables$value,
                     whereconditions = c(whereconditions,
                                         equalityConstraint(
                                             table@obs_id_colname,val_row_name)))
        }
    } else if(!table@isDeep) {
        if(length(val_col_name)) { 
            V <- new("FLVector",
                     table = table, 
                     val_col_name = val_col_name,
                     whereconditions = c(whereconditions))
            ##V <- V[val_row_name]
        } else if(length(val_row_name)) { ## column vector deep table
            V <- new("FLVector",
                     table = table, 
                     val_col_name = names(table),
                     whereconditions = c(whereconditions,
                                         paste0(table@obs_id_colname,"=",val_row_name)))
        }
    }
    if(is.null(V))
        stop("column not in wide table or invalid inputs for deep table")
    
    ## length(V) <- sqlQuery(table@odbc_connection,
    ##                    paste0("SELECT count(",table@obs_id_colname,")
	## 					 		 FROM ",
    ##                           remoteTable(V),
    ##                           constructWhere(c(constraintsSQL(V)))))[1,1]
    return(V)
}

setMethod("show","FLVector",function(object) print(as.vector(object)))


