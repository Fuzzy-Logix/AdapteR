#' @include utilities.R
#' @include utilities.R
#' @include FLMatrix.R
#' @include FLTable.R
NULL


#' Constructor function for FLVector, representing a vector in database
#' 
#' Please use subsetting of FLTable to create FLVector object
#' @return \code{FLVector} returns an object of class FLVector mapped to an in-database vector.
#' @seealso \code{\link{FLTable}}
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' WideTable <- FLTable( "FL_DEMO", "tblAbaloneWide","ObsID")
#' flvectorColumn <- WideTable[,"Diameter"]
#' flvectorRow <- WideTable[3,]
#' flvectorRow
#' flvectorColumn
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
                     val_col_name = table@variables$valueColumn,
                     whereconditions = c(whereconditions,
                                         equalityConstraint(
                                             table@var_id_colname,val_col_name)))
            ##V <- V[val_row_name]
        } else if(length(val_row_name)) { ## column vector deep table
            V <- new("FLVector",
                     table = table, 
                     val_col_name = table@variables$valueColumn,
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
    
    return(V)
}

#' @export
setMethod("show","FLVector",function(object) print(as.vector(object)))


