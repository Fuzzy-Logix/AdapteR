#' @include FLMatrix.R
NULL

#' Matrix Diagonals
#'
#' Extract or replace the diagonal of a matrix, or construct a diagonal matrix.
#'
#' diag has three distinct usages:
#' x is a FLMatrix, when it extracts the diagonal.
#' x is a scalar (length-one FLVector) and the only argument, it returns a square identity matrix of size given by the scalar.
#' x is a FLVector, either of length at least 2. This returns a square matrix with the given diagonal entries.
#' @param x is an object of class FLMatrix or FLVector
#' @return If x is a FLMatrix then diag(x) returns the diagonal of x as FLVector object.
#'   If x is FLVector, the value is a diagonal square FLMatrix with diagonal elements as given in FLVector.
#' @examples
#' flmatrix <- FLMatrix(getTestTableName("tblMatrixMulti"), 5,"MATRIX_ID",
#'                      "ROW_ID","COL_ID","CELL_VAL", dims= c(5,5))
#' resultFLVector <- diag(flmatrix)
#' WideTable <- FLTable(getTestTableName("tblUSArrests"),"ObsID","VarId","Num_Val")
#' flvectorDeep <- WideTable[1:5,1]
#' resultFLMatrix <- diag(flvectorDeep)
#' @seealso \code{\link[base]{diag}} for corresponding R function reference.
#' @export
diag<-function(x, ...)
{
    UseMethod("diag", x)
}

#' @export
diag.default <- base::diag

#' @export
diag.FLMatrix<-function(object,...)
{
    connection<-getFLConnection(object)
    ## flag3Check(connection)
    table <- FLTable(table=getTableNameSlot(object),
                     obs_id_colname = getVariables(object)[[object@dimColumns[[2]]]],
                     whereconditions=c(object@select@whereconditions,
                                       paste0(getVariables(object)[[object@dimColumns[[2]]]],
                                              "=",getVariables(object)[[object@dimColumns[[3]]]])))

    valueColumn <- changeAlias(getVariables(object)$valueColumn,"","mtrx")

    flv <- table[,valueColumn]
    vlength <- min(dim(object))
    if(all(rownames(object)[1:vlength]==colnames(object)[1:vlength]))
    names(flv) <- rownames(object)[1:vlength]
    return(flv)
}

#' @export
diag.FLVector <- function(object,...)
{
    connection <- getFLConnection(object)
    ## flag1Check(connection)

    if(length(object)==1)
    {
        ## flag1Check(connection)
        value <- as.vector(object)
        # MID <- getMaxMatrixId(connection)

        # sqlstr <- paste(sapply(1:value,FUN=function(i)
        #     paste0(" INSERT INTO ",
        #            getOption("ResultMatrixTableFL"),
        #            " SELECT ",MID,",",
        #            i,",",
        #            i,",",
        #            1)),collapse=";")

        # sqlSendUpdate(connection,sqlstr)
        

        # return(FLMatrix( 
        #     table_name = getOption("ResultMatrixTableFL"), 
        #     matrix_id_value = MID,
        #     matrix_id_colname = "MATRIX_ID", 
        #     row_id_colname = "rowIdColumn", 
        #     col_id_colname = "colIdColumn", 
        #     cell_val_colname = "valueColumn",
        #     connection = connection
        # ))
        return(as.FLMatrix(Matrix(base::diag(value),sparse=TRUE)))
	
    }
    else if(length(object)>1)
    {
        if(isDeep(object))
            return(FLMatrix(table_name = getTableNameSlot(object), 
                matrix_id_value = "",
                matrix_id_colname = "", 
                row_id_colname = getVariables(object)$obs_id_colname, 
                col_id_colname = getVariables(object)$obs_id_colname, 
                cell_val_colname = getVariables(object)$cell_val_colname,
                whereconditions = object@select@whereconditions,
                connection = connection
            ))

        else
        {
            if(length(object@Dimnames[[2]])>1)
            {
                MID <- getMaxMatrixId(connection)
                if(isAliasSet(object)) 
                    vpatch <- paste0(getAlias(object),".")
                else vpatch <- ""

                vtemp <- sapply(1:length(object),
                                FUN=function(i){
                                insertIntotbl(pTableName=getOption("ResultMatrixTableFL"),
                                            pSelect=paste0(" SELECT ",MID,",",
                                                           i,",",
                                                           i,",",
                                                           paste0(vpatch,object@Dimnames[[2]][i]),
                                                           " FROM ",tableAndAlias(object),
                                                           constructWhere(constraintsSQL(object))))
                            })

                #sqlSendUpdate(connection,sqlstr)

                return(FLMatrix( 
                    table_name = getOption("ResultMatrixTableFL"), 
                    matrix_id_value = MID,
                    matrix_id_colname = "MATRIX_ID", 
                    row_id_colname = "rowIdColumn", 
                    col_id_colname = "colIdColumn", 
                    cell_val_colname = "valueColumn",
                    connection = connection
                ))
            }
            else return(FLMatrix(
                     table_name= getTableNameSlot(object),
                     matrix_id_value = "",
                     matrix_id_colname = "", 
                     row_id_colname = getVariables(object)$obs_id_colname, 
                     col_id_colname = getVariables(object)$obs_id_colname, 
                     cell_val_colname = object@Dimnames[[2]],
                     whereconditions = object@select@whereconditions,
                     connection = connection
                 ))
        }
    }
}
