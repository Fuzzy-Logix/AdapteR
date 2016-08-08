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
#' connection <- flConnect(odbcSource="Gandalf")
#' flmatrix <- FLMatrix("FL_DEMO", 
#' "tblMatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
#' resultFLVector <- diag(flmatrix)
#' DeepTable <- FLTable( "FL_DEMO", "tblUSArrests","ObsID")
#' flvectorDeep <- DeepTable[1:5,1]
#' resultFLMatrix <- diag(flvectorDeep)
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
    
    connection<-getConnection(object)
    flag3Check(connection)

    table <- FLTable(table=object@select@table_name,
                     obs_id_colname = getVariables(object)[[object@dimColumns[[1]]]],
                     whereconditions=c(object@select@whereconditions,
                                       paste0(getVariables(object)$rowIdColumn,
                                              "=",getVariables(object)$colIdColumn)))

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
    connection <- getConnection(object)
    flag1Check(connection)

    if(length(object)==1)
    {
        flag1Check(connection)
        value <- as.vector(object)
        MID <- getMaxMatrixId(connection)

        sqlstr <- paste(sapply(1:value,FUN=function(i)
            paste0(" INSERT INTO ",
                   getRemoteTableName(getOption("ResultDatabaseFL"),
                    getOption("ResultMatrixTableFL")),
                   " SELECT ",MID,",",
                   i,",",
                   i,",",
                   1)),collapse=";")

        sqlSendUpdate(connection,sqlstr)
        

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
    else if(length(object)>1)
    {
        if(object@isDeep)
            return(FLMatrix(table_name = object@select@table_name, 
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
            
            if(length(object@dimnames[[1]])==1)
            {
                MID <- getMaxMatrixId(connection)
                sqlstr <- paste(sapply(1:length(object),FUN=function(i){
                    if(isAliasSet(object)) 
                    vpatch <- paste0(getAlias(object),".")
                    else vpatch <- ""
                    paste0(" INSERT INTO ",
                           getRemoteTableName(getOption("ResultDatabaseFL"),
                            getOption("ResultMatrixTableFL")),
                           " SELECT ",MID,",",
                           i,",",
                           i,",",
                           paste0(vpatch,object@dimnames[[2]][i]),
                           " FROM ",remoteTable(object),
                           constructWhere(constraintsSQL(object)))
                    }),collapse=";")

                sqlSendUpdate(connection,sqlstr)

                return(FLMatrix( 
                    database = getOption("ResultDatabaseFL"), 
                    table_name = getOption("ResultMatrixTableFL"), 
                    matrix_id_value = MID,
                    matrix_id_colname = "MATRIX_ID", 
                    row_id_colname = "rowIdColumn", 
                    col_id_colname = "colIdColumn", 
                    cell_val_colname = "valueColumn",
                    connection = connection
                ))
            }
            else return(FLMatrix(matrix_id_value = "",
                     matrix_id_colname = "", 
                     row_id_colname = getVariables(object)$obs_id_colname, 
                     col_id_colname = getVariables(object)$obs_id_colname, 
                     cell_val_colname = object@dimnames[[2]],
                     whereconditions = object@select@whereconditions,
                     connection = connection
                 ))
        }
    }
}
