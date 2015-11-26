
                                        # Print the objects

print.FLMatrix <- function(object)
{
    ##gk: todo: implement caching
    print(as.matrix(object,sparse=TRUE))
}


# Prints FLSparseMatrix object
print.FLSparseMatrix <- function(object)
{
    print(as.matrix(object,sparse=TRUE))
}



## gk:  refactor like above!

print.FLVector <- function(object)
{
    ##browser()
    if(object@table@isDeep && length(object@vector_id_value))
    {
        valuedf <- sqlQuery(object@table@odbc_connection,
                            paste0("SELECT * FROM ",
                                   remoteTable(object),
                                   " WHERE ",object@table@primary_key,"=",
                                   object@vector_id_value,
                                   " ORDER BY ",object@table@var_id_name))
        print(valuedf[,object@col_name])
        ##print(valuedf)
    } else if(!object@table@isDeep) {
        valuedf <- sqlQuery(object@table@odbc_connection,
                            paste0("SELECT ",
                                   object@table@primary_key,",",
                                   object@col_name,
                                   " FROM ",
                                   remoteTable(object),
                                   " ORDER BY ",object@table@primary_key))
        print(valuedf[,object@col_name])
    }
    ls()
}

## Prints FLVector object
print.FLVector <- function(object)
{
    print(as.vector(object))
}


# Prints FLSparseMatrix object
print.FLSparseMatrix <- function(object)
{
    print.FLMatrix(object)
}


