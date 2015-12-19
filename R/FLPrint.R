
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
    if(object@isDeep && length(object@vector_id_value))
    {
        valuedf <- sqlQuery(getConnection(object),
                            paste0("SELECT * FROM ",
                                   remoteTable(object),
                                   " WHERE ",object@obs_id_colname,"=",
                                   object@vector_id_value,
                                   " ORDER BY ",object@var_id_name))
        print(valuedf[,object@col_name])
        ##print(valuedf)
    } else if(!object@isDeep) {
        valuedf <- sqlQuery(getConnection(object),
                            paste0("SELECT ",
                                   object@obs_id_colname,",",
                                   object@col_name,
                                   " FROM ",
                                   remoteTable(object),
                                   " ORDER BY ",object@obs_id_colname))
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


