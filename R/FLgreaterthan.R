
">" <- function(pObj1,pObj2)
{
    UseMethod(">", pObj1)
}

#' @export
`>.default` <- function(pObj1,pObj2)
{
    op <- .Primitive(">")
    op(pObj1,pObj2)
}

#' @export
`>.FLMatrix` <- function(pObj1, pObj2)
{
    connection <- getConnection(pObj1)
    if(is.FLMatrix(pObj2))
    {
        checkSameDims(pObj1,pObj2)
        a <- genRandVarName()
        b <- genRandVarName()

        sqlstr <- paste0("SELECT '%insertIDhere%' AS MATRIX_ID,",
                                   a,".rowIdColumn AS rowIdColumn,",
                                   a,".colIdColumn AS colIdColumn,
                                   CASE 
                                    WHEN ",a,".valueColumn > ",b,".valueColumn THEN 1 
                                    WHEN ",a,".valueColumn <= ",b,".valueColumn THEN 0 
                                   END AS valueColumn 
                         FROM (",constructSelect(pObj1),") AS ",a,
                            ",(",constructSelect(pObj2),") AS ",b,
                        constructWhere(c(paste0(a,".rowIdColumn = ",b,".rowIdColumn"),
                                         paste0(a,".colIdColumn = ",b,".colIdColumn"))))

        tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = connection,
                        variables=list(
                            rowIdColumn="rowIdColumn",
                            colIdColumn="colIdColumn",
                            valueColumn="valueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

        flm <- new("FLMatrix",
                select= tblfunqueryobj,
                dimnames=dimnames(pObj1))

        flm <- ensureQuerySize(pResult=flm,
                    pInput=list(pObj1,pObj2),
                    pOperator=">",
                    pStoreResult=TRUE)

        return(matrix(as.logical(as.matrix(flm)),nrow(pObj1),ncol(pObj1)))
    }
    if(is.matrix(pObj2)||class(pObj2)=="dgCMatrix"
            ||class(pObj2)=="dgeMatrix"||class(pObj2)=="dsCMatrix"
            ||class(pObj2)=="dgTMatrix")
    {
        checkSameDims(pObj1,pObj2)
        pObj2 <- as.FLMatrix(pObj2)
        return(">"(pObj1,pObj2))
    }
    # if(is.FLVector(pObj2))
    # {
    #   # pObj2 <- as.FLMatrix(pObj2, sparse=TRUE,rows=nrow(pObj1),cols=ncol(pObj1))
    #   # return(pObj1==pObj2)
    # }
    if(is.vector(pObj2))
    {
        pObj2 <- as.FLMatrix(matrix(pObj2,nrow(pObj1),ncol(pObj1)))
        return(pObj1>pObj2)
    }
    
    return(stop("incomparable inputs"))
}

#' @export
`>.FLVector` <- function(pObj1, pObj2)
{
    if(is.FLVector(pObj2))
    {
        connection <- getConnection(pObj2)
        if(checkMaxQuerySize(pObj1))
        pObj1 <- store(pObj1)
        if(checkMaxQuerySize(pObj2))
        pObj2 <- store(pObj2)
        #if(length(pObj1) != length(pObj2)) stop("non-conformable dimensions")
        a <- genRandVarName()
        b <- genRandVarName()

        if(!pObj1@isDeep && !pObj2@isDeep)
        {
            newColnames1 <- renameDuplicates(colnames(pObj1))
            newColnames2 <- renameDuplicates(colnames(pObj2))
            if(length(newColnames1)==1 && length(newColnames2)==1)
            {
            sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,",
                                    a,".vectorIndexColumn AS vectorIndexColumn,
                                    CASE 
                                     WHEN ",a,".vectorValueColumn > ",b,".vectorValueColumn  THEN 1 
                                     WHEN ",a,".vectorValueColumn <= ",b,".vectorValueColumn THEN 0 
                                    END AS vectorValueColumn 
                             FROM (",constructSelect(pObj1),") AS ",a,
                                ",(",constructSelect(pObj2),") AS ",b,
                            constructWhere(c(paste0(a,".vectorIndexColumn = ",b,".vectorIndexColumn"))),
                            collapse=" UNION ALL ")
            dimnames <- list(rownames(pObj1),"vectorValueColumn")
            }
            else if(length(newColnames1)>1 && length(newColnames2)>1)
            {
            sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,",
                                    1:max(length(newColnames1),length(newColnames2))," AS vectorIndexColumn,
                                    CASE 
                                     WHEN ",a,".",newColnames1," > ",b,".",newColnames2, " THEN 1
                                     WHEN ",a,".",newColnames1," <= ",b,".",newColnames2, " THEN 0 
                                    END AS vectorValueColumn 
                             FROM (",constructSelect(pObj1),") AS ",a,
                                ",(",constructSelect(pObj2),") AS ",b,
                            collapse=" UNION ALL ")
            dimnames <- list(1:max(length(newColnames1),length(newColnames2)),"vectorValueColumn")
            }
            else if(length(newColnames1)>1) return(store(pObj1)>pObj2)
            else return(store(pObj2)>pObj1)
        }
        else
        {
            if(pObj1@isDeep && pObj2@isDeep)
            {
                sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,",
                                        a,".vectorIndexColumn AS vectorIndexColumn,
                                        CASE 
                                         WHEN ",a,".vectorValueColumn <= ",b,".vectorValueColumn THEN 0 
                                         WHEN ",a,".vectorValueColumn > ",b,".vectorValueColumn THEN 1 
                                        END AS vectorValueColumn 
                                 FROM (",constructSelect(pObj1),") AS ",a,
                                    ",(",constructSelect(pObj2),") AS ",b,
                                constructWhere(c(paste0(a,".vectorIndexColumn = ",b,".vectorIndexColumn"))),
                                collapse=" UNION ALL ")
                dimnames <- list(rownames(pObj1),c("vectorIdColumn","vectorIndexColumn","vectorValueColumn"))
            }
            else return(as.vector(pObj1)>s.vector(pObj2))
        }

        tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = connection,
                        variables = list(
                            obs_id_colname = "vectorIndexColumn",
                            cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

        flv <- new("FLVector",
                select = tblfunqueryobj,
                dimnames = dimnames,
                isDeep = FALSE)

        flv <- ensureQuerySize(pResult=flv,
                pInput=list(pObj1,pObj2),
                pOperator=">")
        return(as.logical(as.vector(flv)))
    }
    if(is.vector(pObj2))
    {
        #if(length(pObj1) != length(pObj2)) stop("non-conformable dimensions")
        pObj2 <- as.FLVector(pObj2)
        return(">"(pObj1,pObj2))
    }
    if(is.matrix(pObj2))
    {
        pObj2 <- as.FLMatrix(pObj2)
        return(pObj2>pObj1)
    }
    # if(is.FLMatrix(pObj2))
    # return(pObj2==pObj1)

    return(stop("incomparable inputs"))
}

#' @export
`>.matrix` <- function(pObj1,pObj2)
{
    if(is.FLMatrix(pObj2))
    {
        checkSameDims(pObj1,pObj2)
        pObj1 <- as.FLMatrix(pObj1,connection=getConnection(pObj2))
        return(">"(pObj1,pObj2))   
    }
    else
    return(base::">"(pObj1,pObj2))
}

#' @export
`>.dgCMatrix` <- `>.matrix`
#' @export
`>.dgeMatrix` <- `>.matrix`
#' @export
`>.dgTMatrix` <- `>.matrix`
#' @export
`>.dsCMatrix` <- `>.matrix`

#' @export
`>.numeric` <- function(pObj1,pObj2)
{
    if(is.FLVector(pObj2))
    {
        #if(length(pObj1) != length(pObj2)) stop("non-conformable dimensions")
        pObj1 <- as.FLVector(pObj1,connection=getConnection(pObj2))
        return(">"(pObj1,pObj2))   
    }
    else
    return(base::">"(pObj1,pObj2))
}
