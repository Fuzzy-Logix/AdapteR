# Functions to check the type of the object


#' Check if the object is an FLMatrix object
#' @export
is.FLMatrix <- function(object)
    class(object) == "FLMatrixBind" || inherits(object,"FLMatrix")

#' Check if the object is an FLVector object
#' @export
is.FLVector <- function(object)
    inherits(object,"FLVector")

#' Check if the object is an FLTable object
#' @export
is.FLTable <- function(object)
    inherits(object,"FLTable")
#' @export
is.FLTableMD <- function(object)
    inherits(object,"FLTableMD")

is.FLAbstractColumn <- function(object){
    if(class(object)=="FLAbstractColumn")
    return(TRUE)
    else return(FALSE)
}

is.FLSimpleVector <- function(object)
    class(object)=="FLSimpleVector"
is.RowFLVector <- function(pObject){
    if(is.FLVector(pObject) && 
        !isDeep(pObject) && 
            ncol(pObject)>1)
    return(TRUE)
    else return(FALSE)
}

is.wideFLTable <- function(pObject){
    if(!is.FLTable(pObject) 
        && !is.FLTableMD(pObject)
        && !is.FLVector(pObject))
    return(FALSE)
    else return(!isDeep(pObject))
}

is.FLSelectFrom <- function(pObj)
  class(pObj)=="FLSelectFrom"

#' @export
is.FL <- function(x){
    if(inherits(x,c("FLMatrix",
                    "FLVector",
                    "FLTable",
                    "FLTableQuery",
                    "FLSelectFrom",
                    "FLTableFunctionQuery",
                    "FLTableMD",
                    "FLIndexedValues",
                    "FLAbstractColumn")))
    return(TRUE)
    else return(FALSE)
}

is.RSparseMatrix <- function(object){
    vsparseClass <- c("dgCMatrix","dgeMatrix","dsCMatrix",
                    "dgTMatrix","dtrMatrix","pMatrix",
                    "dspMatrix","dtCMatrix","dgRMatrix",
                    "ddiMatrix","dpoMatrix"
                    )
    if(class(object) %in% vsparseClass)
    return(TRUE)
    else
    return(FALSE)
}


isDotFormula <- function(pFormula){
    if(!is.formula(pFormula))
        return(FALSE)
    vallVars <- all.vars(pFormula)
    if("."==vallVars[1])
        stop(". supported on RHS of formula currently \n ")
    else if("." %in% vallVars)
        return(TRUE)
    else return(FALSE)
}

is.formula <- function(pObject){
    return(class(pObject)=="formula")
}

is.QueryVector <- function(pObject){
    return(tryCatch({
            if(!is.null(pObject@select))
                return(is.FLTableFunctionQuery(pObject@select))},
            error=function(e) return(is.FLTableFunctionQuery(pObject))))
}

is.FLTableFunctionQuery <- function(pObject){
    return(class(pObject)=="FLTableFunctionQuery")
}

isContinuous <- function(x){
    if(any(suppressWarnings(is.na(as.numeric(x)))))
        return(FALSE)
    else x <- as.numeric(x)

    x <- sort(x)
    if(all(abs(diff(x))==1))
        return(TRUE)
    else return(FALSE)
}

is.FLConnection <- function(pObject){
    return(class(pObject)=="FLConnection")
}

is.ODBC <- function(pObject=getFLConnection()){
    if(is.FLConnection(pObject))
        pObject <- getRConnection(pObject)
    return(class(pObject)=="RODBC")
}

is.JDBC <- function(pObject=getFLConnection()){
    if(is.FLConnection(pObject))
        pObject <- getRConnection(pObject)
    return(class(pObject)=="JDBCConnection")
}
