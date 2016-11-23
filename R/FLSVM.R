#' An S4 class to represent output from Support Vector Machine(svm) on in-database Objects
#'
#' @slot offset column name used as offset
#' @slot vfcalls information about system tables
#' @method print FLLinRegr
#' @method residuals FLLinRegr
#' @method plot FLLinRegr
#' @method summary FLLinRegr
#' @method predict FLLinRegr
#' @export
setClass(
    "FLSVM",
    contains="FLRegr",
    slots=list(offset="character",
               vfcalls="character"))




#' @export
#' library(e1071)
#' deeptbl  <- FLTable("tblSVMLinSepMultiDim", "OBSID", whereconditions= "OBSID>307")

svm <- function (formula,data=list(),...) {
    UseMethod("svm", data)
}

#' @export
svm.default <- e1071::svm

#' @export
svm.FLpreparedData <- function(formula, data,kernel = "linear",cost = 1, ...)
{
    print("hi")
    vcallObject <- match.call()
    data <- setAlias(data,"")
    return(svmGeneric(formula=formula,
                      data=data,
                      callObject=vcallObject,
                      familytype="",
                      kernel = "linear",
                      cost = cost,
                      ...))

}

#' @export
##svm.FLTable <- svm.FLpreparedData
svv <- function(formula, data,kernel = "linear",cost = 1, ...)
{
    print("hi")
    vcallObject <- match.call()
    data <- setAlias(data,"")
    return(svmGeneric(formula=formula,
                      data=data,
                      callObject=vcallObject,
                      familytype="",
                      kernel = "linear",
                      cost = cost,
                      ...))

}

#' @export
svm.FLTableMD <- svm.FLpreparedData






## move to file lmGeneric.R
svmGeneric <- function(formula,data,
                       callObject=NULL,
                       familytype = "",
                       kernel = "linear",
                       cost = 1,
                       ...)

{
    browser()
    prepData <- prepareData.lmGeneric(formula,data,
                                      callObject=callObject,
                                      familytype=familytype,
                                      performNorm=1,
                                      cost = 1,
                                      ...)
    
    for(i in names(prepData))
	assign(i,prepData[[i]])
    deeptable <- deepx@select@table_name
    sqlQuery(connection, "SELECT * FROM ",deeptable," ")
    functionName <- "FLSVMLinearUDT"
    pArg <- c(cost)
    if(kernel == "polynomial") {
        functionName <- "FLSVMPolynomialUDT"
        pArg <- c(pArg, Degree)}
    else if(kernel == "radial basis" ){
        functionName = "FLSVMGaussianUDT"
        pArg <- c(pArgs, sig)
    }
    tblname <- gen_unique_table_name("svm")
    t <- createTable(tblname, pSelect = constructUDTSQL(pViewColname = c(GroupID = 1,
                                                                         ObsID = "obs_id_colname",
                                                                         VarID = "var_id_colname",
                                                                         Num_Val= "cell_val_colname"),
                                                        pFuncName = functionName,
                                                        pOutColnames = c("a.*"),
                                                        pSelect = deeptable,
                                                        pArgs = pArg,
                                                        pLocalOrderBy=c("ObsID", "VarID", "Num_Val"), pNest = TRUE))
    
    df <- sqlQuery(connection, "SELECT * FROM ",tblname," ORDER BY 1, 2, 3 ")
    print(df)   
}





