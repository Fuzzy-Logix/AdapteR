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
#' Linear Kernel
#' tbl  <- FLTable("tblSVMLinSepMultiDim", "OBSID", whereconditions= "OBSID>307")
#' flmod <- svm(DEP~., data = tbl, fetchID = TRUE, kernel = "linear")
#' predict(flmod)
#'
#' polynomial Kernel
#'tbl <- FLTable("tblSVMDense", "OBSID", whereconditions = "OBSID>307")
#'flmod <- svm(DEP~., data = tbl, fetchID = TRUE, kernel = "polynomial")
#'
#'
#' 
svm <- function (formula,data=list(),...) {
    UseMethod("svm", data)
}

#' @export
svm.default <- e1071::svm

#' @export
svm.FLpreparedData <- function(formula, data,kernel = "linear",cost = 1, degree = 3, sigma = .1, fetchID = TRUE,...)
{
    vcallObject <- match.call()
    data <- setAlias(data,"")
    return(svmGeneric(formula=formula,
                      data=data,
                      callObject=vcallObject,
                      familytype="",
                      kernel = kernel,
                      cost = cost,
                      degree = degree,
                      sigma = sigma,
                      ...))
}

#' @export
svm.FLTable <- svm.FLpreparedData

#' @export

svm.FLTableMD <- svm.FLpreparedData




## Generic function for SVM.
svmGeneric <- function(formula,data,
                       callObject=NULL,
                       familytype = "",
                       kernel = "linear",
                       cost = 1,
                       degree = 3,
                       sigma = .1,
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
    functionName <- "FLSVMLinearUDT"
    pArg <- c(cost)
    if(kernel == "polynomial") {
        functionName <- "FLSVMPolynomialUDT"
        pArg <- c(pArg, degree)}
    else if(kernel == "radial basis" ){
        functionName = "FLSVMGaussianUDT"
        pArg <- c(pArg, sigma)
        kernel <- "Gaussian"
    }
    tblname <- gen_unique_table_name("svm")
    t <- createTable(tblname, pSelect = constructUDTSQL(pViewColname = c(GroupID = 1,
                                                                         ObsID = deepx@select@variables$obs_id_colname,
                                                                         VarID = deepx@select@variables$var_id_colname,
                                                                         Num_Val= deepx@select@variables$cell_val_colname),
                                                        pFuncName = functionName,
                                                        pOutColnames = c("a.*"),
                                                        pSelect = deeptable,
                                                        pArgs = pArg,
                                                        pLocalOrderBy=c("GroupID", "ObsID", "VarID"), pNest = TRUE, pFromTableFlag = TRUE))
    
    return(new("FLSVM",
               formula=formula,
               table=data,
               results=list(call=callObject,
                            kernel = kernel,
                            outtbl=tblname),
               deeptable=deepx,
               mapTable=mapTable,
               scoreTable="",
               offset=as.character(offset),
               RegrDataPrepSpecs=RegrDataPrepSpecs))   
}


predict.FLSVM <- function(object, newData = object@table){
    var <- getVariables(object@deeptable@select)
    tblname <- gen_unique_table_name("svmoutput")
    scrmethod <- toupper(substr(object@results$kernel, 1,1))
    browser()
    qer <- paste0("CALL FLSVMScore(",fquote(object@results$outtbl),",
                                    ",fquote(object@deeptable@select@table_name),",
                                    NULL,
                                    ",fquote(var[[1]])," ,
                                    ",fquote(var[[2]])," ,
                                    ",fquote(var[[3]])," ,
                                    ",fquote(scrmethod),",  
                                    ",fquote(tblname),",
                                     OutAnalysisID);")
    print(qer)
    
    print(sqlQuery(connection, qer))
    str <- paste0("SELECT TOP 5 * FROM ",tblname)
    return(sqlQuery(connection, str) )
}


`$.FLSVM`<-function(object,property){
                                        #parentObject <- deparse(substitute(object))
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

    if(property=="degree"){
        browser()
        if(object@results$kernel == "polynomial"){
            quer <- paste0("SELECT TOP 1 DegreePoly FROM ",object@results$outtbl," ")
            dno <- sqlQuery(connection, quer)
            return(dno$DegreePoly)
            
        }
        
    }
}



