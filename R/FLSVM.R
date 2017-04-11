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

#' library(e1071)
#' Linear Kernel
#' tbl  <- FLTable("tblSVMLinSepMultiDim", "OBSID", whereconditions= "OBSID>307")
#' flmod <- svm(DEP~., data = tbl, fetchID = TRUE, kernel = "linear")
#' predict(flmod)
#'
#' polynomial Kernel
#'tbl <- FLTable("tblSVMDense", "OBSID", whereconditions = "OBSID>307")
#'flmod <- svm(DEP~., data = tbl, fetchID = TRUE, kernel = "polynomial")
#' @export
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
##    browser()
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
                            outtbl=tblname,
                            varg = pArg),
               deeptable=deepx,
               mapTable=mapTable,
               scoreTable="",
               offset=as.character(offset),
               RegrDataPrepSpecs=RegrDataPrepSpecs))   
}

## use FLsimpleVector
#' @export
predict.FLSVM <- function(object, newData = object@table){
    var <- getVariables(object@deeptable@select)
    tblname <- gen_unique_table_name("svmoutput")
    scrmethod <- toupper(substr(object@results$kernel, 1,1))
    browser()
    ret <- sqlStoredProc(connection,
                         "FLSVMScore",
                         ModelTable = object@results$outtbl,
                         TableName = object@deeptable@select@table_name,
                         GroupIDCol = NULL,
                         ObsIDCol = var[[1]],
                         VarIDCol = var[[2]],
                         NumValCol = var[[3]],
                         ScoreMethod = scrmethod,
                         ScoreTable = tblname,
                         "OutAnalysisID"
                         )
    val <- new("FLSimpleVector",
               select= new("FLSelectFrom",
                           table_name=tblname,
                           connectionName=getFLConnectionName(),
                           variables=list(OBSID = var[[1]],pred = "PredYScoring" ),
                           whereconditions="",
                           order=""),
               dimColumns = c(var[[1]], "pred"),
               Dimnames = list(1:nrow(object@deeptable)),
               dims    = as.integer(c(nrow(object@deeptable), 1)),
               type       = "double"
               )

    ##    qer <- paste0("CALL FLSVMScore(",fquote(object@results$outtbl),",
    ##                                    ",fquote(object@deeptable@select@table_name),",
    ##                                    NULL,
    ##                                    ",fquote(var[[1]])," ,
    ##                                    ",fquote(var[[2]])," ,
    ##                                    ",fquote(var[[3]])," ,
    ##                                    ",fquote(scrmethod),",  
    ##                                    ",fquote(tblname),",
    ##                                     OutAnalysisID);")
    
    ##    print(sqlQuery(connection, qer))
    ##    str <- paste0("SELECT TOP 5 * FROM ",tblname)
    ##    return(sqlQuery(connection, str) )
}

#' @export
`$.FLSVM`<-function(object,property){
                                        #parentObject <- deparse(substitute(object))
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

    if(is.null(object@results$votbl))
    {
        quer <- paste0("SELECT TOP 1 * FROM ",object@results$outtbl," ")
        dno <- sqlQuery(connection, quer)
        object@results <- c(object@results,list(votbl = dno))
        assign(parentObject,object,envir=parent.frame())
    }
    if(property=="degree"){
        if(object@results$kernel == "polynomial")
            return(object@results$votbl$DegreePoly)
        else
            return("Not applicable for this kernel")     
    }
    if(property == "cost"){
        return(object@results$varg[1]) }

    if(property == "lambda"){
        if(any(c("polynomial", "gaussian") == object@results$kernel) )
            return(object@results$votbl$lambda)
        else
            return("Not applicable for this kernel")
    }

    if(property == "BValue"){
        if(any(c("polynomial", "gaussian") == object@results$kernel))
            return(object@results$votbl$BValue)
        else   
            return("Not applicable for this kernel")          
    }
    if(property == "crbfConstant"){
        if(object@results$kernel == "gaussian")
            return(object@results$votbl$crbfConstant)
        else
            return("Not applicable for this kernel")          
    }

    if(property == "misclassifications"){
        return(object@results$votbl$NumOfMisclassifications)
    }

    if(property == "SV"){
                                        # browser()
        if(object@results$kernel == "linear")
        {
            ObsID <- getVariables(object@deeptable)$obs_id_colname
            VarID <- getVariables(object@deeptable)$var_id_colname
            Num_Val <- getVariables(object@deeptable)$cell_val_colname
            dim <- object@table@Dimnames[[2]][-length(object@table@Dimnames[[2]])]
            vdimnames <- list(object@table@Dimnames[[1]], dim)
            
            quer <- paste0("SELECT a.",ObsID," AS obs_id_colname, a.",VarID," AS var_id_colname, b.PlaneWt*a.",Num_Val," AS cell_val_colname FROM ",object@deeptable@select@table_name," AS a, ",object@results$outtbl," AS b WHERE ",VarID," = planedimension AND  planedimension<> 0 AND ",VarID," <> 0 ")

            tblfunqueryobj <- new("FLTableFunctionQuery",
                                  connectionName = getFLConnectionName(),
                                  variables = list(
                                      obs_id_colname = "obs_id_colname",
                                      var_id_colname = "var_id_colname",
                                      cell_val_colname = "cell_val_colname"),
                                  whereconditions="",
                                  order = "",
                                  SQLquery=quer)

            T <- newFLTable( 
                select = tblfunqueryobj,
                Dimnames = vdimnames,
                dims = c(nrow(object@table), ncol(object@table)-1),
                isDeep = TRUE,
                type=object@table@type,
                dimColumns=c("obs_id_colname","var_id_colname", "cell_val_colname"))
            
            return(T)
        }
        else
            return("Not applicable for this kernel")

    }
    if(property == "call"){
        return(object@results$call)
    }
}



#' @export
setMethod("names", signature("FLSVM"), function(x) c("BValue",
                                                          "SV",
                                                          "misclassifications",
                                                          "crbfConstant",
                                                          "lambda",
                                                          "cost",
                                                          "degree",
                                                          "call" ))
