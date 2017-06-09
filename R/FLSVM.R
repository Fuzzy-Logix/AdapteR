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
    pArg <- c(cost=cost)
    if(kernel == "polynomial") {
        functionName <- "FLSVMPolynomialUDT"
        pArg <- c(pArg, degree=degree)}
    else if(kernel == "radial basis" ){
        functionName = "FLSVMGaussianUDT"
        pArg <- c(pArg, sigma=sigma)
        kernel <- "Gaussian"
    }
    tblname <- gen_unique_table_name("svm")

    if(is.TDAster())
        vgroupCol <- "partition1"
    else vgroupCol <- "outputgroupid"
    t <- createTable(tblname, 
                    pSelect = constructUDTSQL(pViewColname = c(groupid = 1,
                                                             obsid = deepx@select@variables$obs_id_colname,
                                                             varid = deepx@select@variables$var_id_colname,
                                                             num_val = deepx@select@variables$cell_val_colname,
                                                             pArg),
                                            pFuncName = functionName,
                                            pOutColnames = c("a.*"),
                                            pSelect = deeptable,
                                            pLocalOrderBy=c("groupid", "obsid", "varid"), 
                                            pNest = TRUE, 
                                            pFromTableFlag = TRUE),
                    pPrimaryKey=vgroupCol,
                    pTemporary=FALSE)
    vfcalls <- c(functionName = functionName)
    
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
               RegrDataPrepSpecs=RegrDataPrepSpecs,
               wideToDeepAnalysisID = prepData[["wideToDeepAnalysisID"]],
               vfcalls = vfcalls))   
}

## use FLsimpleVector
#' @export
predict.FLSVM <- function(object, newdata = object@deeptable, scoreTable = "", ...){
    browser()
    if(!is.FLTable(newdata) && class(newdata) != "FLpreparedData") stop("scoring allowed on FLTable only")
    if(class(newdata) == "FLpreparedData"){
        newdata <- newdata$deepx
    }
    else if(newdata@select@table_name == object@deeptable@select@table_name ||
            newdata@select@table_name == object@table@select@table_name){
        newdata <- object@deeptable
        rname <- rownames(object@table)
    }
    else{
        newdata <- prepareData(object,newdata,outDeepTableName="", ...)
        rnames <- rownames(newdata)
    }

    newdata <- setAlias(newdata,"")


    if(scoreTable == ""){
        
        pvar <- getVariables(newdata)
        scoreTable <- gen_unique_table_name("svmoutput")
        scrmethod <- toupper(substr(object@results$kernel, 1,1))

        ret <- sqlStoredProc(connection,
                             "FLSVMScore",
                             ModelTable = object@results$outtbl,
                             InTable = getTableNameSlot(newdata),
                             GroupIDCol = "NULL",
                             ObsIDCol = pvar[[1]],
                             VarIDCol = pvar[[2]],
                             NumValCol = pvar[[3]],
                             ScoreMethod = scrmethod,
                             ScoreTable = scoreTable,
 ##                            Note="SVMScore from AdapteR",
                             outputParameter=c(AnalysisID="a")
                             )
        
    }
    sqlstr <- paste0("SELECT '%insertIDHere%' AS vectorIdColumn, \n ",
                     "obsid AS vectorIndexColumn, \n ",
                     "predyscoring as vectorValueColumn \n ",
                     " FROM ",scoreTable)

    tblfunqueryobj <- new("FLTableFunctionQuery",
                          connectionName = attr(connection,"name"),
                          variables = list(
                              obs_id_colname = "vectorIndexColumn",
                              cell_val_colname = "vectorValueColumn"),
                          whereconditions="",
                          order = "",
                          SQLquery=sqlstr)
    val <- newFLVector(
        select = tblfunqueryobj,
        Dimnames = list(rname,"vectorValueColumn"),
        isDeep = FALSE,
        type="double")

                                        # val <- new("FLSimpleVector",
                                        #            select= new("FLSelectFrom",
                                        #                        table_name=tblname,
                                        #                        connectionName=getFLConnectionName(),
                                        #                        variables=list(obsid = pvar[[1]],pred = "predyscoring" ),
                                        #                        whereconditions="",
                                        #                        order=""),
                                        #            dimColumns = c(pvar[[1]], "pred"),
                                        #            Dimnames = list(1:nrow(object@deeptable)),
                                        #            dims    = as.integer(c(nrow(object@deeptable), 1)),
                                        #            type       = "double"
                                        #            )
    return(val)
}

#' @export
`$.FLSVM`<-function(object,property){
                                        #parentObject <- deparse(substitute(object))
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
                            "(",fixed=T))[2],",",fixed=T))[1]

    if(is.null(object@results$votbl))
    {
        quer <- limitRowsSQL(paste0("SELECT * FROM ",object@results$outtbl," "),1)
        dno <- sqlQuery(connection, quer)
        colnames(dno) <- tolower(colnames(dno))
        object@results <- c(object@results,list(votbl = dno))
        assign(parentObject,object,envir=parent.frame())
    }
    if(property=="degree"){
        if(object@results$kernel == "polynomial")
            return(object@results$votbl$degreepoly)
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
            return(object@results$votbl$bvalue)
        else   
            return("Not applicable for this kernel")          
    }
    if(property == "crbfConstant"){
        if(object@results$kernel == "gaussian")
            return(object@results$votbl$crbfconstant)
        else
            return("Not applicable for this kernel")          
    }

    if(property == "misclassifications"){
        vcolnames <- colnames(object@results$votbl)
        vname <-  vcolnames[grepl("numofmisclass",vcolnames)]
        return(as.vector(object@results$votbl[[vname]]))
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
            
            quer <- paste0("SELECT a.",ObsID," AS obs_id_colname, a.",
                                        VarID," AS var_id_colname, b.PlaneWt*a.",
                                        Num_Val," AS cell_val_colname FROM ",
                                        object@deeptable@select@table_name," AS a, ",
                                        object@results$outtbl," AS b WHERE ",
                                        VarID," = planedimension AND  planedimension<> 0 AND ",
                                        VarID," <> 0 ")

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


print.FLSVM <- function(object, ...){
    cat("Call: \n\n")
    print(object@results$call)
    cat("Parameters:\n")
    cat("SVM-Method: Sequential Minimal Optimization(SMO)\n")   
    cat("SVM-kernel:",object@results$kernel,"\n")
    cat("misclassifications: ",object$misclassifications,"\n")
    cat("Lambda: ",object$lambda,"\n")
    cat("degree: ",object$degree,"\n")
}

setMethod("show","FLSVM",function(object){print.FLSVM(object)})
