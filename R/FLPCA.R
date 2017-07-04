#' @export
setClass(
    "FLPCA",
    slots=list(deeptbl="FLTable",
               otbl = "character",
               results="list"
               ))

#' \code{prcomp} Performs principal component analysis on a FLTable.
#' The function internally computes either the correlation matrix or the
#' covariance matrix and performs eigenvalue decomposition.
#' The DB Lytix function which is called id FLPCA.
#' @seealso \code{\link[stats]{prcomp}} for R reference implementation.
#' @param formula A symbolic description of model variables for which PCA is to be computed.
#' @param data An object of class FLTable.
#' @param matrixtype Indicates whether a correlation matrix or a covariance matrix
#' should be used for Eigen value decomposition.Allowed values are "CORREL" and "COVAR".
#' @slot results cache list of results computed.
#' @slot deeptbl Input data object in deep format.
#' @slot otbl output table name.
#' @return \code{prcomp} returns an object of class \code{FLPCA}
#' @examples
#' fltbl <- FLTable("tblLogRegrMulti","OBSID", "VARID", "NUM_VAL")
#' flmod <- prcomp.FLTable(data = fltbl, matrixtype = "COVAR",where = "")
#' rtbl <- iris
#' rtbl$Species <- as.numeric(rtbl$Species)
#' fliris <- as.FL(rtbl)
#' flirispca <- prcomp(Species~., data = fliris)
#' @export
prcomp <- function (formula,data=list(),...) {
    UseMethod("prcomp", data)
}

#' @export
prcomp.default <- stats::prcomp

#' @export
prcomp.FLTable <- function(formula, data,matrixtype = "COVAR" ,where = "",...)
{
    vcallObject <- match.call()
    data <- setAlias(data,"")
    return(pcageneric(formula=formula,
                      data=data,
                      matrixtype = matrixtype,
                      where = where,
                      callObject=vcallObject,
                      familytype="pca",
                      ...))
}

#' TO-DO:- support without formula.
#' @export
pcageneric <- function(formula, data, matrixtype = "COVAR",where = "" ,...){
    vcallObject <- match.call()
    deeptblname <- gen_unique_table_name("pca")
    vdeeptbl <- data
    if(!isDeep(vdeeptbl))
    {
        FLdeep <- prepareData(formula         = formula ,
                              data            = vdeeptbl,
                              outDeepTable    = deeptblname,
                              makeDataSparse  = 1,
                              performVarReduc = 0,
                              minStdDev       = .01,
                              perfromNorm = 1,     
                              maxCorrel       = .8,
                              fetchIDs        = FALSE
                              )
        vdeeptbl <- FLdeep$deepx
        vTableName =  FLdeep$deepx@select@table_name[[1]]
        vObsIDColName = FLdeep$deepx@select@variables$obs_id_colname
        vVarIDColName = FLdeep$deepx@select@variables$var_id_colname
        vValueColName= FLdeep$deepx@select@variables$cell_val_colname }
    else
    { 
        vTableName =  data@select@table_name[[1]]
        vObsIDColName = gsub("flt.", "",data@select@variables$obs_id_colname)
        vVarIDColName = gsub("flt.", "",data@select@variables$var_id_colname)
        vValueColName= gsub("flt.", "",data@select@variables$cell_val_colname) }

    cnames <- list(TableName = vTableName,
                   ObsIDColName = vObsIDColName,
                   VarIDColName = vVarIDColName,
                   ValueColName = vValueColName,
                   WhereClaue = where,
                   GroupBy = NULL,
                   MatrixType = matrixtype,
                   TableOutput = 1)

    functionName <- "FLPCA"
    ret <- sqlStoredProc(connection,
                         functionName,
                         pInputParams = cnames,
                         outputParameter = c(OutTable = 'a')
                         )
    vAnalysisID <- as.character(ret[[1]])
    vstr <- paste0("select count(distinct(OutputColNum))  as val FROM ",vAnalysisID," ")
    vdf <- sqlQuery(connection, vstr)
    vcol <- vdf$val
    vclass <- "FLPCA"

    return(new(vclass,
               deeptbl = vdeeptbl,
               otbl = vAnalysisID,
               results = list(ncol = vcol,
                              call = vcallObject)))      }


## move to file lm.R
#' @export
`$.FLPCA`<-function(object,property){
                                        #parentObject <- deparse(substitute(object))
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

    if(property == "rotation"){
        vstr <- paste0("select EIGENVEC FROM ",object@otbl," order by OutputRowNum, OutputColNum")
        vdf  <- sqlQuery(connection, vstr)
        
        vmatrix <- matrix(vdf$EIGENVEC, ncol = object@results$ncol,byrow = TRUE)
        colnames(vmatrix) <- paste0("PC",1:object@results$ncol)
        return(vmatrix) }

    if(property == "call") {
        return(object@results$call)
    }

    if(property == "sdev"){
        vstr <- paste0( "select EIGENVAL as val FROM ",object@otbl," WHERE OutputRowNum =  OutputColNum ORDER BY EIGENVAL DESC")
        vdf <- sqlQuery(connection,vstr)
        return(vdf$val^.5)
    }
    
    if(property == "x")
    {
        return("Don't compute PC mapping as of now")
    } }



#' @export
setMethod("names", signature("FLPCA"), function(x) c("call","x","rotation", "sdev"))


#' @export
print.FLPCA <- function(object){

    return(list("Standard Deviation" = object$sdev,
             "rotation" = object$rotation))
}

#' @export
setMethod("show", signature("FLPCA"), function(object) print.FLPCA(object))







