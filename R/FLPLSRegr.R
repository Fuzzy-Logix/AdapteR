NULL

#' PLS Regression.
#'
#' \code{mvr} performs linear regression on FLTable objects.
#'
#' The DB Lytix function called is FLPLSRegr. Performs PLS Regression and 
#' stores the results in predefined tables.
#'
#' @seealso \code{\link[stats]{mvr}} for R reference implementation.
#' @param formula A symbolic description of model to be fitted
#' @param data An object of class FLTable or FLTableMD.
#' @param ncomp Number of components for performing pls & opls
#' @return \code{mvr} returns an object of class \code{FLPLSRegr}
#' @examples
#' deeptbl  <- FLTable(getTestTableName("tblPLSDeep2y"), "ObsID", "VarID", "Num_Val")
#' flmod<- mvr(a~., data =deeptbl, ncomp = 5 )
#' predict(flmod); residuals(flmod);plot(flmod)
#' cof <-coefficients(flmod)
#' Functions to perform partial least squares regression (PLSR),
#' canonical powered partial least squares (CPPLS) or principal
#' component regression (PCR), with a formula interface.
#' Cross-validation can be used.  Prediction, model extraction, plot,
#' print and summary methods exist.
#' @export
mvr <- function (formula,data=list(),...) {
	UseMethod("mvr", data)
}

## move to file rlm.R
#' @export
mvr.default <- function (formula,data=list(),...) {
    if (!requireNamespace("pls", quietly = TRUE)){
        stop("mvr package needed for mvr. Please install it.",
             call. = FALSE)
    }
    else return(pls::mvr(formula=formula,data=data,...))
}

## move to file rlm.R
#' @export
mvr.FLpreparedData <- function(formula, data, ncomp = 4,...)
{
    vcallObject <- match.call()
    return(lmGeneric(formula=formula,
                     data=data,
                     callObject=vcallObject,
                     familytype="pls",
                     nfactor = ncomp,
                     ...))

}

## move to file rlm.R
#' @export
mvr.FLTable <- mvr.FLpreparedData

## move to file rlm.R
#' @export
mvr.FLTableMD <- mvr.FLpreparedData


#'OPLS Regression.
#'
#' \code{mvr} performs linear regression on FLTable objects.
#'
#' The DB Lytix function called is FLOPLSRegr. Performs OPLS Regression and 
#' stores the results in predefined tables.
#' @param formula A symbolic description of model to be fitted
#' @param data An object of class FLTable or FLTableMD.
#' @param ncomp Number of components for performing pls & opls.
#' @param northo Number of orthogonal vectors.
#' @return \code{opls} returns an object of class \code{FLPLSRegr}
#' @examples
#' deeptbl  <- FLTable(getTestTableName("tblPLSDeep2y"), "ObsID", "VarID", "Num_Val")
#' flmod<- opls(a~., data =deeptbl, ncomp = 5,northo = 5 )
#' cof <- coefficients(flmod)
#' pred <- predict(flmod);res <- residuals(flmod) 
#' @export
opls <- function (formula,data=list(),...) {
	UseMethod("opls", data)
}


## move to file rlm.R
#' @export
opls.FLpreparedData <- function(formula, data, ncomp = 4,northo = 5, ...)
{
    vcallObject <- match.call()
    return(lmGeneric(formula=formula,
                     data=data,
                     callObject=vcallObject,
                     familytype="opls",
                     nfactor = ncomp,
                     Northo = northo, 
                     ...))

}

## move to file rlm.R
#' @export
opls.FLTable <- opls.FLpreparedData

## move to file rlm.R
#' @export
opls.FLTableMD <- opls.FLpreparedData


#' @export
setClass(
    "FLPLSRegr",
    contains="FLRegr",
    slots=list(offset="character",
               vfcalls="character"))



## move to file lm.R
#' @export
`$.FLPLSRegr`<-function(object,property){
                                        #parentObject <- deparse(substitute(object))
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

    if(property=="Xmeans"){
        str <- paste0("SELECT FLmean(a.",getVariables(object@deeptable)$cell_val_colname,") AS xmeans
                       FROM ",getTableNameSlot(object@deeptable)," a \n ",
                        " WHERE a.",getVariables(object@deeptable)$var_id_colname," > -1 \n ",
                    " GROUP BY a.",getVariables(object@deeptable)$var_id_colname,
                    " ORDER BY a.",getVariables(object@deeptable)$var_id_colname," ")
        mval <- sqlQuery(connection, str)
        mval <- mval$xmeans
        return(mval)
    }
    else if(property == "Ymeans"){
        str <- paste0("SELECT FLmean(a.",getVariables(object@deeptable)$cell_val_colname,") AS ymeans \n ",
                        " FROM ",getTableNameSlot(object@deeptable)," a \n ",
                        " WHERE a.",getVariables(object@deeptable)$var_id_colname," = -1 \n ",
                        " GROUP BY a.",getVariables(object@deeptable)$var_id_colname,"")
        mval <- sqlQuery(connection, str)$ymeans
        return(as.numeric(mval))
    }
    else if(property == "methods")
    {
        stm <- "FL model"
        return(stm)
    }
    else if(property == "fitted.values")
    {
        res <- predict(object)
        return(res)
    }
    else if(property=="coefficients"){
        coefficientsvector <- coefficients(object)
        assign(parentObject,object,envir=parent.frame())
        return(coefficientsvector)
    }
    else if(property == "Yscores"){
        scr <- dfgeneric(object, "ScoreY")
        scr <- as.matrix.data.frame(scr)
        class(scr) <- "scores"
        return(scr)       
    }
    else if(property == "scores"){
        scr <- dfgeneric(object, "ScoreX")
        scr <- as.matrix.data.frame(scr)
        class(scr) <- "scores"
        return(scr)        
    }
    else if(property == "loadings")
    {
        load <- dfgeneric(object, "XBetaT")
        load <- as.matrix.data.frame(load)
        class(load) <- "loadings"
        return(load)        
    }
     else if(property == "loading.weights")
    {
        load <- dfgeneric(object, "WeightX")
        load <- as.matrix.data.frame(load)
        class(load) <- "loadings"
        return(load)                
    }
    else if(property == "WeightYN")
    {
        return(dfgeneric(object, "WeightYN"))
    }
    else if(property == "Yloadings")
    {
        load <- dfgeneric(object, "YBetaU")
        load <- as.matrix.data.frame(load)
        class(load) <- "loadings"
        return(load)              
    }
    else if(property == "rsquare")
    {
        if(object@vfcalls["functionName"]!= "FLOPLSRegr")
            print("only computed for opls as of now")
        else
        {
            print(object@AnalysisID)
            dtf <- sqlQuery(connection, "SELECT a.NumOfFactors, a.RSquare
                                     FROM fzzlOPLSRegrFactorFit AS a
                                     WHERE a.AnalysisID = '",object@AnalysisID,"'
                                     ORDER BY a.NumOfFactors")
        return(dtf)}
    }
    else if(property=="y")
    {
        if(!is.null(object@results[["y"]]))
            return(object@results[["y"]])
        else
        {
            vtablename <- getTableNameSlot(object@deeptable)
            obs_id_colname <- getVariables(object@deeptable)[["obs_id_colname"]]
            var_id_colname <- getVariables(object@deeptable)[["var_id_colname"]]
            cell_val_colname <- getVariables(object@deeptable)[["cell_val_colname"]]

            sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn,\n",
                             obs_id_colname," AS vectorIndexColumn,\n",
                             cell_val_colname," AS vectorValueColumn\n",
                             " FROM ",vtablename,
                             " \nWHERE ",var_id_colname," = -1 \n")

            tblfunqueryobj <- new("FLTableFunctionQuery",
                                  connectionName = getFLConnectionName(),
                                  variables = list(
                                      obs_id_colname = "vectorIndexColumn",
                                      cell_val_colname = "vectorValueColumn"),
                                  whereconditions="",
                                  order = "",
                                  SQLquery=sqlstr)

            yvector <- newFLVector(
                select = tblfunqueryobj,
                Dimnames = list(object@deeptable@Dimnames[[1]],
                                "vectorValueColumn"),
                dims = as.integer(c(nrow(object@deeptable),1)),
                isDeep = FALSE)
            object@results <- c(object@results,list(y=yvector))
            assign(parentObject,object,envir=parent.frame())
            return(yvector)
        }
    }    
}



#' @export
coefficients<-function(table){
	UseMethod("coefficients",table)
}

#' @export
coefficients.FLPLSRegr<-function(object){
    parentObject <- unlist(strsplit(unlist(strsplit(
        as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
    str <- paste0("SELECT * FROM ",object@vfcalls["coefftablename"],
                " WHERE AnalysisID = '",object@AnalysisID,"'ORDER BY 3, 2;")
    dtf <- sqlQuery(connection, str)
    colnames(dtf) <- tolower(colnames(dtf))
    cof <- dtf$beta[2:length(dtf$beta)]
    var <- all.vars(object@formula)[2:length(all.vars(object@formula))]
    names(cof) <- var
    cof <- as.array(cof)
    assign(parentObject,object,envir=parent.frame())
    return(cof)   
}

#' @export
predict.FLPLSRegr <- function(object,
                              newdata = object@table) {
        ObsID <- getVariables(object@deeptable)$obs_id_colname
        VarID <- getVariables(object@deeptable)$var_id_colname
        Num_Val <- getVariables(object@deeptable)$cell_val_colname

        if(object@vfcalls["functionName"] == "FLOPLSRegr")
        {cof <- "BetaHat"}
        else cof <- "Beta"
        str <- paste0(" SELECT  '%insertIDhere%' AS vectorIdColumn,
                                 b.",ObsID," AS VectorIndexColumn,
                                 FLSUMProd(b.",Num_Val,", a.",cof,") AS vectorValueColumn FROM ",
                                 object@vfcalls["coefftablename"]," a,",
                                 getTableNameSlot(object@deeptable)," b
                         WHERE a.XVarID  = b.",VarID," AND a.AnalysisID = '",object@AnalysisID,"'
                         GROUP BY b.",ObsID,"")

	tblfunqueryobj <- new("FLTableFunctionQuery",
                              connectionName = getFLConnectionName(),
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=str)
        flv <- newFLVector(
				select = tblfunqueryobj,
				Dimnames = list(rownames(object@table),
								"vectorValueColumn"),
                dims = as.integer(c(newdata@dims[1],1)),
				isDeep = FALSE)
	return(flv)
    }


#' @export
residuals.FLPLSRegr <- function(object)
{
    vfit <- predict(object)
    vyval <- object$y
    vres <- vfit - vyval
    return(vres)
}


dfgeneric <- function(object, vcomp)
{
    str <- paste0("SELECT * FROM ",object@vfcalls["statstablename"]," a
                       WHERE a.VectorName = ",fquote(vcomp)," AND
                       a.AnalysisID = '",object@AnalysisID,"'
                       ORDER BY 2,4")
    dtf <- sqlQuery(connection, str)
    colnames(dtf) <- tolower(colnames(dtf))
    ncomp <- as.numeric(object@results$mod["ncomp"])
    df2 <- data.frame(
        lapply(1:ncomp, function(x){
            dtf$num_val[dtf$factornumber == x]
        }))
    colnames(df2) <- paste0("Comp ",1:ncomp)
    return(df2)
}

#' @export
plot.FLPLSRegr <- function(object, ...)
{
    measured <- as.R(object$y)
    predicted <- as.R(predict(object))
    plot(measured, predicted, ...)
    }
