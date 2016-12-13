NULL

##https://www.quora.com/Mathematical-Modeling-How-are-posterior-probabilities-calculated-in-linear-discriminant-analysis
##http://stats.stackexchange.com/questions/134282/relationship-between-svd-and-pca-how-to-use-svd-to-perform-pca
##https://www.analyticsvidhya.com/blog/2015/09/naive-bayes-explained/



#' An S4 class to represent output from Discriminant Analysis on in-database Objects
#'
#' @slot offset column name used as offset
#' @method coefficients FLLDA.
#' @method residuals FLLDA. 
#' @method influence FLLDA. 
#' @method lm.influence FLLDA. 
#' @method plot FLLDA. 
#' @method predict FLLDA. 
#' @export

setClass(
    "FLLDA",
    contains="FLRegr",
    slots=list(offset="character",
               vfcalls="character"))

#' @export
#' library(mda)
#' Flexible Discriminant Analysis
#' \code{fda} performs Flexible Discriminant Analysis on FLTable objects.
#'
#' The DB Lytix function called is FLFlexDiscriminant. Performs Flexible Discriminant Analysis and stores the results in predefined tables.

#' @seealso \code{\link[stats]{fda}} for R reference implementation.
#' @param formula A symbolic description of model to be fitted
#' @param data An object of class FLTable or FLTableMD.
#' @param MaxMARSMaximum number of basis functions to be used for multivariate nonparametric
#' regression step where multivariate adaptive regression splines is used.
#' @param MinRsq Terminating condition for multivariate regression step based on change in Rsq upon addition of new basis functions.
#' @return \code{fda} returns an object of class \code{FLLDA}
#' @examples
#' #'deeptbl <- FLTable("tblIrisDeep", "ObsID", "VarID", "Num_Val")
#' flmod <- fda(a~., data = deeptbl)
#' predict(flmod); cof <-coefficients(flmod);confusion(flmod)
#' Performs flexible discriminant analysis and stores the results in predefined tables. It
#' involves the use of multivariate adaptive regression splines for obtaining a basis
#' transformation of the independent variables and performing the multivariate
#' non-parametric regression step in the Flexible Discriminant Analysis procedure.
#' couldnt be implemented: plot, values, precent.explained
#' (lack of data of discriminant space).
 
fda <- function (formula,data=list(),...) {
    UseMethod("fda", data)
}

#' @export
fda.default <- mda::fda

#' @export
fda.FLpreparedData <- function(formula, data,MaxMARS = 11, MinRsq = .001 ,...)
{
    vcallObject <- match.call()
    data <- setAlias(data,"")
    return(ldaGeneric(formula=formula,
                      data=data,
                      callObject=vcallObject,
                      MaxMARS = MaxMARS,
                      MinRsq = MinRsq,
                      familytype="Flex",
                      ...))
}

#' @export
fda.FLTable <- fda.FLpreparedData

#' @export
fda.FLTableMD <- fda.FLpreparedData

#' Linear discriminant analysis.
#' \code{lda} performs Linear discriminant analysis on FLTable objects.
#'
#' The DB Lytix function called is FLLDA. Performs Linear discriminant analysis and 
#' stores the results in predefined tables.
#'
#' @seealso \code{\link[stats]{lda}} for R reference implementation.
#' @param formula A formula of the form groups ~ x1 + x2 + ... That is, the response is the grouping factor and the right hand side specifies the (non-factor) discriminators.
#' @param data An object of class FLTable or FLTableMD.
#' @return \code{LDA} returns an object of class \code{FLLDA}
#' @examples
#' tbl <- FLTable("tblLDA", "OBSID", "VARID", "NUM_VAL")
#' flmod <- lda(a~. , data = tbl)
#' flmod$scaling, flmod$means;cof <-coefficients(flmod)
#' predict(flmod): Not implemented yet.
#' plot(flmod)

## TO DO: predict, 

lda <- function (formula,data=list(),...) {
    UseMethod("lda", data)
}

#' @export
lda.default <- MASS::lda

#' @export
lda.FLpreparedData <- function(formula, data,...)
{
    vcallObject <- match.call()
    data <- setAlias(data,"")
    return(ldaGeneric(formula=formula,
                      data=data,
                      callObject=vcallObject,
                      familytype="lda",
                      ...))
}

#' @export
lda.FLTable <- lda.FLpreparedData


lda.FLTableMD <- lda.FLpreparedData


## MDA function
#' library(mda)
## Mixed DA.
#' @export
#' Mixture Discriminant Analysis.
#' \code{mda} performs Mixture Discriminant Analysis on FLTable objects.
#' The DB Lytix function called is FLMDA. Performs Mixture Discriminant Analysis and 
#' stores the results in predefined tables.
#' @seealso \code{\link[stats]{mda}} for R reference implementation.
#' @param formula A symbolic description of model to be fitted
#' @param data An object of class FLTable or FLTableMD.
#' @param subclasses Number of subclasses.
#' @param iter Maximum number of iterations for expectation maximization.
#' @param init Initialization method for each obs' latent variable Prob(x is in
#' the subclass of class) 1 = Assign weight of 1 to a
#' random subclass of its class; 0 otherwise.
#' @param hypothesis Number of hypotheses to run simultaneously
#' @return \code{mda} returns an object of class \code{FLLDA}
#' @examples
#' #'deeptbl <- FLTable("tblMDA","ObsID", "VarID", "Num_Val")
#' flmod <- mda(a~., data = deeptbl)
#' predict(flmod); flmod$N
#' cof <-coefficients(flmod)
#' FLMDA performs mixed discriminant analysis. For the training data, MDA divides each
#' class into a number of artificial subclasses. It calibrates the mixture of Gaussians and the mixing probability by maximizing the log-likelihood with expectation maximization.


#' couldnt implement dollar operator: means, precent.explained, values, plot((lack of data of discriminant space).


mda <- function (formula,data=list(),...) {
    UseMethod("mda", data)
}

#' @export
mda.default <- mda::mda

#' @export
mda.FLpreparedData <- function(formula, data,subclasses = 3, iter = 5, init = 1,hypothesis = 5, ...)
{
    vcallObject <- match.call()
    data <- setAlias(data,"")
    return(ldaGeneric(formula=formula,
                      data=data,
                      subclasses = subclasses,
                      iter = iter,
                      init = init,
                      hypothesis = hypothesis, 
                      callObject=vcallObject,
                      familytype="Mixed",
                      ...))
}

#' @export
mda.FLTable <- mda.FLpreparedData

#' @export
mda.FLTableMD <- mda.FLpreparedData



## Generic function for DA.
ldaGeneric <- function(formula,data,
                       callObject=NULL,
                       familytype = "",
                       MaxMARS = MaxMARS,
                       MinRsq = MinRsq,
                       subclasses = subclasses,
                       iter = iter,
                       init = init,
                       hypothesis = hypothesis,
                       matrixtype = "COVAR",
                       ...)
    
{
    
    prepData <- prepareData.lmGeneric(formula,data,
                                      callObject=callObject,
                                      familytype=familytype,
                                      performNorm=1,
                                      cost = 1,
                                      ...)
    vclass <- "FLLDA"
    for(i in names(prepData))
   	assign(i,prepData[[i]])
    deeptable <- deepx@select@table_name
    functionName <- "FLLDA"
    var <- getVariables(deepx)
    vinputcols <- list()
    vinputcols <- c(vinputcols,
                    TableName = deeptable,
                    ObsIDCol = var[[1]],
                    VarIDCol = var[[2]],
                    ValueCol = var[[3]])
    extra <- NULL
    if(familytype == "Flex")
    {
        functionName = "FLFlexDiscriminant"
        vinputcols = c(vinputcols, MaxBasisMARS = MaxMARS, MinDeltaRsqMARS =MinRsq)     
        
    }
    
    if (familytype == "Mixed")
    {
        functionName = "FLMDA"
        vinputcols = c(vinputcols, WhereClause = " ",Subclasses = subclasses,Iterations = iter,Initilization = init,Hypothesis = hypothesis)
       extra  <- subclasses
    }

    if (familytype == "pca"){
        functionName = "FLPCA"
        vinputcols = c(vinputcols, WhereClause = " ", GroupBy = 'NULL', MatrixType = matrixtype, TableOutput = 1)
        vclass <- "FLPCA"
    }

    if(familytype %in% c("lda", "Mixed", "Flex"))
        vinputcols <- c(vinputcols, NOTE = "")

    ret <- sqlStoredProc(connection,
                         functionName,
                         pInputParams = vinputcols,
                         outputParameter = c(OutTable = 'a')
                         )
    return(new(vclass,
               formula=formula,
               AnalysisID = as.character(ret[1,1]),
               table=data,
               results=list(call=callObject,
                            familytype = familytype,
                            extra = extra),
               ##AnalysisID = as.character(ret[1,1])),
               deeptable=deepx,
               mapTable=mapTable,
               scoreTable="",
               offset=as.character(offset)))
}



#' @export
`$.FLLDA`<-function(object,property){
                                        #parentObject <- deparse(substitute(object))
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]
    var <- getVariables(object@deeptable)

    if(property=="scaling"){
        str <- paste0("SELECT VarID, CanID, Num_Val FROM fzzlLDACanCoeff WHERE AnalysisID = ",fquote(object@AnalysisID)," AND CanType = 'Within-Class Can Struct' ORDER BY VarID, CanID ")
        df <- sqlQuery(connection ,str)
        nvar <- length(unique(df$CANID))
        dtf <- as.data.frame(lapply(1:nvar, function(x){
            df$NUM_VAL[df$CANID == x]
        }), ncol = nvar)
        dtf <- as.matrix.data.frame(dtf)
        colnames(dtf) <- paste0("LD",1:nvar)
        return(dtf)
    }
    else if (property=="coefficients"){
        cof <- coefficients(object)
        return(cof)
        
    }
    else if (property == "N"){
        return(length(object@deeptable@Dimnames[[1]]))
    }
    else if (property == "call"){
        return(object@results$call)}
    else if (property == "counts"){
        vcount <- sqlQuery(connection, paste0("SELECT ",var[[3]]," AS id, count(*) AS val FROM ",object@deeptable@select@table_name," WHERE ",var[[2]]," = -1 GROUP BY ",var[[3]]," ORDER BY ",var[[3]]," "))
        vect <- vcount$val
        names(vect) <- vcount$id
        return(vect)       
    }
    else if (property == "means")
    {
        if(object@results$familytype %in% c("lda", "Mixed"))
        {
            str <- paste0("SELECT FLMean(d.",var[[3]],") as means, d.",var[[2]]," AS VarID, c.val  FROM ",object@deeptable@select@table_name," d, (SELECT ",var[[1]]," AS ObsID, ",var[[3]]," AS val FROM tbllda b WHERE b.",var[[2]]," = -1) AS
c WHERE d.",var[[1]]," = c.ObsID AND d.",var[[2]]," <> -1 GROUP BY c.val, d.",var[[2]]," ORDER BY d.",var[[2]],", c.val ")
            df <- sqlQuery(connection, str)
            var <- unique(df$val)
                                        #browser()
            dtf <- t(sapply(var, function(x){df$means[df$val == x]}))
            rownames(dtf) <- as.character(var)
            colnames(dtf) <- as.character(object@deeptable@Dimnames[[2]][-1])
            return(dtf)}
        else {
            
            str <- paste0("SELECT * FROM fzzlFDAThetaMeans WHERE AnalysisID = '",object@AnalysisID,"' ORDER BY Col_ID, Row_ID")
            dtf <- sqlQuery(connection, str)
            var <- unique(dtf$Col_ID)
            df <- sapply(var, function(x){
                dtf$Num_Val[dtf$Col_ID == x]})
            rownames(df) <- object$lev
            return(df)    
        }
        
    }
    
    else if (property == "lev"){
        level <- sqlQuery(connection, paste0("SELECT DISTINCT ",var[[3]]," AS val FROM ",object@deeptable@select@table_name," WHERE ",var[[2]]," = -1 ORDER BY ",var[[3]]," "))
        return(as.character(level$val))
    }
    else if (property == "xlevels")
        return(NULL)
    else if (property == "prior")
    {
        return(object$counts/object$N)
    }
    else if (property == "confusion")
    {
        if(object@results$familytype %in% "Flex")
        {
            str <- paste0("SELECT * FROM fzzlFDAConfusionMtx WHERE AnalysisID = '",object@AnalysisID,"'ORDER BY PredictedClass, ActualClass ")
            dtf <- sqlQuery(connection, str)
            var <- as.integer(max(object$lev))
            df <- as.data.frame(lapply(1:var, function(x){
                dtf$Num_Val[dtf$ActualClass == x]
            }), ncol = var)
            rownames(df) <- 1:var
            colnames(df) <- 1:var
            return(df)
        }
        else return(NULL)
    }
    else if(property == "weights")
    {
        if(object@results$familytype == "Mixed"){
            str <- paste0("SELECT * FROM fzzlMDAWeight WHERE AnalysisID = '",object@AnalysisID,"' AND HypothesisID = 1 ORDER BY 3,2,4")
            dtf <- sqlQuery(connection, str)
            dtf <- dtf[, 2:6]
            var <- as.integer(max(object$lev))
            vsub <- object@results$extra
            dl <- list()
            for(i in seq(from = 0 , to  = var))
                dl[i] <- sapply(1:vsub, function(x){
                    dtf$Weight[dtf$SubclassID == x][dtf$ClassID == i]})
            colnames(df) <- paste0("s", 1:vsub)
            return(dl)
        }
    }

}



coefficients.FLLDA <- function(object)
{
    if( object@results$familytype %in% "lda"){
        str <- paste0("SELECT VarID, CanID, Num_Val FROM fzzlLDACanCoeff WHERE AnalysisID = ",fquote(object@AnalysisID)," AND CanType = 'Raw Canonical Coefficients' ORDER BY VarID, CanID ")
        df <- sqlQuery(connection ,str)
        nvar <- length(unique(df$CANID))
        dtf <- data.frame(lapply(1:nvar, function(x){
            df$NUM_VAL[df$CANID == x]
        }))
        dtf <- as.matrix.data.frame(dtf)
        colnames(dtf) <- paste0("LD",1:nvar)  
        return(dtf)}
    else
    {
        vID <- object@AnalysisID
        str <- paste0("SELECT * FROM fzzlFDARegrCoeffs WHERE AnalysisID = '",vID,"' ORDER BY 2,3 ")
        df <- sqlQuery(connection, str)
        nvar <- 2
        dtf <- data.frame(lapply(1:nvar, function(x){
            df$CoeffEst[df$DepVarID == x]
        }))
        dtf <- as.matrix.data.frame(dtf)
        colnames(dtf) <- 1:nvar
        rownames(dtf) <- 1:length(dtf[,1])
        return(dtf)   
    }
    
}
##posterior probablity:https://www.quora.com/Mathematical-Modeling-How-are-posterior-probabilities-calculated-in-linear-discriminant-analysis
## http://sites.stat.psu.edu/~jiali/course/stat597e/notes2/lda.pdf
predict.FLLDA <- function(object){
    var <- getVariables(object@deeptable)
    if(object@results$familytype %in% "Flex")
    {
        vinputcols <- list()
        tblname <- gen_unique_table_name("flexscore")
        
        ret <- sqlStoredProc(connection,"FLFlexDiscriminantScore",
                             TableName = object@deeptable@select@table_name,
                             ObsIDCol = var[[1]],
                             VarIDCol = var[[2]],
                             ValueCol = var[[3]],
                             WhereClause = "WHERE VarID >= 0",
                             InAnalysisID = object@AnalysisID,
                             ScoreTable = tblname,
                             Note = "",
                             outputParameter = c(OutTable = 'a')                            
                             )
        str <- paste0("SELECT PredictedClass AS predicted FROM ",tblname," ORDER BY 1 ")
        df <- sqlQuery(connection,str)
        rownames(df) <- object@deeptable@Dimnames[[1]]
        return(df)}
    else if (object@results$familytype %in% "lda"){
        
        str <- paste0("SELECT a.",var[[1]]," , a.",var[[3]]," AS LD1, b.",var[[3]]," AS LD2  FROM fzzlLDACanVariate a, (SELECT * FROM fzzlLDACanVariate) b WHERE a.",var[[1]]," = b.",var[[1]],"   AND a.",var[[2]]," = 1 AND b.",var[[2]]," = 2 AND a.",var[[1]]," < 3000  AND a.AnalysisID = b.AnalysisID AND a.AnalysisID = '",object@AnalysisID,"' ORDER BY a.",var[[1]]," ")
        ##
        ##str <- paste0("SELECT * FROM fzzlLDACanVariate WHERE AnalysisID = '",object@AnalysisID,"' AND ",var[[1]]," < 100 ORDER BY ",var[[1]]," ")
        x <- sqlQuery(connection, str)
        post <- NULL
        cl <- NULL
        list(x = x, class = cl, posterior = post)    }

    else if (object@results$familytype %in% "Mixed"){
        str <- paste0("SELECT ClassID FROM fzzlMDAClassify WHERE AnalysisID = '",object@AnalysisID,"' AND HypothesisID = 1 ORDER BY ClassID")
        dtf <- sqlQuery(connection , str)
        dtf <- as.integer(dtf$ClassID)
        names(dtf) <- flmod@deeptable@Dimnames[[1]]
        return(dtf)      
    }
}




confusion.FLLDA <- function(object){
    if(object@results$familytype %in% "Flex")
        return(object$confusion)
    else
        return(NULL)
}

plot.FLLDA <- function(object){
    if(object@results$familytype %in% "lda")
        val <- predict(object)$x
    val <- val[, 2:3]
    plot(val)
}


