setClass(
    "FLLDA",
    contains="FLRegr",
    slots=list(offset="character",
               vfcalls="character"))

#' @export
#' library(mda)
#'deeptbl <- FLTable("tblIrisDeep", "ObsID", "VarID", "Num_Val")
#' flmod <- fda(a~., data = deeptbl)
## Flexible DA.

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


## LDA function.

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



#'tbl <- FLTable("tblLDA", "OBSID", "VARID", "NUM_VAL")
#'flmod <- lda(a~. , data = tbl)
#' flmod$scaling, flmod$means



## Generic function for DA.
ldaGeneric <- function(formula,data,
                       callObject=NULL,
                       familytype = "",
                       MaxMARS = MaxMARS,
                       MinRsq = MinRsq,
                       ...)
    
{
    
    prepData <- prepareData.lmGeneric(formula,data,
                                      callObject=callObject,
                                      familytype=familytype,
                                      performNorm=1,
                                      cost = 1,
                                      ...)
    
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
    
    if(familytype == "Flex")
    {
        functionName = "FLFlexDiscriminant"
        vinputcols = c(vinputcols, MaxBasisMARS = MaxMARS, MinDeltaRsqMARS =MinRsq)     
        
    }
    
    vinputcols <- c(vinputcols, NOTE = "")
    ret <- sqlStoredProc(connection,
                         functionName,
                         pInputParams = vinputcols,
                         outputParameter = c(OutTable = 'a')
                         )
    return(new("FLLDA",
               formula=formula,
               AnalysisID = as.character(ret[1,1]),
               table=data,
               results=list(call=callObject,
                            familytype = familytype),
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
        if(object@results$familytype %in% "lda")
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
    else{
        
        str <- paste0("SELECT a.",var[[1]]," , a.",var[[3]]," AS LD1, b.",var[[3]]," AS LD2  FROM fzzlLDACanVariate a, (SELECT * FROM fzzlLDACanVariate) b WHERE a.",var[[1]]," = b.",var[[1]],"   AND a.",var[[2]]," = 1 AND b.",var[[2]]," = 2 AND a.",var[[1]]," < 3000  AND a.AnalysisID = b.AnalysisID AND a.AnalysisID = '",object@AnalysisID,"' ORDER BY a.",var[[1]]," ")
        ##
        ##str <- paste0("SELECT * FROM fzzlLDACanVariate WHERE AnalysisID = '",object@AnalysisID,"' AND ",var[[1]]," < 100 ORDER BY ",var[[1]]," ")
        x <- sqlQuery(connection, str)
        post <- NULL
        cl <- NULL
        list(x = x, class = cl, posterior = post)    }
}




confusion.FLLDA <- function(object){
    if(object@results$familytype %in% "Flex")
        return(object$confusion)
    else
        return(NULL)
}

plot.FLLDA <- function(object){
    if(object@results$familytype %in% "LDA")
        val <- predict(object)$x
    val <- val[, 2:3]
    plot(val)
}


