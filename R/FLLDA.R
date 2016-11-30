
setClass(
    "FLLDA",
    contains="FLRegr",
    slots=list(offset="character",
               vfcalls="character"))

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
                      familytype="",
                      ...))
}

#' @export
lda.FLTable <- lda.FLpreparedData


lda.FLTableMD <- lda.FLpreparedData



#'tbl <- FLTable("tblLDA", "OBSID")
#'flmod <- lda(a~. , data = tbl)
#' flmod$scaling, flmod$means, flmod$



## Generic function for LDA.
ldaGeneric <- function(formula,data,
                       callObject=NULL,
                       familytype = "",
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
    ret <- sqlStoredProc(connection,
                         functionName,
                         TableName = deeptable,
                         ObsIDCol = var[[1]],
                         VarIDCol = var[[2]],
                         ValueCol = var[[3]],
                         NOTE = "",
                         outputParameter = c(OutTable = 'a')
                         )
    return(new("FLLDA",
               formula=formula,
               table=data,
               results=list(call=callObject,
                            AnalysisID = ret),
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
        str <- paste0("SELECT VarID, CanID, Num_Val FROM fzzlLDACanCoeff WHERE AnalysisID = ",fquote(object@results$AnalysisID)," AND CanType = 'Within-Class Can Struct' ORDER BY VarID, CanID ")
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
        str <- paste0("SELECT FLMean(d.",var[[3]],") as means, d.",var[[2]]," AS VarID, c.val  FROM ",object@deeptable@select@table_name," d, (SELECT ",var[[1]]," AS ObsID, ",var[[3]]," AS val FROM tbllda b WHERE b.",var[[2]]," = -1) AS
c WHERE d.",var[[1]]," = c.ObsID AND d.",var[[2]]," <> -1 GROUP BY c.val, d.",var[[2]]," ORDER BY d.",var[[2]],", c.val ")
        df <- sqlQuery(connection, str)
        var <- unique(df$val)
        #browser()
        dtf <- t(sapply(var, function(x){df$means[df$val == x]}))
        rownames(dtf) <- as.character(var)
        colnames(dtf) <- as.character(object@deeptable@Dimnames[[2]][-1])
        return(dtf)
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
    
}



coefficients.FLLDA <- function(object)
{
    str <- paste0("SELECT VarID, CanID, Num_Val FROM fzzlLDACanCoeff WHERE AnalysisID = ",fquote(object@results$AnalysisID)," AND CanType = 'Raw Canonical Coefficients' ORDER BY VarID, CanID ")
    df <- sqlQuery(connection ,str)
    nvar <- length(unique(df$CANID))
    dtf <- data.frame(lapply(1:nvar, function(x){
        df$NUM_VAL[df$CANID == x]
    }))
    dtf <- as.matrix.data.frame(dtf)
    colnames(dtf) <- paste0("LD",1:nvar)  
    return(dtf)
}






