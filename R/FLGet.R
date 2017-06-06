getAlias <- function(object){
  return(names(getTableNameSlot(object)))
}
getObsIdColname <- function(object){
  if(isDeep(object) && ncol(object)>1)
  return("var_id_colname")
  else return("obs_id_colname")
}

## gk @ phani:  the vmapping is dangerous for platfrom independence.  Need to discuss this.
## returns INT for integers or bool,VARCHAR(255)
## for characters and FLOAT for numeric
getFLColumnType <- function(x,columnName=NULL){
    # if(!is.FL(x)) stop("Input is not FL object. Use typeof. \n ")
    vmapping <- c(VARCHAR="character",
                  INT="integer",
                  FLOAT="double",
                  FLOAT="numeric",
                  VARCHAR="logical")
    vresult <- names(vmapping)[vmapping==typeof(x)]
    if(vresult=="VARCHAR") 
    vresult <- "VARCHAR(255)"
    return(vresult)
}

#' @export
setGeneric("typeof",function(x)
      standardGeneric("typeof"))
setMethod("typeof",signature(x="ANY"),
      function(x){
        return (base::typeof(x))
        })
setMethod("typeof",signature(x="FLIndexedValues"),
      function(x){
        return(x@type)
        })
setMethod("typeof",signature(x="FLVector"),
      function(x){
        vtype <- x@type
        if(any(is.na(vtype))){
            warning("type is NA, lost -- setting to double")
            vtype <- "double"
        }
        if(length(vtype)>1){
          if("character" %in% vtype)
            vtype <- "character"
          else if("double" %in% vtype)
            vtype <- "double"
          else if("integer" %in% vtype)
            vtype <- "integer"
          else vtype <- "logical"
        }
        return(vtype)
      })
setMethod("typeof",signature(x="FLSimpleVector"),
    function(x){
        class(x) <- "FLVector"
        return(typeof(x))
        })
setMethod("typeof",signature(x="FLTable"),
      function(x){
        if(isDeep(x)){
          vValCol <- getVariables(x)[["cell_val_colname"]]
          vValCol <- changeAlias(vValCol,"","")
          vtype <- x@type[vValCol]
          if(is.na(vtype) || is.null(vtype))
            vtype <- x@type[1]
          names(vtype) <- vValCol
        }
        else{
          if(length(x@type)==1)
            vtype <- rep(x@type,ncol(x))
          else vtype <- x@type
          if(is.null(names(vtype)))
            names(vtype) <- colnames(x)
        }
        return(vtype)
      })

setGeneric("getGroupIdSQLExpression",function(object)
      standardGeneric("getGroupIdSQLExpression"))
setMethod("getGroupIdSQLExpression",signature(object="FLTable"),
      function(object){
        return(NULL)
        })
setMethod("getGroupIdSQLExpression",signature(object="FLTableMD"),
      function(object){
        return(getIndexSQLExpression(object,1))
        })

setGeneric("getObsIdSQLExpression",function(object)
      standardGeneric("getObsIdSQLExpression"))
setMethod("getObsIdSQLExpression",signature(object="FLTable"),
      function(object){
        return(getIndexSQLExpression(object,1))
        })
setMethod("getObsIdSQLExpression",signature(object="FLTableMD"),
      function(object){
        return(getIndexSQLExpression(object,2))
        })
setMethod("getObsIdSQLExpression",signature(object="FLIndexedValues"),
      function(object){
        return(getIndexSQLExpression(object,1))
        })
setGeneric("getVarIdSQLExpression",function(object)
      standardGeneric("getVarIdSQLExpression"))
setMethod("getVarIdSQLExpression",signature(object="FLTable"),
      function(object){
        return(getIndexSQLExpression(object,2))
        })
setMethod("getVarIdSQLExpression",signature(object="FLTableMD"),
      function(object){
        return(getIndexSQLExpression(object,3))
        })
setMethod("getVarIdSQLExpression",signature(object="FLIndexedValues"),
      function(object){
        return(getIndexSQLExpression(object,2))
        })

setMethod("getValueSQLExpression",signature(object="FLTable"),
      function(object){
        return(getIndexSQLExpression(object,3))
        })
setMethod("getValueSQLExpression",signature(object="FLTableMD"),
      function(object){
        return(getIndexSQLExpression(object,4))
        })

setGeneric("getGroupIdSQLName",function(object)
      standardGeneric("getGroupIdSQLName"))
setMethod("getGroupIdSQLName",signature(object="FLTable"),
      function(object){
        return(NULL)
        })
setMethod("getGroupIdSQLName",signature(object="FLTableMD"),
      function(object){
        return(getIndexSQLName(object,1))
        })

setGeneric("getObsIdSQLName",function(object)
      standardGeneric("getObsIdSQLName"))
setMethod("getObsIdSQLName",signature(object="FLTable"),
      function(object){
        return(getIndexSQLName(object,1))
        })
setMethod("getObsIdSQLName",signature(object="FLMatrix"),
      function(object){
        return(getIndexSQLName(object,1))
        })
setMethod("getObsIdSQLName",signature(object="FLTableMD"),
      function(object){
        return(getIndexSQLName(object,2))
        })
setGeneric("getVarIdSQLName",function(object)
      standardGeneric("getVarIdSQLName"))
setMethod("getVarIdSQLName",signature(object="FLTable"),
      function(object){
        return(getIndexSQLName(object,2))
        })
setMethod("getVarIdSQLName",signature(object="FLMatrix"),
      function(object){
        return(getIndexSQLName(object,2))
        })
setMethod("getVarIdSQLName",signature(object="FLTableMD"),
      function(object){
        return(getIndexSQLName(object,3))
        })

setMethod("getValueSQLName",signature(object="FLTable"),
      function(object){
        return(getIndexSQLName(object,3))
        })
setMethod("getValueSQLName",signature(object="FLTableMD"),
      function(object){
        return(getIndexSQLName(object,4))
        })

## @phani: below functions need review.
## update to use dimColumns or deprecate
###########################################################################################
setGeneric("getValueColumn",function(object)
      standardGeneric("getValueColumn"))
setMethod("getValueColumn",signature(object="FLMatrix"),
      function(object){
        return(c(valueColumn=getVariables(object)[["valueColumn"]]))
        })
setMethod("getValueColumn",signature(object="FLVector"),
      function(object){
        if(isDeep(object))
        return(c(cell_val_colname=getVariables(object)[["cell_val_colname"]]))
        else{
          vtemp <- ""
          if(!is.null(getAlias(object)) && 
            getAlias(object)!="")
          vtemp <- paste0(getAlias(object),".")
          return(sapply(colnames(object),
                      function(x){
                        if(!grepl(vtemp,x))
                        return(paste0(vtemp,x))
                        else return(x)
                        }))
        }
      })

setMethod("getValueColumn",signature(object="FLTable"),
      function(object){
        if(isDeep(object))
        return(c(cell_val_colname=getVariables(object)[["cell_val_colname"]]))
        vtemp <- ""
        if(!is.null(getAlias(object)) && 
            getAlias(object)!="")
        vtemp <- paste0(getAlias(object),".")
        return(sapply(colnames(object),
                      function(x){
                        if(!grepl(vtemp,x))
                        return(paste0(vtemp,x))
                        else return(x)
                        }))
        })

setGeneric("getIdColname",function(object)
      standardGeneric("getIdColname"))
setMethod("getIdColname",signature(object="FLMatrix"),
      function(object){
        return("MATRIX_ID")
        })
setMethod("getIdColname",signature(object="FLVector"),
      function(object){
        return("vectorIdColumn")
        })
setMethod("getIdColname",signature(object="FLTable"),
      function(object){
        return("obs_id_colname")
        })

########################################################################################
genDeepFormula <- function(pColnames,
                          pDepColumn=NULL)
{
  if(is.null(pDepColumn)){
    suppressWarnings(
      if(any(is.na(as.numeric(pColnames))))
        stop("varID column must be numeric \n")
    )
    vcolnames <- as.numeric(pColnames)
    # if(!(-1 %in% vcolnames))
    # stop("-1 denoting dependent column must be present in colnames of deep table.\n")
    vcolnames <- paste0("var",vcolnames[!vcolnames %in% c(0,-1,-2)],collapse="+")
    vformula <- paste0("varY~",vcolnames)
  }
  else{
    pColnames <- paste0(pColnames,collapse="+")
    vformula <- paste0(pDepColumn,"~",pColnames)
  }
  return(as.formula(vformula))
}

getXMatrix <- function(object,
                       pDropCols=c(),
                       pColnames=NULL,
                       ...){
                                        #browser()
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),
                                                    "(",fixed=T))[2],",",fixed=T))[1]
    coeffVector <- object$coefficients
    vdroppedCols <- object@results[["droppedCols"]]
    modelframe <- object@deeptable
    vID <- object@results$mod[["nID"]]

  pDropCols <- unique(c(pDropCols,vdroppedCols))

  vobsidSQLName <- getObsIdSQLName(object@deeptable)
  vvaridSQLName <- getVarIdSQLName(object@deeptable)
  vvalueSQLName <- getValueSQLName(object@deeptable)

  if(length(pDropCols)>0)
  modelframe@select@whereconditions <- c(modelframe@select@whereconditions,
                  paste0(getVarIdSQLExpression(object@deeptable)," NOT IN ",
                    "(",paste0(pDropCols,collapse=","),
                    ")"))

  ## Takes care of cases  when varIds are dropped in step
  ## And when input deeptable is sparse
  # if(length(vdroppedCols)==0){
  #   vcurrColumns <- setdiff(colnames(modelframe),pDropCols)

    # varidoffset <- sapply(-2:0,function(x){
    #               if(all(x:0 %in% object@results[["CoeffID"]]))
    #                 x
    #               else NULL
    #           })
    # varidoffset <- unlist(varidoffset)
    # if(length(varidoffset)>0)
    #   varidoffset <- abs(min(varidoffset))+1
    # else varidoffset <- 0

    varidoffset <- 0
    if(min(object@results[[vID]])==0)
        varidoffset <- 1

  #   vsqlstr <- paste0("SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
  #             "obs_id_colname AS rowIdColumn,\n ",
  #             "var_id_colname ",
  #             ifelse(varidoffset==0,"",paste0("+",varidoffset)),
  #             " AS colIdColumn, \n ",
  #             "cell_val_colname AS valueColumn \n ",
  #               " FROM (",constructSelect(modelframe),") a \n "
  #             )
  # }
  # else{
  #   if(is.null(object@results[["varidMapTable"]])){
  #     vtablename <- gen_unique_table_name("varidMap")
  #     object@results <- c(object@results,list(varidMapTable=vtablename))
  #     createTable(pTableName=vtablename,
  #           pSelect=paste0("SELECT ROW_NUMBER()OVER(ORDER BY var_id_colname)",
  #                       " AS varid,var_id_colname AS varidold \n ",
  #                    " FROM (SELECT DISTINCT var_id_colname \n ",
  #                     " FROM (",constructSelect(modelframe),")a)a"))
  #   }
  #   else vtablename <- object@results[["varidMapTable"]]

    if(isContinuous(rownames(modelframe)))
        vrowidcolumn <- paste0("a.",vobsidSQLName)
    else vrowidcolumn <- paste0("DENSE_RANK()OVER(ORDER BY ",vobsidSQLName,")")

    if(is.null(object@results[[vID]])
        ||!isContinuous(object@results[[vID]]))
    vsqlstr <- paste0("SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
                            "CAST(",vrowidcolumn," AS INT) AS rowIdColumn, \n ",
                            "CAST(b.CoeffIDNew AS INT) AS colIdColumn, \n ",
                            "a.",vvalueSQLName," AS valueColumn \n ",
                      " FROM (",constructSelect(modelframe),") a, \n ",
                            "(SELECT ",vID,",ROW_NUMBER()over(order by ",vID,") AS CoeffIDNew \n ",
                            " FROM ",object@vfcalls["coefftablename"]," a \n ",
                            " WHERE a.AnalysisID = ",fquote(object@AnalysisID),
                                    ifelse(length(object@results[["modelID"]])>0 
                                            && object@vfcalls["functionName"]!= "FLRobustRegr",
                                        paste0("\n AND a.ModelID = ",
                                                object@results[["modelID"]]),""),
                            ") b \n ",
                      " WHERE b.",vID,"=a.",vvaridSQLName," "
                    )
    else
    vsqlstr <- paste0("SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
                            "CAST(",vrowidcolumn," AS INT) AS rowIdColumn, \n ",
                            "CAST(b.",vID," ",
                            ifelse(varidoffset==0,"",paste0("+",varidoffset))," AS INT) AS colIdColumn, \n ",
                            "a.",vvalueSQLName," AS valueColumn \n ",
                      " FROM (",constructSelect(modelframe),") a, \n ",
                            object@vfcalls["coefftablename"]," b \n ",
                      " WHERE b.AnalysisID = ",fquote(object@AnalysisID),
                            ifelse(length(object@results[["modelID"]])>0 
                                    && object@vfcalls["functionName"]!= "FLRobustRegr",
                                paste0("\n AND b.ModelID = ",object@results[["modelID"]]),""),
                            " AND b.",vID,"=a.",vvaridSQLName," "
                    )

    # vsqlstr <- paste0("SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
  #             "a.obs_id_colname AS rowIdColumn,\n ",
  #             "b.varid AS colIdColumn, \n ",
  #             "a.cell_val_colname AS valueColumn \n ",
  #           " FROM (",constructSelect(modelframe),") a, \n ",
  #               " (SELECT ROW_NUMBER()OVER(ORDER BY var_id_colname)",
  #                     " AS varid,var_id_colname AS varidold \n ",
  #                " FROM (SELECT DISTINCT var_id_colname \n ",
  #                 " FROM (",constructSelect(modelframe),")a)a) b \n ",
  #         " WHERE b.varidold=a.var_id_colname \n "
  #            )

  # }
  vselect <- new("FLTableFunctionQuery",
                connectionName = getFLConnectionName(object),
                variables=list(MATRIX_ID="MATRIX_ID",
                              rowIdColumn="rowIdColumn",
                              colIdColumn="colIdColumn",
                              valueColumn="valueColumn"),
                whereconditions="",
                SQLquery=vsqlstr)

  vallVars <- all.vars(object@formula)
  
  ## For LogRegrMN CoeffVector is Matrix
  if(!is.null(object@results[["XMatrixColnames"]]))
    vcolnames <- object@results[["XMatrixColnames"]]
  else if(!is.null(pColnames))
    vcolnames <- pColnames
  else{
    if(is.matrix(coeffVector)){
      vcolnames <- c("(Intercept)",colnames(coeffVector)[2:ncol(coeffVector)])
    }
    else vcolnames <- c("(Intercept)",names(coeffVector)[2:length(coeffVector)])
  }

  vdimnames <- list(rownames(modelframe),vcolnames)

  modelframe <- newFLMatrix(
                  select=vselect,
                  dims=as.integer(c(nrow(modelframe),
                                    length(vcolnames))),
                  Dimnames=list(NULL,vcolnames))
  #dimnames(modelframe) <- vdimnames

  # colnames(modelframe)[1] <- "Intercept"
  ## Do not store. Better to fetch each time as
  ## it saves memory and not much time loss in
  ## Fetching.
  assign(parentObject,object,envir=parent.frame())
  return(modelframe)
}

calcLinearPred <- function(object,...){
  parentObject <- unlist(strsplit(unlist(strsplit(
    as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
  vfit <- object$fitted.values
  if(object@vfcalls["functionName"]=="FLLogRegr"){
    vlinPred <- log(vfit)
  }
  else if(object@vfcalls["functionName"]=="FLPoissonRegr"){
    sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                          "a.vectorIndexColumn AS vectorIndexColumn, \n ",
                          " ln(a.vectorValueColumn/(1.0-a.vectorValueColumn)) ",
                          " AS vectorValueColumn \n ",
                      " FROM (",constructSelect(vfit),") a ")
    tblfunqueryobj <- new("FLTableFunctionQuery",
                        connectionName = attr(connection,"name"),
                        variables = list(
                        obs_id_colname = "vectorIndexColumn",
                        cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)
    flv <- newFLVector(
              select = tblfunqueryobj,
              Dimnames = dimnames(vfit),
              isDeep = FALSE)

    vlinPred <- ensureQuerySize(pResult=flv,
                                pInput=list(object,...),
                                pOperator="calcLinearPred")
  }
  assign(parentObject,object,envir=parent.frame())
  return(vlinPred)
}

calcResiduals <- function(object,
                          type = c("deviance", "pearson", "working", 
                                  "response", "partial"),
                          ...){
  vtype <- match.arg(type)
  vfit <- object$fitted.values
  vYVector <- object$y
  if(vtype=="partial")
    stop("partial type is not supported currently \n ")
  if(object@vfcalls["functionName"]=="FLLinRegr"
    || vtype=="response"|| object@vfcalls["functionName"] =="FLRobustRegr"||object@vfcalls["functionName"] =="FLPLSRegr"){
    sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                                "a.vectorIndexColumn AS vectorIndexColumn, \n ",
                                "(a.vectorValueColumn-b.vectorValueColumn)",
                                " AS vectorValueColumn \n ",
                      " FROM(",constructSelect(vYVector),") a, \n ",
                            "(",constructSelect(vfit),") b \n ",
                      " WHERE a.vectorIndexColumn=b.vectorIndexColumn ")
  }
  else if(object@vfcalls["functionName"]%in%c("FLLogRegr","FLLogRegrWt")){
    if(type=="deviance")
      sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                                "a.vectorIndexColumn AS vectorIndexColumn, \n ",
                                "CASE WHEN (a.vectorValueColumn<>1) THEN \n ",
                                "-1*sqrt(2*abs(ln(1-b.vectorValueColumn))) ELSE \n ",
                                "sqrt(2*abs(ln(b.vectorValueColumn))) END AS vectorValueColumn \n ",
                      " FROM(",constructSelect(vYVector),") a, \n ",
                            "(",constructSelect(vfit),") b \n ",
                      " WHERE a.vectorIndexColumn=b.vectorIndexColumn ")
    else if(type=="pearson")
      sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                                "a.vectorIndexColumn AS vectorIndexColumn, \n ",
                                "(a.vectorValueColumn-b.vectorValueColumn)/",
                                "(sqrt(b.vectorValueColumn*(1.0-b.vectorValueColumn)))",
                                " AS vectorValueColumn \n ",
                      " FROM(",constructSelect(vYVector),") a, \n ",
                            "(",constructSelect(vfit),") b \n ",
                      " WHERE a.vectorIndexColumn=b.vectorIndexColumn ")
    else if(type=="working")
      sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                                "a.vectorIndexColumn AS vectorIndexColumn, \n ",
                                "(a.vectorValueColumn-b.vectorValueColumn)/",
                                "(b.vectorValueColumn*(1.0-b.vectorValueColumn))",
                                " AS vectorValueColumn \n ",
                      " FROM(",constructSelect(vYVector),") a, \n ",
                            "(",constructSelect(vfit),") b \n ",
                      " WHERE a.vectorIndexColumn=b.vectorIndexColumn ")
  }
  else if(object@vfcalls["functionName"]=="FLPoissonRegr"){
    if(type=="deviance")
      sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                                "a.vectorIndexColumn AS vectorIndexColumn, \n ",
                                "CASE WHEN (a.vectorValueColumn==0) THEN \n ",
                                "sqrt(2 *b.vectorValueColumn)*(-1) ELSE \n ",
                                "sqrt(2 * (a.vectorValueColumn * ln(a.vectorValueColumn/b.vectorValueColumn)",
                                  " - (a.vectorValueColumn - b.vectorValueColumn)))",
                                  " *((a.vectorValueColumn-b.vectorValueColumn)/",
                                    "abs(a.vectorValueColumn-b.vectorValueColumn)) END AS vectorValueColumn \n ",
                      " FROM(",constructSelect(vYVector),") a, \n ",
                            "(",constructSelect(vfit),") b \n ",
                      " WHERE a.vectorIndexColumn=b.vectorIndexColumn ")
    else if(type=="pearson")
      sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                                "a.vectorIndexColumn AS vectorIndexColumn, \n ",
                                "(a.vectorValueColumn-b.vectorValueColumn)/",
                                "sqrt(b.vectorValueColumn) AS vectorValueColumn \n ",
                      " FROM(",constructSelect(vYVector),") a, \n ",
                            "(",constructSelect(vfit),") b \n ",
                      " WHERE a.vectorIndexColumn=b.vectorIndexColumn ")
    else if(type=="working")
      sqlstr <- paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                                "a.vectorIndexColumn AS vectorIndexColumn, \n ",
                                "(a.vectorValueColumn-b.vectorValueColumn)/",
                                "b.vectorValueColumn AS vectorValueColumn \n ",
                      " FROM(",constructSelect(vYVector),") a, \n ",
                            "(",constructSelect(vfit),") b \n ",
                      " WHERE a.vectorIndexColumn=b.vectorIndexColumn ")
  }

  tblfunqueryobj <- new("FLTableFunctionQuery",
                        connectionName = attr(connection,"name"),
                        variables = list(
                        obs_id_colname = "vectorIndexColumn",
                        cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)
  
  flv <- newFLVector(
              select = tblfunqueryobj,
              Dimnames = dimnames(vfit),
              dims = vfit@dims,
              isDeep = FALSE)

  vresidVector <- ensureQuerySize(pResult=flv,
                                  pInput=list(object,type,...),
                                  pOperator="calcResiduals")

  parentObject <- unlist(strsplit(unlist(strsplit(
                  as.character(sys.call()),"(",fixed=T))[2],")",fixed=T))[1]
  assign(parentObject,object,envir=parent.frame())
  return(vresidVector)
}

getArithmeticType <- function(pObj1,pObj2,pOperator){
  if(missing(pObj2))
    pObj2 <- 1
  vcompvector <- c("==",">","<",">=","<=","!=")
  if(pOperator %in% vcompvector)
    return("logical")
  vtype <- c(typeof(pObj1),typeof(pObj2))
  if("character" %in% vtype)
    return("character")
  else if("double" %in% vtype)
    return("double")
  else if("integer" %in% vtype 
        && pOperator %in% c("+","-","*","%*%"))
    return("integer")
  else if(all(vtype=="logical"))
    return("logical")
  else return("double")
}


## May use FLMod in future.
## https://app.asana.com/0/98264711960805/148450351472400
## FLMod related issues
## @phani:- connection based platform dispatch
getMODSQL <- function(pConnection=getFLConnection(),
                    pColumn1,pColumn2){
    if(is.TD(pConnection))
        return(paste0(" ",pColumn1," MOD ",pColumn2," "))
    else if(is.TDAster(pConnection))
        return(paste0(" MOD(",pColumn1,",",pColumn2,") "))
    else if(is.Hadoop(pConnection))
        return(paste0(" ",pColumn1,"%",pColumn2," "))
}


## Mapping of R Types to in-DB Types
getRToFLDataTypeMap <- function(pRType){
    vnames <- names(pRType)
    pRType[pRType=="character"] <- "VARCHAR(255)"
    pRType[pRType=="numeric"] <- "FLOAT"
    pRType[pRType=="integer"] <- "INT"
    pRType[pRType=="logical"] <- "VARCHAR(255)"
    if(!all(pRType %in% c("VARCHAR(255)","INT","FLOAT"))==TRUE)
    stop("currently class(colnames(object)) can be only character,numeric,integer. Use casting if possible \n ")
    names(pRType) <- vnames
    # return(pRType)
    return(getFLPlatformDataTypeMap(pRType))
}
getFLPlatformDataTypeMap <- function(pFLType){
    vnames <- names(pFLType)
    vtypeMap <- list(TD=c(INT="INT",BYTEINT="BYTEINT",
                        "VARCHAR(255)"="VARCHAR(255)",
                        FLOAT="FLOAT",BIGINT="BIGINT"),
                    TDAster=c(INT="INT",BYTEINT="BYTEA",
                        "VARCHAR(255)"="VARCHAR(255)",
                        FLOAT="FLOAT"),
                    Hadoop=c(INT="INT",BYTEINT="TINYINT",
                        "VARCHAR(255)"="VARCHAR(255)",
                        FLOAT="FLOAT"))
    if(!is.null(pFLType)){
        pFLType <- vtypeMap[[getFLPlatform()]][pFLType]
        names(pFLType) <- vnames
    }
    return(pFLType)
}

getFLVectorTableFunctionQuerySQL <- function(idColumn="'%insertIDhere%'",
                                            indexColumn,
                                            valueColumn,
                                            FromTable){
    return(paste0(" SELECT ",idColumn," AS vectorIdColumn,",
                            indexColumn," AS vectorIndexColumn,",
                            valueColumn," AS vectorValueColumn",
                    " FROM ",FromTable))
}

#' @export
getTestTableName <- function(tableName){
    getRemoteTableName(databaseName=getOption("TestDatabase")[getFLPlatform()],
                        tableName=tableName,
                        temporaryTable=FALSE)
}

getNativeRandFunction <- function(...){
    vfuncName <- getStoredProcMapping("RANDOM")$funcNamePlatform
    vinputArgNames <- names(getStoredProcMapping("RANDOM")$argsPlatform)
    return(paste0(vfuncName,
                "(",paste0(list(...)[vinputArgNames],
                            collapse=","),")")
            )
}
