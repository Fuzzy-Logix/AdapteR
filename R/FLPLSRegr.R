

##library(pls)
## deeptbl  <- FLTable("tblPLSDeep2y", "ObsID", "VarID", "Num_Val")
## rtbl <- as.R(deeptbl)
## names(rtbl) <- letters[1:16]
## flmod<- pls(A~., data =deeptbl, nfactor = 15 )
## rmod <- mvr(a~., data = rtbl)




#' @export
setClass(
	"FLPLSRegr",
	contains="FLRegr",
	slots=list(offset="character",
				vfcalls="character"))



## pls Regression.

mvr.default <- pls::mvr

pls <- function(formula, data, nfactor, ...)
{
    print(match.call())
    if(is.FL(data))    {
        vcallObject <- match.call()
        data <- setAlias(data,"")

        return(plsGeneric(formula=formula,
                        data=data,
                        callObject=vcallObject,
                        familytype="pls",
                        nfactor,
                        ...))
    }
}



plsGeneric <- function(formula, data,callObject, familytype = "pls", nfactor,nofortho = NULL,...)
{
    prepData <- prepareData.lmGeneric(formula,
                                      data,
                                      callObject=callObject,
                                      familytype = familytype
                                      )
    
    for(i in names(prepData))
        assign(i,prepData[[i]])
    deeptable <- deepx@select@table_name
    vfcalls <- c(functionName="FLPLSRegr",
                 infotableName="fzzlPLSRegrInfo",
                 note="plsregr",
                 coefftablename="fzzlPLSRegrCentCoeffs",
                 statstablename="fzzlPLSRegrConvVec"                                                                        )



    functionName <- vfcalls["functionName"]
    infotableName <- vfcalls["infotableName"]
    vnote <- genNote(vfcalls["note"])
    coefftablename <- vfcalls["coefftablename"]
    statstablename <- vfcalls["statstablename"]

    vinputCols <- list()
    vinputCols <- c(vinputCols,
                    INPUT_TABLE=deeptable,
                    OBSID_COL=getVariables(deepx)[["obs_id_colname"]],
                    VARID_COL=getVariables(deepx)[["var_id_colname"]],
                    VALUE_COL=getVariables(deepx)[["cell_val_colname"]]
                    )

    if(familytype %in% "pls")
    {
        functionName <- "FLLinRegr"
        vinputCols <- c(vinputCols,
                        NumOfFactors = nfactor)
    }
    vfuncName <- "FLPLSRegr"
    
    
    vinputCols <- c(vinputCols,
                    NOTE=vnote)

    retobj <- sqlStoredProc(getFLConnection(),
                            vfuncName,
                            outputParameter=c(AnalysisID="a"),
                            pInputParams=vinputCols
                            )
    retobj <- checkSqlQueryOutput(retobj)
    AnalysisID <- as.character(retobj[1,1])
    vmaxModelID <- NULL
    vmaxLevelID <- NULL

    return(new(vfuncName,
               formula=formula,
               AnalysisID=AnalysisID,
               wideToDeepAnalysisId=wideToDeepAnalysisId,
               table=data,
               results=list(call=callObject
                            ),
               deeptable=deepx,
               mapTable=mapTable,
               scoreTable="",
               vfcalls=vfcalls,
               offset=as.character(offset),
               RegrDataPrepSpecs=RegrDataPrepSpecs))   
}



## move to file lm.R
#' @export
`$.FLPLSRegr`<-function(object,property){
                                        #parentObject <- deparse(substitute(object))
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

    if(property=="Xmeans"){
        str <- paste0("SELECT FLmean(a.",getVariables(object@deeptable)$cell_val_colname,") AS Xmeans
                       FROM ",object@deeptable@select@table_name," a
WHERE a.",getVariables(object@deeptable)$var_id_colname," > -1
GROUP BY a.",getVariables(object@deeptable)$var_id_colname,"")
        mval <- sqlQuery(connection, str)
        return(mval)
    }
    else if(property == "Ymeans"){
        str <- paste0("SELECT FLmean(a.",getVariables(object@deeptable)$cell_val_colname,") AS Ymeans
        FROM ",object@deeptable@select@table_name," a
                           WHERE a.",getVariables(object@deeptable)$var_id_colname," = -1
                           GROUP BY a.",getVariables(object@deeptable)$var_id_colname,"")
            mval <- sqlQuery(connection, str)
            return(mval)
        }

}




#coefficients.FLPLSRegr <- 

