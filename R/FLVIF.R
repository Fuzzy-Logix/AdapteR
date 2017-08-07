#' @export
setClass(
    "FLVIF",
    slots = list(results = "list",
                 table = "FLTable"
                 ) )

#' @export
vif <- function (formula=NULL,data=list(),...) {
    if((!is.formula(formula) && !is.null(formula)) || !is.FL(data))
        return(car::vif(formula,data=data,...))
    else UseMethod("vif", data)
}


#' \code{vif} performs VIF on FLTable and FLTableMD objects.
#' The DB Lytix function which can be called are
#' c(FLVIF,FLVIFBW,FLVIFFB,FLVIFMultiDataSet,FLVIFBWMultiDataSet,FLVIFFBMultiDataSet).
#' Performs variance inflation factor analysis on data to identify
#' redundant variables in a dataset. The square root of the variance inflation factor
#' tells you how much larger the standard error is, compared with what it would be if
#' that variable were uncorrelated with the other predictor variables in the model.
#' 
#' @seealso \code{\link[vif]{vif}} for R reference implementation.
#' @param formula A symbolic description of model variables for which vif is to be calculated.
#' @param data An object of class FLTable or FLTableMD.
#' @param method  three methods are Available 'normal' for VIF , 'bw' for backward selections on independent variable and 'fw' for fast backward selections on independent variables 
#' @param threshold  One variable is dropped at a time till all of the VIF values in the
#' model are below the VIFThresholdNumber.

#' @slot results cache list of results computed
#' @slot table Input data object
#' @return \code{vif} returns an object of class \code{FLVIF}
#' @examples
#' fltbl <- FLTable(table = "tbllinregr", obs_id_colname="OBSID", var_id_colnames="VARID", "NUM_VAL")
#' flmod <- vif.FLTable(data = fltbl)
#' flmod$vif
#' flmod <- vif.FLTable(data = fltbl, method = "fb")
#' flmod <- vif.FLTable(data = fltbl, method = "bw")
#' For Multi-Dataset:
#' fltbl <- FLTableMD(table = "tblLogRegrMulti",group_id_colname="DATASETID",obs_id_colname="ObsID",var_id_colname="VarID",cell_val_colname="Num_Val")
#' flmod <- vif(data= fltbl, method = "bw", threshold = 5)
#' flmod$vif
#' @export
vif.FLTable <- function(formula, data, fetchID = TRUE,method = "normal",threshold = c(2,10),...)
{
    vcallObject <- match.call()
    deeptblname <- gen_unique_table_name("vif")
    vdeeptbl <- data
    vfun <- list(Multi = c("FLVIFMultiDataSet",
                           "FLVIFBWMultiDataSet",
                           "FLVIFFBMultiDataSet" ),
                 NonMulti = c("FLVIF",
                              "FLVIFBW",
                              "FLVIFFB"))
    if(is.FLTableMD(data)){
        vvar <- "Multi"
        functionName <- vfun[[vvar]][1]
    }
    else{
        vvar <- "NonMulti"
        functionName <- vfun[[vvar]][1]
    }
    
        
    if(!isDeep(data))
    {
        FLdeep <- prepareData(formula         = formula ,
                              data            = data,
                              outDeepTable    = deeptblname,
                              makeDataSparse  = 1,
                              performVarReduc = 0,
                              minStdDev       = .01,
                              perfromNorm = 1,     
                              maxCorrel       = .8,
                              fetchIDs        = FALSE
                              )
        vdeeptbl <- FLdeep$deepx
        cnames <- c(TableName =  FLdeep$deepx@select@table_name[[1]])

        vmapping <- list("1"=FLdeep$vmapping)
        if(is.FLTableMD(data)){
            cnames <- c(cnames,
                        GroupID = FLdeep$deepx@select@variables$group_id_colname)
        }
        cnames <- c(cnames,
                    ObsIDCol = FLdeep$deepx@select@variables$obs_id_colname,
                    VarIDCol = FLdeep$deepx@select@variables$var_id_colname,
                    ValueCol= FLdeep$deepx@select@variables$cell_val_colname) 
    }
    else
    { cnames <- c(TableName =  data@select@table_name[[1]])
        if(is.FLTableMD(data)){
            cnames <- c(cnames,
                        GroupID =gsub("flt.", "", data@select@variables$group_id_colname) )
            vmapping <- lapply(colnames(data),
                                function(x){
                                    x <- setdiff(x,c(0,-1))
                                    vmap <- x
                                    names(vmap) <- paste0("var",x)
                                    vmap
                                    })
        }
        else{
            vmapping <- lapply(list("1"=colnames(data)),
                            function(x){
                                x <- setdiff(x,c(0,-1))
                                vmap <- x
                                names(vmap) <- paste0("var",x)
                                vmap
                                })
        }
        cnames <- c(cnames,
                    ObsIDCol = gsub("flt.", "",data@select@variables$obs_id_colname),
                    VarIDCol = gsub("flt.", "",data@select@variables$var_id_colname),
                    ValueCol= gsub("flt.", "",data@select@variables$cell_val_colname))
    }

    vstat <- "fzzlvifstats"
    data <- setAlias(data,"")

    
    if(method == "bw"){
        functionName = vfun[[vvar]][2]
        cnames <- c(cnames,
                    VIFThreshold = threshold[1]
                    )
    vstat <- "fzzlVIFBWStats"}

    if(method == "fb"){
        functionName = vfun[[vvar]][3]
        if(length(threshold) == 2)
            cnames <- c(cnames,
                    VIFThreshold1 = threshold[2],
                    VIFThreshold2 = threshold[1]
                    )
        else
            cnames <- c(cnames,
                        VIFThreshold1 = 10,
                        VIFThreshold2 = 5
                        )
        vstat <- "fzzlVIFBWStats"}
    ##    vmap <- FLdeep$vmapping[FLdeep$vmapping != 0]
    cnames <- c(cnames,
                notes = paste0("",functionName,"imp"))
    
    
        
    ret <- sqlStoredProc(connection,
                         functionName,
                         pInputParams = cnames,
                         outputParameter = c(OutTable = 'a')
                         )
    vAnalysisID <- ret[[1]]

    return(new("FLVIF",
               table=data,
               results=list(call=vcallObject,
                            deeptbl = vdeeptbl,
                            vspec = vAnalysisID,
                            stattbl = vstat,
                            method = method,
                            vmapping = vmapping,
                            viter = NULL ))) 
}



#' @export
vif.FLTableMD <- vif.FLTable



#' @export
`$.FLVIF`<-function(object,property){
                                        #parentObject <- deparse(substitute(object))
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

    if(object@results$method != "normal" && is.null(object@results$viter)){
        vstr <- paste0("select max(iteration) as viter from ",object@results$stattbl," WHERE ANalysisID = ",fquote(object@results$vspec))
        if(is.null(object@results[["viter"]])){
            vdf <- sqlQuery(connection,vstr)
            object@results[["viter"]] <- vdf$viter
            # object@results <- c(object@results,list(viter = vdf$viter))
        }
    }

    if(property == "vif")
    {
        if(!is.null(object@results[["vif"]]))
            return(object@results[["vif"]])
        vstr <- paste0("select datasetid as datasetid, VARID as predictors, VIF as vif from ",object@results$stattbl,
                    " WHERE AnalysisID = ",fquote(object@results$vspec),
                    ifelse(!is.null(object@results$viter),paste0(" AND iteration = ",object@results$viter),""),
                    " order by datasetid, varid, VIF")
        vdf <- sqlQuery(connection, vstr)
        vres <- dlply(vdf,"datasetid",
                    function(x){
                        vvif <- x$vif
                        vdatasetid <- as.character(unique(x$datasetid))
                        vmap <- object@results[["vmapping"]][[vdatasetid]]
                        if(!is.null(vmap))
                            vnames <- names(vmap)[match(x$predictors,vmap)]
                        else vnames <- NULL
                        names(vvif) <- vnames
                        return(vvif)
                    })
        if(length(vres)==1)
            vres <- vres[[1]]
        object@results[["vif"]] <- vres
        assign(parentObject,object,envir=parent.frame())
        return(vres)
    }

    if(property == "model.matrix"){
        if(!isDeep(flmod@table)){
            vval <- object$select + 2
            vtbl <- object@table[,vval]
            return(vtbl)
            
        }
        else
            return(NULL)
    }
    
    if(property == "select"){
        if(object@results$method == "normal")
            vthreshold <- paste0(" and vif<5")
        else
            vthreshold <- paste0(" and iteration = ",object@results$viter,"")
        vstr <- paste0("select VARID as predictors from ",object@results$stattbl,
                        " WHERE AnalysisID = ",fquote(object@results$vspec),"",vthreshold,"order by predictors ")
        vdf <- sqlQuery(connection, vstr)       
        return(vdf$predictors)
    }

    }
