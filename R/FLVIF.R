#' @export
setClass(
    "FLVIF",
    slots = list(results = "list",
                 table = "FLTable"
                 ) )

#' @export
#' fltbl <- FLTable(table = "tbllinregr", obs_id_colname="OBSID", var_id_colnames="VARID", "NUM_VAL")
#' flmod <- vif.FLTable(data = fltbl)
#' flmod <- vif.FLTable(data = fltbl, method = "fb")
#' flmod <- vif.FLTable(data = fltbl, method = "bw")
#' vtblname <- gen_wide_table_name("vif")
#' vtbl <- createView(vtblname, pSelect="select * from tbllinregr where obsid<200")
#' vstr <- paste0("create view ",vtblname," as select * from tbllinregr where obsid<201")
#' sqlSendUpdate(connection, vstr)
#' fltbl <- FLTable(table = vtblname, obs_id_colname="OBSID", var_id_colnames="VARID", "NUM_VAL")
#' flmod <- vif.FLTable(data = fltbl)
#' flmod <- vif.FLTable(data = fltbl, method = "bw")
#' flmod <- vif.FLTable(data = fltbl,method = "fb")
#' methods = vif:- "normal",FLVIFMultiDataSet: groupid
#' FLVIFBW:vifthreshold, FLVIFBWMultiDataSet: GroupIDCol, vifthreshold
#' FLVIFFB: thresh1, thresh2, FLVIFFBMultiDataSet:groupidcol, thresh1,thresh2.
#' method = "bw", "fb"
#'dropTable("tbllinvif")
#' createTable(pTableName = "tbllinvif", pSelect = "select * from tbllinregr where obsid <400")
#' fltbl <- FLTable(table = "tbllinvif",obs_id_colname = "OBSID",var_id_colname = "VARID", cell_val_colname = "NUM_VAL" )
#' flmod <- vif.FLTable(data = fltbl)


vif.FLTable <- function(formula, data, fetchID = TRUE,method = "normal",...)
{
    browser()
    vcallObject <- match.call()
    deeptblname <- gen_unique_table_name("vif")
    vdeeptbl <- data
    
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
        cnames <- c(TableName =  data@select@table_name[[1]],
                    ObsIDCol = FLdeep$deepx@select@variables$obs_id_colname,
                    VarIDCol = FLdeep$deepx@select@variables$var_id_colname,
                    ValueCol= FLdeep$deepx@select@variables$cell_val_colname) }
    else
    { cnames <- c(TableName =  data@select@table_name[[1]],
                  ObsIDCol = gsub("flt.", "",data@select@variables$obs_id_colname),
                  VarIDCol = gsub("flt.", "",data@select@variables$var_id_colname),
                  ValueCol= gsub("flt.", "",data@select@variables$cell_val_colname)) }

    functionName <- "FLVIF"
    vstat <- "fzzlvifstats"
    data <- setAlias(data,"")
    
    if(method == "bw"){
        functionName = "FLVIFBW"
        cnames <- c(cnames,
                    VIFThreshold = 5
                    )
    vstat <- "fzzlVIFBWStats"}

    if(method == "fb"){
        functionName = "FLVIFFB"
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
                            stattbl = vstat)))   
}


#' @export
`$.FLVIF`<-function(object,property){
                                        #parentObject <- deparse(substitute(object))
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

    if(property == "vif")
    {
        ##browser()
        vstr <- paste0("select VARID as predictors, VIF as vif from ",object@results$stattbl," WHERE AnalysisID = ",fquote(object@results$vspec)," order by Varid ")
        sqlQuery(connection, vstr)
        
    }

    }
