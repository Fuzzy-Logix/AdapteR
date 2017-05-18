#' @export
setClass(
    "FLNnet",
    slots = list(formula = "formula",
                 scoreTable = "character",
                 results = "list",
                 table = "FLTable"
                 ) )

#' tbl <- FLTable("tblwinetrain", obs_id_colname = "OBSID")
#' @export
Nnet <- function(formula, data, fetchID = TRUE,maxiter = 10,...)
{
    browser()
    vcallObject <- match.call()
    deeptblname <- gen_unique_table_name("nnetdeep")
    if(!isDeep(data))
        {
    FLdeep <- prepareData(formula         = formula ,
                          data            = data,
                          outDeepTable    = deeptblname,
                          makeDataSparse  = 1,
                          performVarReduc = 0,
                          minStdDev       = .01,
                          maxCorrel       = .8,
                          fetchIDs        = FALSE)}
    vmap <- FLdeep$vmapping
    outtblname <- gen_unique_table_name("nnetout")
    
    data <- setAlias(data,"")
    functionName <- "FLNNetUdt"
    
    cnames <- c(GroupID = 1,
                ObsID = FLdeep$deepx@select@variables$obs_id_colname,
                VarID = FLdeep$deepx@select@variables$var_id_colname,
                Num_Val= FLdeep$deepx@select@variables$cell_val_colname )

    varg <- c(NeuronCountOne = 10,
              NeuronCountTwo = 5,
              LearningRate= .2,
              MaxEpochs = 500,
              IsSigmoid =1 )


    t <- createTable(outtblname, 
                    pSelect =  constructUDTSQL(pViewColnames = cnames,
                                             pFuncName = functionName,
                                             pOutColnames = c("a.*"),
                                             pSelect = FLdeep$deepx@select@table_name,
                                             pLocalOrderBy=c("GroupID", "ObsID", "VarID"), 
                                             pNest = TRUE, 
                                             pFromTableFlag = TRUE,
                                             pArg = varg))
    vstr <- paste0("SELECT TOP 5 * FROM ",outtblname,"")
    sqlQuery(connection, vstr)
                   ## pPrimaryKey=vprimaryKey)
    return(new("FLNnet",
               formula=formula,
               scoreTable="",
               table=data,
               results=list(call=vcallObject,
                            deeptbl = FLdeep$deepx,
                            vspec = outtblname)))   
}

