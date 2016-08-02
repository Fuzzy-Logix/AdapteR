#' @export
setClass(
    "FLTableMD",
    slots = list(
        select = "FLTableQuery",
        dimnames = "list",
        isDeep = "logical",
        mapSelect = "FLSelectFrom"
    )
)

#' @export
FLTableMD <- function(database, 
                    table,
                    group_id_colname,
                    obs_id_colname,
                    var_id_colnames=character(0), 
                    cell_val_colname=character(0),
                    connection=NULL,
                    group_id=c())
{
    whereconditions <- ""
    vgrp <- group_id
    if(is.null(connection)) connection <- getConnection(NULL)
    ## If alias already exists, change it to flt.
    if(length(names(table))>0)
    oldalias <- names(table)[1]
    else oldalias <- ""
    var_id_colnames <- changeAlias(var_id_colnames,"flt",oldalias)
    obs_id_colname <- changeAlias(obs_id_colname,"flt",oldalias)
    cell_val_colname <- changeAlias(cell_val_colname,"flt",oldalias)
    group_id_colname <- changeAlias(group_id_colname,"flt",oldalias)

    if(length(group_id)>0){
        whereconditions <- paste0(group_id_colname," IN (",
                                paste0(group_id,collapse=","),")")

        whereconditions <- changeAlias(whereconditions,
                                      "flt",
                                      c(paste0(database,".",table),
                                        paste0(table),
                                        paste0(database,".",oldalias),
                                        oldalias))
    }
    names(table) <- "flt"
    if(length(var_id_colnames) && length(cell_val_colname))
    {
        vdims <- sqlQuery(connection,
                         paste0("SELECT ",group_id_colname," AS grpID, \n ",
                                        "MIN(",var_id_colnames,") AS MinID, \n ",
                                        "Max(",var_id_colnames,") AS MaxID \n ",
                                        "Max(",obs_id_colname,") AS MaxObsID \n ",
                                " FROM ",remoteTable(database,table),
                                constructWhere(whereconditions)," \n ",
                                " GROUP BY ",group_id_colname,
                                " ORDER BY ",group_id_colname))
        if(length(unique(vdims[["MinID"]]))!=1 
            ||length(unique(vdims[["MaxID"]]))!=1)
            stop("currently each set should have same columns \n ")
        cols <- vdims[["MinID"]]:vdims[["MaxID"]]

        rows <- vdims[["MaxObsID"]]
        if(length(vgrp)==0)
            vgrp <- vdims[["grpID"]]
        # cols <- gsub("^ +| +$","",cols)
        # rows <- gsub("^ +| +$","",rows)

        ##change factors to strings
        vstringdimnames <- lapply(list(rows,cols),
                                  function(x){
                                      if(is.factor(x))
                                      as.numeric(x)
                                      else x
                                  })
        rows <- vstringdimnames[[1]]
        cols <- vstringdimnames[[2]]

        if(is.character(rows) || is.character(cols))
            stop("obsIDs and varIDs cannot be characters \n ")

        select <- new(
        "FLSelectFrom",
        connection = connection, 
        database = database, 
        table_name = table, 
        variables = list(
                group_id_colname= group_id_colname,
                obs_id_colname = obs_id_colname,
                var_id_colname = var_id_colnames,
                cell_val_colname = cell_val_colname),
        whereconditions=whereconditions,
        order = "")

        new("FLTableMD",
            select = select,
            dimnames = list(rows,cols,vgrp),
            isDeep = TRUE)
    }
    else
    {
        browser()
        R <- sqlQuery(connection,
                      limitRowsSQL(paste0("select * from ",remoteTable(database,table)),1))
        cols <- names(R)
        vdims <- sqlQuery(connection,
                         paste0("SELECT ",group_id_colname," AS grpID, \n ",
                                        "Max(",obs_id_colname,") AS MaxObsID \n ",
                                " FROM ",remoteTable(database,table),
                                constructWhere(whereconditions)," \n ",
                                " GROUP BY ",group_id_colname,
                                " ORDER BY ",group_id_colname))
        rows <- vdims[["MaxObsID"]]
        ##change factors to strings
        vstringdimnames <- lapply(list(rows,cols),
                                  function(x){
                                      if(is.factor(x))
                                      as.numeric(x)
                                      else x
                                  })
        rows <- vstringdimnames[[1]]
        cols <- vstringdimnames[[2]]
        if(length(vgrp)==0)
            vgrp <- vdims[["grpID"]]
        if(is.character(rows))
            stop("obsIDs cannot be characters \n ")

        cols <- setdiff(cols,obs_id_colname)

        select <- new(
        "FLSelectFrom",
        connection = connection, 
        database = database, 
        table_name = table, 
        variables = list(
                group_id_colname=group_id_colname,
                obs_id_colname = obs_id_colname,
                #var_id_colname = var_id_colnames,
                cell_val_colname = cell_val_colname),
        whereconditions=whereconditions,
        order = "")

        T <- new("FLTable", 
                 select = select,
                 dimnames = list(rows,cols,vgrp),
                 isDeep = FALSE)
    }
}

setMethod("FLRegrDataPrep",
          signature(object = "FLTableMD",
                    depCol="character"
                    ),
          function(object,
                  depCol,
                  outDeepTableName=gen_deep_table_name(object@select@table_name),
                  outDeepTableDatabase=getOption("ResultDatabaseFL"),
                  outObsIDCol="obs_id_colname",
                  outVarIDCol="var_id_colname",
                  outValueCol="var_id_colname",
                  catToDummy=0,
                  performNorm=0,
                  performVarReduc=0,
                  makeDataSparse=0,
                  minStdDev=0,
                  maxCorrel=0,
                  trainOrTest=0,
                  excludeCols="",
                  classSpec=list(),
                  whereconditions="",
                  inAnalysisID="")
          {
            if(object@isDeep) return(list(table=object))
            connection <- getConnection(object)
            object <- setAlias(object,"")
            if(outDeepTableName == "")
            deeptablename <- gen_deep_table_name(object@select@table_name)
            #deeptablename <- genRandVarName()
            else deeptablename <- outDeepTableName
            if(outDeepTableDatabase == "")
            outDeepTableDatabase <- getOption("ResultDatabaseFL")
            if(class(object@select)=="FLTableFunctionQuery")
            {
              ## Views are not working  in FLDeepToWide and FLWideToDeep
              widetable <- gen_view_name(object@select@table_name)
              # sqlstr <- paste0("CREATE VIEW ",outDeepTableDatabase,".",widetable," AS ",constructSelect(object))
              # sqlSendUpdate(connection,sqlstr)
              createView(pViewName=getRemoteTableName(outDeepTableDatabase,
                                                      widetable),
                        pSelect=constructSelect(object))
              select <- new("FLSelectFrom",
                        connection = connection, 
                        database = outDeepTableDatabase, 
                        table_name = widetable, 
                        variables = list(
                                obs_id_colname = obs_id_colname),
                        whereconditions="",
                        order = "")

              object <- new("FLTable",
                            select = select,
                            dimnames = object@dimnames,
                            isDeep = FALSE)
              #object <- store(object)
            }

            checkParamsLM <- function(pObject,pExpected)
            {
              vresult <- sapply(1:length(pObject),function(x){
                vIn <- pObject[x]
                if(is.numeric(pExpected[[x]]))
                {
                  
                  if(any(is.na(as.numeric(vIn))))
                  stop("argument mis-match.Only numeric allowed")
                  vIn <- as.numeric(vIn)
                }
                if(vIn %in% pExpected[[x]])
                return(vIn)
                else
                return(pExpected[[x]][1])
                })
            }

            vresult <-
            checkParamsLM(c(catToDummy,performNorm,performVarReduc,
              makeDataSparse,trainOrTest),
                            list(c(0,1),c(0,1),c(0,1),c(0,1,2),c(0,1)))

            vIn <- c("catToDummy","performNorm","performVarReduc",
                      "makeDataSparse","trainOrTest")

            vtemp <- sapply(1:5,function(x){
              assign(vIn[x],vresult[x],envir=parent.env(environment()))
              })

            if(!is.numeric(minStdDev) || !minStdDev>=0)
            minStdDev <- 0
            if(!is.numeric(maxCorrel) || maxCorrel<=0 || maxCorrel>1)
            maxCorrel <- 0

            if(trainOrTest==1) depCol <- "NULL"
            else if(!(depCol %in% colnames(object)))
            stop(depCol," not in colnames of input table for FLRegrDataPrep")

            if(trainOrTest==1 && inAnalysisID %in% c("NULL",""))
            stop("inAnalysisID should be valid when trainOrTest=1")
            else if(inAnalysisID=="" || is.null(inAnalysisID)) inAnalysisID <- "NULL"
            else inAnalysisID <- inAnalysisID

            if(length(classSpec)==0 || classSpec=="") classSpec <- "NULL"
            else{
              classSpec <- paste0(list_to_class_spec(classSpec))
              catToDummy <- 1
            }
            whereconditions <- c(whereconditions,object@select@whereconditions)
            whereClause <- constructWhere(whereconditions)
            if(whereClause=="") whereClause <- "NULL"
            else
            whereClause <- paste0(whereClause)
            if(excludeCols=="" || length(excludeCols)==0) excludeCols <- "NULL"
            else
            excludeCols <- paste0(excludeCols)

            if(outObsIDCol=="") outObsIDCol <- "obs_id_colname"
            if(outVarIDCol=="") outVarIDCol <- "var_id_colname"
            if(outValueCol=="") outValueCol <- "cell_val_colname"

            retobj <- sqlStoredProc(connection,
                                    "FLRegrDataPrepMD",
                                    outputParameter=c(AnalysisID="a"),
                                    InWideTable=paste0(object@select@database,
                                                      ".",object@select@table_name),
                                    GroupID=getVariables(object)[["group_id_colname"]],
                                    ObsIDCol=getVariables(object)[["obs_id_colname"]],
                                    DepCol=depCol,
                                    OutDeepTable=paste0(outDeepTableDatabase,
                                                        ".",deeptablename),
                                    OutObsIDCol=outObsIDCol,
                                    OutVarIDCol=outVarIDCol,
                                    OutValueCol=outValueCol,
                                    CatToDummy=catToDummy,
                                    PerformNorm=performNorm,
                                    PerformVarReduc=performVarReduc,
                                    MakeDataSparse=makeDataSparse,
                                    MinStdDev=minStdDev,
                                    MaxCorrel=maxCorrel,
                                    TrainOrtest=trainOrTest,
                                    ExcludeCols=excludeCols,
                                    ClassSpec=classSpec,
                                    WhereClause=whereClause,
                                    InAnalysisID=inAnalysisID
                                    )
            # sqlstr<-paste0("CALL FLRegrDataPrep('",
            #                                   object@select@database,".",object@select@table_name,"',\n      ",
            #                                   fquote(getVariables(object)[["obs_id_colname"]]),",\n      ",
            #                                   fquote(depCol),",\n      ",
            #                                   fquote(paste0(outDeepTableDatabase,".",deeptablename)),",\n      ",
            #                                   fquote(outObsIDCol),",\n      ",
            #                                   fquote(outVarIDCol),",\n      ",
            #                                   fquote(outValueCol),",\n      ",
            #                                   catToDummy,",\n      ",
            #                                   performNorm,",\n      ",
            #                                   performVarReduc,",\n      ",
            #                                   makeDataSparse,",\n      ",
            #                                   minStdDev,",\n      ",
            #                                   maxCorrel,",\n      ",
            #                                   trainOrTest,",\n      ",
            #                                   excludeCols,",\n      ",
            #                                   classSpec,",\n      ",
            #                                   whereClause,",\n      ",
            #                                   inAnalysisID,",\n      ",
            #                                   "AnalysisID);")
            # t <- sqlQuery(connection,sqlstr,
            #               AnalysisIDQuery="SELECT top 1 ANALYSISID from fzzlRegrDataPrepInfo order by RUNENDTIME DESC")
                
            dataprepID <- as.vector(retobj[1,1])

            updateMetaTable(pTableName=paste0(outDeepTableDatabase,
                                              ".",deeptablename),
                            pType="deepTable")

            table <- FLTableMD(outDeepTableDatabase,
                               deeptablename,
                               outObsIDCol,
                               outVarIDCol,
                               outValueCol
                              )
            return(list(table=table,
                        AnalysisID=dataprepID))

          }
        )