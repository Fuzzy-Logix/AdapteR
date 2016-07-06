#' @include FLMatrix.R
NULL



#' Constructor function for FLTable.
#'
#' \code{FLTable} constructs an object of class \code{FLTable}.
#'
#' \code{FLTable} refers to an in-database table. This is equivalent to data.frame object. 
#' This object is commonly used as input for data mining functions.
#' @param connection ODBC/JDBC connection object
#' @param database name of the database
#' @param table name of the table
#' @param obs_id_colname column name set as primary key
#' @param var_id_colname column name where variable id's are stored if \code{FLTable} is deep
#' @param cell_val_colname column name where cell values are stored if \code{FLTable} is deep
#' @param whereconditions whereconditions if any to reference the table
#' @return \code{FLTable} returns an object of class FLTable mapped to a table
#' in Teradata.
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' widetable  <- FLTable( "FL_Deep", "tblAbaloneWide", "ObsID")
#' deeptable <- FLTable("FL_DEMO","tblUSArrests","ObsID","VarID","Num_Val")
#' names(widetable)
#' @export
FLTable <- function(database, 
                    table,
                    obs_id_colname,
                    var_id_colnames=character(0), 
                    cell_val_colname=character(0),
                    whereconditions=character(0),
                    connection=NULL)
{
    if(is.null(connection)) connection <- getConnection(NULL)
    ## If alias already exists, change it to flt.
    if(length(names(table))>0)
    oldalias <- names(table)[1]
    else oldalias <- ""
    var_id_colnames <- changeAlias(var_id_colnames,"flt",oldalias)
    obs_id_colname <- changeAlias(obs_id_colname,"flt",oldalias)
    cell_val_colname <- changeAlias(cell_val_colname,"flt",oldalias)
    whereconditions <- changeAlias(whereconditions,
                                  "flt",
                                  c(paste0(database,".",table),
                                    paste0(table),
                                    paste0(database,".",oldalias),
                                    oldalias))
    names(table) <- "flt"
    if(length(var_id_colnames) && length(cell_val_colname))
	{
        cols <- sort(sqlQuery(connection,
                         paste0("SELECT DISTINCT(",
                                var_id_colnames,") as VarID FROM ",remoteTable(database,table),
                          " ",constructWhere(whereconditions)))$VarID)
        rows <- sort(sqlQuery(connection,
                         paste0("SELECT DISTINCT(",
                                obs_id_colname,") as VarID FROM ",remoteTable(database,table),
                          " ",constructWhere(whereconditions)))$VarID)
        cols <- gsub("^ +| +$","",cols)
        rows <- gsub("^ +| +$","",rows)

        ##change factors to strings
        vstringdimnames <- lapply(list(rows,cols),
                                  function(x){
                                      if(is.factor(x))
                                      as.character(x)
                                      else x
                                  })
        rows <- vstringdimnames[[1]]
        cols <- vstringdimnames[[2]]

        select <- new(
        "FLSelectFrom",
        connection = connection, 
        database = database, 
        table_name = table, 
        variables = list(
                obs_id_colname = obs_id_colname,
                var_id_colname = var_id_colnames,
                cell_val_colname = cell_val_colname),
        whereconditions=whereconditions,
        order = "")

		new("FLTable",
            select = select,
            dimnames = list(rows,cols),
            isDeep = TRUE)
	}
	else
	{
        
        R <- sqlQuery(connection,paste0("select top 1 * from ",remoteTable(database,table)))
        cols <- names(R)
        rows <- sort(sqlQuery(connection,
                            paste0("SELECT DISTINCT(",
                                obs_id_colname,") as VarID
						  FROM ",remoteTable(database,table),
                          " ",constructWhere(whereconditions)))$VarID)
        cols <- gsub("^ +| +$","",cols)
        rows <- gsub("^ +| +$","",rows)

        ##change factors to strings
        vstringdimnames <- lapply(list(rows,cols),
                                  function(x){
                                      if(is.factor(x))
                                      as.character(x)
                                      else x
                                  })
        rows <- vstringdimnames[[1]]
        cols <- vstringdimnames[[2]]
        
        if(length(var_id_colnames)==0)
            var_id_colnames <- cols
        if(length(setdiff(var_id_colnames,cols)))
            stop(paste0("columns do not exist: "))

        select <- new(
        "FLSelectFrom",
        connection = connection, 
        database = database, 
        table_name = table, 
        variables = list(
                obs_id_colname = obs_id_colname,
                #var_id_colname = var_id_colnames,
                cell_val_colname = cell_val_colname),
        whereconditions=whereconditions,
        order = "")

		T <- new("FLTable", 
                 select = select,
                 dimnames = list(rows,var_id_colnames),
                 isDeep = FALSE)
	}
}

##' Gives the column names of FLTable object
##'
##' @param object 
#' @export
names.FLTable <- function(object) object@dimnames[[2]]
#' @export
colnames.FLTable <- function(object) object@dimnames[[2]]
#' @export
rownames.FLTable <- function(object) object@dimnames[[1]]

#' @export
setMethod("show","FLTable",function(object) print(as.data.frame(object)))

#' @export
`$.FLTable` <- function(object,property){
  #browser()
  vcolnames <- colnames(object)
  property <- property[1]
  if(!is.character(property))
  return(NULL)
  if(property %in% colnames(object))
  return(object[,as.character(property)])
  else return(NULL)
}

#' @export
`[[.FLTable` <- function(object,property,...){
  #browser()
  if(is.character(property))
  return(do.call("$",list(object,property)))
  else if(is.numeric(property) || as.integer(property))
  {
    vcolnames <- colnames(object)
    property <- as.integer(property)
    if(length(property)==1){
      vtemp <- as.character(vcolnames[property])
      return(do.call("$",list(object,vtemp)))
    }
    else{
      vtemp <- object[[property[1]]]
      property <- property[-1]
      for(i in 1:length(property)){
        tryCatch(vtemp <- vtemp[property[i]],
                 error=function(e)
                   stop("error in recursive subsetting at level",i+1))
      }
      return(vtemp)
    }
  }
  else return(NULL)
}

# data(iris)
# irisFL <- as.FLTable(iris)
# irisFL$SepalArea <- irisFL$SepalLength * irisFL$SepalWidth
# head(irisFL)
#' @export
`$<-.FLTable` <- function(x,name,value){
  #browser()
  vcolnames <- x@dimnames[[2]]
  vtablename <- getRemoteTableName(databaseName=x@select@database,
                tableName=x@select@table_name)
  name <- gsub("\\.","",name,fixed=TRUE)
  xcopy <- x
  x <- setAlias(x,"")
  
  addColumnFLQuery <- function(pTable,pName,pValue){
    ##Get data type of pValue
    vColumnType <- getFLColumnType(x=pValue)
    sqlstr <- paste0("ALTER TABLE ",pTable," \n ",
                    " ADD ",pName," ",vColumnType,";")
    return(sqlSendUpdate(getOption("connectionFL"),sqlstr))
  }
  if(!x@isDeep){
    #browser()
    if(!tolower(name) %in% tolower(vcolnames)){
      vtemp <- addColumnFLQuery(pTable=vtablename,
                              pName=name,
                              pValue=value)
      vcolnames <- c(vcolnames,name)
    }
    else{
      vtableColType <- getFLColumnType(x=as.vector(x[1,vcolnames[tolower(name)==tolower(vcolnames)][1]]))
      vnewColType <- getFLColumnType(x=value)
      if(!vtableColType %in% vnewColType){
        vtemp <- sqlSendUpdate(getOption("connectionFL"),
                                    paste0(" ALTER TABLE ",vtablename," DROP COLUMN ",name))
        vtemp <- addColumnFLQuery(pTable=vtablename,
                              pName=name,
                              pValue=value)
      }
    }
    if(!is.FLVector(value))
    value <- as.FLVector(value)
    sqlstr <- paste0("UPDATE ",vtablename," \n ",
                    " FROM(",constructSelect(value),") a \n ",
                    " SET ",name," = a.vectorValueColumn \n ",
                    " WHERE a.vectorIndexColumn = ",getVariables(x)[["obs_id_colname"]],";")
  }
  else{
    if(tolower(name)%in%tolower(vcolnames))
    sqlstr <- paste0("UPDATE ",vtablename," \n ",
                    " FROM(",constructSelect(value),") a \n ",
                    " SET ",getVariables(x)[["cell_val_colname"]]," = a.vectorValueColumn \n ",
                    " WHERE a.vectorIndexColumn = ",getVariables(x)[["obs_id_colname"]],
                            " AND ",getVariables(x)[["var_id_colname"]]," = ",name,";")
    else{
      if(is.na(as.numeric(name)))
      stop("name should be numeric in deep table \n ")
      sqlstr <- paste0(" INSERT INTO ",vtablename," \n ",
                    " SELECT a.vectorIndexColumn, \n ",
                            name,
                            ", \n a.vectorValueColumn \n ",
                    " FROM(",constructSelect(value),") a;")
      vcolnames <- c(vcolnames,name)
    }
  }
  sqlSendUpdate(getOption("connectionFL"),sqlstr)
  xcopy@dimnames[[2]] <- vcolnames
  return(xcopy)
}

#' Convert Wide Table to Deep Table in database.
#'
#' @param object FLTable object
#' @param excludeCols character vector specifying columns to be excluded from conversion
#' @param classSpec list representing Class specification which identifies then value of the categorical variable to be used a reference
#' @param whereconditions character vector giving where conditions if any to reference the wide table
#' @param outDeepTableName name to be given to the output deep table
#' @param outDeepTableDatabase name of database to store the output deep table
#' @param outObsIDCol name to give to the primary key column name of the output deep table
#' @param outVarIDCol name to give to the varibales name column of the output deep table
#' @param outValueCol name to give to the value column of the output deep table
#' @return \code{wideToDeep} returns a list containing components 
#' \code{table} which is the FLTable referencing the deep table and \code{AnalysisID} giving the AnalysisID of conversion
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' widetable  <- FLTable( "FL_DEMO", "tblAbaloneWide", "ObsID")
#' resultList <- wideToDeep(widetable)
#' deeptable <- resultList$table
#' analysisID <- resultList$AnalysisID
#' @export

setGeneric("wideToDeep", function(object,excludeCols,classSpec,whereconditions,
                                  outDeepTableName,outDeepTableDatabase,
                                  outObsIDCol,outVarIDCol,outValueCol) {
    standardGeneric("wideToDeep")
})
setMethod("wideToDeep",
          signature(object = "FLTable",
                    excludeCols="character",
                    classSpec="list",
                    whereconditions="character",
                    outDeepTableName="character",
                    outDeepTableDatabase="character",
                    outObsIDCol="character",
                    outVarIDCol="character",
                    outValueCol="character"
                    ),
          function(object,
                  excludeCols,
                  classSpec,
                  whereconditions,
                  outDeepTableName,
                  outDeepTableDatabase,
                  outObsIDCol,
                  outVarIDCol,
                  outValueCol)
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
              sqlstr <- paste0("CREATE VIEW ",outDeepTableDatabase,".",widetable," AS ",constructSelect(object))
              sqlSendUpdate(connection,sqlstr)
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

            if(length(classSpec)==0) 
            classSpec <- "NULL"
            else
            classSpec <- list_to_class_spec(classSpec)
            whereconditions <- c(whereconditions,object@select@whereconditions)
            whereClause <- constructWhere(whereconditions)
            if(whereClause=="") whereClause <- "NULL"
            if(excludeCols=="" || length(excludeCols)==0) 
            excludeCols <- "NULL"
            if(outObsIDCol=="") outObsIDCol <- "obs_id_colname"
            if(outVarIDCol=="") outVarIDCol <- "var_id_colname"
            if(outValueCol=="") outValueCol <- "cell_val_colname"

            retobj <- sqlStoredProc(
                              connection,
                              "FLWideToDeep",
                              outputParameter=c(AnalysisID="a"),
                              InWideTable=paste0(object@select@database,
                                          ".",object@select@table_name),
                              ObsIDCol=getVariables(object)[["obs_id_colname"]],
                              OutDeepTable=paste0(outDeepTableDatabase,".",deeptablename),
                              OutObsIDCol=outObsIDCol,
                              outVarIDCol=outVarIDCol,
                              outValueCol=outValueCol,
                              excludeCols=excludeCols,
                              classSpec=classSpec,
                              whereClause=whereClause)
            # sqlstr<-paste0("CALL FLWideToDeep('",object@select@database,".",object@select@table_name,"','",
            #                                   getVariables(object)[["obs_id_colname"]],"','",
            #                                   outDeepTableDatabase,".",deeptablename,
            #                                   "','",outObsIDCol,"',
            #                                   '",outVarIDCol,"',
            #                                   '",outValueCol,"',",
            #                                   excludeCols,",",
            #                                   classSpec,",",
            #                                   whereClause,
            #                                   ",AnalysisID);")
            # t <- sqlQuery(connection,sqlstr,
            #     AnalysisIDQuery="SELECT top 1 ANALYSISID from fzzlRegrDataPrepInfo order by RUNENDTIME DESC")
                
            dataprepID <- as.vector(retobj[1,1])

            table <- FLTable(outDeepTableDatabase,
                           deeptablename,
                           outObsIDCol,
                           outVarIDCol,
                           outValueCol
                          )
            return(list(table=table,
                        AnalysisID=dataprepID))

          }
        )
setMethod("wideToDeep",
          signature(object = "FLTable",
                    excludeCols="missing",
                    classSpec="missing",
                    whereconditions="missing",
                    outDeepTableName="missing",
                    outDeepTableDatabase="missing",
                    outObsIDCol="missing",
                    outVarIDCol="missing",
                    outValueCol="missing"),
          function(object)
          wideToDeep(object,
                    excludeCols="",
                    classSpec=list(),
                    whereconditions="",
                    outDeepTableName="",
                    outDeepTableDatabase="",
                    outObsIDCol="",
                    outVarIDCol="",
                    outValueCol=""))

setMethod("wideToDeep",
          signature(object = "FLTable",
                    excludeCols="character",
                    classSpec="list",
                    whereconditions="character",
                    outDeepTableName="missing",
                    outDeepTableDatabase="missing",
                    outObsIDCol="missing",
                    outVarIDCol="missing",
                    outValueCol="missing"),
          function(object,excludeCols,classSpec,whereconditions)
          wideToDeep(object,
                    excludeCols=excludeCols,
                    classSpec=classSpec,
                    whereconditions=whereconditions,
                    outDeepTableName="",
                    outDeepTableDatabase="",
                    outObsIDCol="",
                    outVarIDCol="",
                    outValueCol=""))


#' Convert Deep Table to Wide Table
#' @param object FLTable object to convert to wide table
#' @param whereconditions character vector specifying whereconditions if any to reference the input deep table
#' @param mapTable name of the in-database table containing mapping information to be used in conversion if any
#' @param mapName unique identifier for the mapping information in mapping table if any
#' @param outWideTableDatabase name of database where output wide table is to be stored
#' @param outWideTableName name to give to the output wide table
#' @return \code{deepToWide} returns a list containing components 
#' \code{table} which is the FLTable referencing the wide table and \code{AnalysisID} giving the AnalysisID of conversion
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' deeptable  <- FLTable( "FL_DEMO", "tblUSArrests", "ObsID","VarID","Num_Val")
#' resultList <- deepToWide(deeptable)
#' widetable <- resultList$table
#' analysisID <- resultList$AnalysisID
#' @export
setGeneric("deepToWide", function(object,
                                  whereconditions,
                                  mapTable,
                                  mapName,
                                  outWideTableDatabase,
                                  outWideTableName,
                                  Analysisid
                                  ) {
    standardGeneric("deepToWide")
})
setMethod("deepToWide",
          signature(object = "FLTable",
                    whereconditions="character",
                    mapTable="character",
                    mapName="character",
                    outWideTableDatabase="character",
                    outWideTableName="character",
                    Analysisid = "character"),
          function(object,
                  whereconditions,
                  mapTable,
                  mapName,
                  outWideTableDatabase,
                  outWideTableName,
                  Analysisid
                  )
          {
            #browser()
            if(!object@isDeep) return(list(table=object))
            connection <- getConnection(object)
            object <- setAlias(object,"")
            if(outWideTableDatabase=="")
            outWideTableDatabase <- getOption("ResultDatabaseFL")
            usedwidetablename <- paste0(getOption("ResultDatabaseFL"),".",
                                      gen_wide_table_name("MAP"))
            if(mapTable=="" || mapTable=="NULL"){
              if(Analysisid!="")
              {
                sqlstr1<-paste0("DELETE FROM ",usedwidetablename,"; \n ",
                                " INSERT INTO ",usedwidetablename," \n ", 
                                " SELECT a.Final_VarID, \n  
                                        a.COLUMN_NAME, \n 
                                        a.FROM_TABLE
                                 FROM fzzlRegrDataPrepMap a 
                                 WHERE a.AnalysisID = '",Analysisid,"';")
                sqlSendUpdate(connection,sqlstr1)
                mapTable<-usedwidetablename
                mapname<- genRandVarName()
              }
              else{
              mapTable <- "NULL"
              mapName <- "NULL"}
            }
            else if(mapName == "") mapName <- "NULL"

            whereconditions <- c(whereconditions,object@select@whereconditions)
            #whereconditions <- whereconditions[whereconditions!="" && whereconditions!="NULL"]
              object@select@whereconditions <- whereconditions
            
              deeptable <- gen_view_name(object@select@table_name)
              #deeptable <- paste0(sample(letters[1:26],1),round(as.numeric(Sys.time())))
              sqlstr <- paste0("CREATE VIEW ",outWideTableDatabase,".",deeptable," AS ",constructSelect(object))
              sqlSendUpdate(connection,sqlstr)
              select <- new("FLSelectFrom",
                        connection = connection, 
                        database = outWideTableDatabase, 
                        table_name = deeptable, 
                        variables = list(
                                obs_id_colname = "obs_id_colname",
                                var_id_colname = "var_id_colname",
                                cell_val_colname = "cell_val_colname"),
                        whereconditions="",
                        order = "")

              object <- new("FLTable",
                            select = select,
                            dimnames = object@dimnames,
                            isDeep = TRUE)
            # if(class(object@select)=="FLTableFunctionQuery" || length(whereconditions)>0)
            # object <- store(object)

            if(outWideTableName=="")
            outWideTableName <- gen_wide_table_name(object@select@table_name)
            #outWideTableName <- paste0(sample(letters[1:26],1),round(as.numeric(Sys.time())))

            message <- sqlStoredProc(
                              connection,
                              "FLDeepToWide",
                              outputParameter=c(MESSAGE="a"),
                              InDeepTable=paste0(object@select@database,
                                          ".",object@select@table_name),
                              ObsIDCol="obs_id_colname",
                              VarIDCol="var_id_colname",
                              ValueCol="cell_val_colname",
                              MapTable=mapTable,
                              MapName=mapName,
                              WideTable=paste0(outWideTableDatabase,
                                      ".",outWideTableName))

            # sqlstr<-paste0("CALL FLDeepToWide('",object@select@database,".",object@select@table_name,"',
            #                                   'obs_id_colname',
            #                                   'var_id_colname',
            #                                   'cell_val_colname',",
            #                                   ifelse(mapTable=="NULL",mapTable,paste0("'",mapTable,"'")),",",
            #                                   ifelse(mapName=="NULL",mapName,paste0("'",mapName,"'")),",'",
            #                                   paste0(outWideTableDatabase,".",outWideTableName),
            #                                   "',MESSAGE);")
            # message <- sqlQuery(connection,sqlstr)
            message <- checkSqlQueryOutput(message)
            table <- FLTable(
                           outWideTableDatabase,
                           outWideTableName,
                           "obs_id_colname"
                          )
            return(list(table=table,
                        message = as.character(message)))
          }
        )
setMethod("deepToWide",
          signature(object = "FLTable",
                    whereconditions="missing",
                    mapTable="missing",
                    mapName="missing",
                    outWideTableDatabase="missing",
                    outWideTableName="missing",
                    Analysisid = "missing"
                   ),
          function(object)
          deepToWide(object,
                    whereconditions="",
                    mapTable="",
                    mapName="",
                    outWideTableDatabase="",
                    outWideTableName="",
                    Analysisid = ""
                    ))

#' Convert Wide Table to Deep Table in database.
#'
#' @param object FLTable object
#' @param excludeCols character vector specifying columns to be excluded from conversion
#' @param classSpec list representing Class specification which identifies then value of the categorical variable to be used a reference
#' @param whereconditions character vector giving where conditions if any to reference the wide table
#' @param outDeepTableName name to be given to the output deep table
#' @param outDeepTableDatabase name of database to store the output deep table
#' @param outObsIDCol name to give to the primary key column name of the output deep table
#' @param outVarIDCol name to give to the varibales name column of the output deep table
#' @param outValueCol name to give to the value column of the output deep table
#' @return \code{wideToDeep} returns a list containing components 
#' \code{table} which is the FLTable referencing the deep table and \code{AnalysisID} giving the AnalysisID of conversion
#' @examples
#' connection <- flConnect(odbcSource="Gandalf")
#' widetable  <- FLTable( "FL_DEMO", "tblAbaloneWide", "ObsID")
#' resultList <- wideToDeep(widetable)
#' deeptable <- resultList$table
#' analysisID <- resultList$AnalysisID
#' @export

setGeneric("FLRegrDataPrep", function(object,depCol,
                                  outDeepTableName,outDeepTableDatabase,
                                  outObsIDCol,outVarIDCol,outValueCol,
                                  catToDummy,performNorm,performVarReduc,
                                  makeDataSparse,minStdDev,maxCorrel,
                                  trainOrTest,excludeCols,classSpec,
                                  whereconditions,inAnalysisID) {
    standardGeneric("FLRegrDataPrep")
})
setMethod("FLRegrDataPrep",
          signature(object = "FLTable",
                    depCol="character",
                    outDeepTableName="character",
                    outDeepTableDatabase="character",
                    outObsIDCol="character",
                    outVarIDCol="character",
                    outValueCol="character",
                    catToDummy="numeric",
                    performNorm="numeric",
                    performVarReduc="numeric",
                    makeDataSparse="numeric",
                    minStdDev="numeric",
                    maxCorrel="numeric",
                    trainOrTest="numeric",
                    excludeCols="character",
                    classSpec="list",
                    whereconditions="character",
                    inAnalysisID="character"
                    ),
          function(object,depCol,
                  outDeepTableName,outDeepTableDatabase,
                  outObsIDCol,outVarIDCol,outValueCol,
                  catToDummy,performNorm,performVarReduc,
                  makeDataSparse,minStdDev,maxCorrel,
                  trainOrTest,excludeCols,classSpec,
                  whereconditions,inAnalysisID)
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
              sqlstr <- paste0("CREATE VIEW ",outDeepTableDatabase,".",widetable," AS ",constructSelect(object))
              sqlSendUpdate(connection,sqlstr)
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
            else inAnalysisID <- fquote(inAnalysisID)

            if(length(classSpec)==0) classSpec <- "NULL"
            else
            classSpec <- paste0("'",list_to_class_spec(classSpec),"'")
            whereconditions <- c(whereconditions,object@select@whereconditions)
            whereClause <- constructWhere(whereconditions)
            if(whereClause=="") whereClause <- "NULL"
            else
            whereClause <- paste0("'",whereClause,"'")
            if(excludeCols=="" || length(excludeCols)==0) excludeCols <- "NULL"
            else
            excludeCols <- paste0("'",excludeCols,"'")

            if(outObsIDCol=="") outObsIDCol <- "obs_id_colname"
            if(outVarIDCol=="") outVarIDCol <- "var_id_colname"
            if(outValueCol=="") outValueCol <- "cell_val_colname"

            sqlstr<-paste0("CALL FLRegrDataPrep('",
                                              object@select@database,".",object@select@table_name,"',\n      ",
                                              fquote(getVariables(object)[["obs_id_colname"]]),",\n      ",
                                              fquote(depCol),",\n      ",
                                              fquote(paste0(outDeepTableDatabase,".",deeptablename)),",\n      ",
                                              fquote(outObsIDCol),",\n      ",
                                              fquote(outVarIDCol),",\n      ",
                                              fquote(outValueCol),",\n      ",
                                              catToDummy,",\n      ",
                                              performNorm,",\n      ",
                                              performVarReduc,",\n      ",
                                              makeDataSparse,",\n      ",
                                              minStdDev,",\n      ",
                                              maxCorrel,",\n      ",
                                              trainOrTest,",\n      ",
                                              excludeCols,",\n      ",
                                              classSpec,",\n      ",
                                              whereClause,",\n      ",
                                              inAnalysisID,",\n      ",
                                              "AnalysisID);")
            t <- sqlQuery(connection,sqlstr,
                          AnalysisIDQuery="SELECT top 1 ANALYSISID from fzzlRegrDataPrepInfo order by RUNENDTIME DESC")
                
            dataprepID <- as.vector(t[1,1])

            table <- FLTable(outDeepTableDatabase,
                           deeptablename,
                           outObsIDCol,
                           outVarIDCol,
                           outValueCol
                          )
            return(list(table=table,
                        AnalysisID=dataprepID))

          }
        )
