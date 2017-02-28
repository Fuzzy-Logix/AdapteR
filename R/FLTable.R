#' @include FLMatrix.R
NULL

## move to file FLTable.R
#' Constructor function for FLTable.
#'
#' \code{FLTable} constructs an object of class \code{FLTable}.
#'
#' \code{FLTable} refers to an in-database table. This is equivalent to data.frame object. 
#' This object is commonly used as input for data mining functions.
#' @param connection ODBC/JDBC connection object
#' @param table name of the table
#' @param obs_id_colname column name set as primary key
#' @param var_id_colname column name where variable id's are stored if \code{FLTable} is deep
#' @param cell_val_colname column name where cell values are stored if \code{FLTable} is deep
#' @param whereconditions whereconditions if any to reference the table
#' @return \code{FLTable} returns an object of class FLTable mapped to a table
#' in Teradata.
#' @examples
#' widetable  <- FLTable("tblAbaloneWide", "ObsID")
#' deeptable <- FLTable("tblUSArrests","ObsID","VarID","Num_Val")
#' names(widetable)
#' @export
FLTable <- function(table,
                    obs_id_colname,
                    var_id_colnames=character(0), 
                    cell_val_colname=character(0),
                    whereconditions=character(0),
                    connection=getFLConnection(),
                    type="double",
                    fetchIDs=TRUE,
                    ...)
{
    ## If alias already exists, change it to flt.
    if(length(names(table))>0)
    oldalias <- names(table)[1]
    else oldalias <- ""
    var_id_colnames <- changeAlias(var_id_colnames,"flt",oldalias)
    obs_id_colname <- changeAlias(obs_id_colname,"flt",oldalias)
    cell_val_colname <- changeAlias(cell_val_colname,"flt",oldalias)
    whereconditions <- changeAlias(whereconditions,
                                   "flt",
                                   c(getTablename(table),
                                     oldalias))
    names(table) <- "flt"
    if(length(var_id_colnames) && length(cell_val_colname))
	{
        cols <- cleanNames(sort(sqlQuery(connection,
                                         paste0("SELECT DISTINCT(",
                                                var_id_colnames,") as VarID FROM ",tableAndAlias(table),
                                                " ",constructWhere(whereconditions)))[[1]]))
        ncol <- length(cols)
        if(!is.null(list(...)[["ObsID"]]))
          rows <- list(...)[["ObsID"]]
        else if(fetchIDs) {
          rows <- sort(sqlQuery(connection,
                         paste0("SELECT DISTINCT(",
                                obs_id_colname,") as VarID FROM ",tableAndAlias(table),
                          " ",constructWhere(whereconditions)))[[1]])
          rows <- cleanNames(rows)
          nrow <- length(rows)
        } else {
            rows <- NULL
            nrow <- sqlQuery(connection,
                            paste0("SELECT count(DISTINCT(",obs_id_colname,")) as N
                                    FROM ",tableAndAlias(table),
                                    " ",constructWhere(whereconditions)))[[1]]
        }


        ## To account for sparse format
        # vdimnames <- lapply(list(rows),
        #                     function(x){
        #                       if(is.numeric(x))
        #                       return(1:max(x))
        #                       else x
        #                       })
        # rows <- vdimnames[[1]]
        #cols <- vdimnames[[2]]

        select <- new("FLSelectFrom",
                      connectionName = attr(connection,"name"), 
                      table_name = table, 
                      variables = list(
                          obs_id_colname = obs_id_colname,
                          var_id_colname = var_id_colnames,
                          cell_val_colname = cell_val_colname),
                      whereconditions=whereconditions,
                      order = "")
        
        newFLTable(
            select = select,
            Dimnames = list(rows,cols),
            dims = as.integer(c(nrow,ncol)),
            isDeep = TRUE,
            type=type,
            dimColumns=c("obs_id_colname","var_id_colname","cell_val_colname"),...)
	}
	else
	{
        R <- sqlQuery(connection,
                      limitRowsSQL(paste0("select * from ",tableAndAlias(table)),1))
        cols <- names(R)
        cols <- changeAlias(cols,"","")
        ## @phani: is.TD() used here as in other platforms output 
        ## schema is always in lower case
        if(is.TD())
            vobsid <- changeAlias(obs_id_colname,"","")
        else vobsid <- tolower(changeAlias(obs_id_colname,"",""))
        if(!vobsid %in% cols)
          stop(paste0(vobsid,
                      " not a column in table.Please check case Sensitivity \n "))
        
        if(!is.null(list(...)[["ObsID"]])){
          rows <- list(...)[["ObsID"]]
          nrow <- length(rows)
        }
        else if(fetchIDs) {
          rows <- sort(sqlQuery(connection,
                            paste0("SELECT DISTINCT(",
                                        obs_id_colname,") as VarID
                                    FROM ",tableAndAlias(table),
                                    " ",constructWhere(whereconditions)))[[1]])
          rows <- cleanNames(rows)
          nrow <- length(rows)
        } else {
            rows <- NULL
            nrow <- sqlQuery(connection,
                            paste0("SELECT count(DISTINCT ",obs_id_colname,") as N
                                    FROM ",tableAndAlias(table),
                                    " ",constructWhere(whereconditions)))[[1]]
        }
        cols <- cleanNames(cols)
        
        if(length(var_id_colnames)==0)
            var_id_colnames <- cols
        if(length(setdiff(var_id_colnames,cols)))
            stop(paste0("columns do not exist: "))

        ncol <- length(var_id_colnames)

        mydimnames <- list(rows,var_id_colnames)
        select <- new("FLSelectFrom",
                      connectionName = attr(connection,"name"), 
                      table_name = table, 
                      variables = list(
                          obs_id_colname = obs_id_colname,
                                        #var_id_colname = var_id_colnames,
                          cell_val_colname = cell_val_colname),
                      whereconditions=whereconditions,
                      order = "")

        T <- newFLTable( 
                 select = select,
                 Dimnames = mydimnames,
                 dims = as.integer(c(nrow,ncol)),
                 isDeep = FALSE,
                 type=type,
                 dimColumns=c("obs_id_colname"))
	}
}

##' Gives the column names of FLTable object
##'
##' @param object 
#' @export
colnames <- function(object,...)
    UseMethod("colnames")
#' @export
colnames.default <- function(object,...)
    base::colnames(x=object,...)
#' @export
colnames.FLTable <- function(object,...){
    vcolnames <- object@Dimnames[[2]]
    if(is.null(vcolnames) && !is.null(dim(object)[2]))
        vcolnames <- 1:dim(object)[2]
    return(vcolnames)
}
#' @export
names.FLTable <- colnames.FLTable

#' @export
rownames <- function(object,...)
    UseMethod("rownames")
#' @export
rownames.default <- function(object,...)
    base::rownames(x=object,...)
#' @export
rownames.FLTable <- function(object){
    vrownames <- object@Dimnames[[1]]
    if(is.null(vrownames) && !is.null(dim(object)[1]))
        vrownames <- 1:dim(object)[1]
    return(vrownames)
}

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
  vcolnames <- x@Dimnames[[2]]
  vtablename <- getTableNameSlot(x)
  name <- gsub("\\.","",name,fixed=TRUE)
  xcopy <- x
  x <- setAlias(x,"")
  
  addColumnFLQuery <- function(pTable,pName,pValue){
    ##Get data type of pValue
    vColumnType <- getFLColumnType(x=pValue)
    sqlstr <- paste0("ALTER TABLE ",pTable," \n ",
                    " ADD ",pName," ",vColumnType,";")
    return(sqlSendUpdate(getFLConnection(),sqlstr))
  }
  if(!isDeep(x)){
    if(!tolower(name) %in% tolower(vcolnames)){
      vtemp <- addColumnFLQuery(pTable=vtablename,
                              pName=name,
                              pValue=value)
      vcolnames <- c(vcolnames,name)
    }
    else{
        types <- typeof(x)
        ## gk @ phani: I hacked this for the release, but I think going forward, getFLColumnType can be streamlined with typeof, and this can be simplified ....
        ##vtableColType <- getFLColumnType(x=as.vector(x[1,vcolnames[tolower(name)==tolower(vcolnames)][1]]))
        vnewColType <- getFLColumnType(x=value)
        dropCol <- FALSE
        # if(is.na(types[name])){
        #     dropCol <- TRUE ## gk @ phani:  in this case a drop is actually not required, but it works with drop...
        # } else 
        if(typeof(value)!=types[name])
            dropCol <- TRUE
        if(dropCol){
        vtemp <- sqlSendUpdate(getFLConnection(),
                                    paste0(" ALTER TABLE ",vtablename," DROP COLUMN ",name))
        vtemp <- addColumnFLQuery(pTable=vtablename,
                                  pName=name,
                                  pValue=value)
        }
    }
    if(!is.FLVector(value) & !inherits(value,"FLSimpleVector"))
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
      sqlstr <- paste0(" SELECT a.vectorIndexColumn, \n ",
                            name,
                            ", \n a.vectorValueColumn \n ",
                        " FROM(",constructSelect(value),") a;")
      insertIntotbl(pTableName=vtablename,
                    pSelect=sqlstr)
      sqlstr <- NULL
      vcolnames <- c(vcolnames,name)
    }
  }
  sqlSendUpdate(getFLConnection(),sqlstr)
  xcopy@Dimnames[[2]] <- vcolnames
  xcopy@type[name] <- typeof(value)
  return(xcopy)
}

## move to file FLWidetoDeep.R
#' Convert Wide Table to Deep Table in database.
#'
#' The DB Lytix function called is FLWideToDeep.  FLWideToDeep is similar to FLRegrDataPrep , except that 
#' FLWideToDeep does not require dependent variable or intercept, thus it creates deep tables without them.
#'
#' @param object FLTable object
#' @param excludeCols character vector specifying columns to be excluded from conversion
#' @param classSpec list representing Class specification which identifies then value of the categorical variable to be used a reference
#' @param whereconditions character vector giving where conditions if any to reference the wide table
#' @param outDeepTableName name to be given to the output deep table, possibly with database
#' @param outObsIDCol name to give to the primary key column name of the output deep table
#' @param outVarIDCol name to give to the varibales name column of the output deep table
#' @param outValueCol name to give to the value column of the output deep table
#' @return \code{wideToDeep} returns a list containing components 
#' \code{table} which is the FLTable referencing the deep table and \code{AnalysisID} giving the AnalysisID of conversion
#' @examples
#' widetable  <- FLTable("tblAbaloneWide", "ObsID")
#' deeptable <- wideToDeep(widetable)
#' analysisID <- deeptable@wideToDeepAnalysisID
#' @export
wideToDeep <- function(object,...)
    UseMethod("wideToDeep")

#' @export
wideToDeep.default <- function(object,
                                fetchIDs=TRUE,
                                ...){
    object <- setAlias(object,"")
    inputParams <- list(...)
    requiredParams <- list(InWideTable=getTableNameSlot(object),
                          ObsIDCol=getVariables(object)[["obs_id_colname"]],
                          OutDeepTable=gen_deep_table_name(getTableNameSlot(object)),
                          OutObsIDCol="obs_id_colname",
                          OutVarIDCol="var_id_colname",
                          OutValueCol="cell_val_colname",
                          ExcludeCols="",
                          ClassSpec=list(),
                          WhereClause=""
                          )
    inputParams <- setDefaultInputParams(requiredParams=requiredParams,
                                        inputParams=inputParams)
    return(FLGenericRegrDataPrep(object=object,
                                DepCol="NULL",
                                inputParams=inputParams,
                                fetchIDs=fetchIDs,
                                TrainOrTest=1,
                                funcName="FLWideToDeep",
                                useBoolean=FALSE))
}

#' @export
wideToDeep.FLTable.Hadoop <- function(object,
                                    fetchIDs=TRUE,
                                    ...){
    object <- setAlias(object,"")
    inputParams <- list(...)
    requiredParams <- list(InWideTable=getTableNameSlot(object),
                          ObsIDCol=getVariables(object)[["obs_id_colname"]],
                          OutDeepTable=gen_deep_table_name(getTableNameSlot(object)),
                          OutObsIDCol="obs_id_colname",
                          OutVarIDCol="var_id_colname",
                          OutValueCol="cell_val_colname",
                          MakeDataSparse=TRUE,
                          ExcludeCols="",
                          ClassSpec=list(),
                          WhereClause=""
                          )
    inputParams <- setDefaultInputParams(requiredParams=requiredParams,
                                        inputParams=inputParams)
    return(FLGenericRegrDataPrep(object=object,
                                DepCol="NULL",
                                inputParams=inputParams,
                                fetchIDs=fetchIDs,
                                TrainOrTest=1,
                                funcName="FLWideToDeep",
                                useBoolean=TRUE))
}

## move to file FLDeepToWide.R
#' Convert Deep Table to Wide Table
#'
#' The DB Lytix function called is FLWideToDeep.Stored Procedure that transforms the 
#' data in a deep table format to a wide table format.
#'
#' @param object FLTable object to convert to wide table
#' @param whereconditions character vector specifying whereconditions if any to reference the input deep table
#' @param mapTable name of the in-database table containing mapping information to be used in conversion if any
#' @param mapName unique identifier for the mapping information in mapping table if any
#' @param outWideTableName name to give to the output wide table
#' @return \code{deepToWide} returns a list containing components 
#' \code{table} which is the FLTable referencing the wide table and \code{AnalysisID} giving the AnalysisID of conversion
#' @examples
#' deeptable  <- FLTable("tblUSArrests", "ObsID","VarID","Num_Val")
#' resultList <- deepToWide(deeptable)
#' widetable <- resultList$table
#' analysisID <- resultList$AnalysisID
#' @export
setGeneric("deepToWide", function(object,
                                whereconditions="",
                                mapTable="",
                                mapName="",
                                outWideTableName="",
                                Analysisid = ""
                                  ) {
    standardGeneric("deepToWide")
})

## move to file FLDeepToWide.R
setMethod("deepToWide",
          signature(object = "FLTable"),
          function(object,
                    whereconditions="",
                    mapTable="",
                    mapName="",
                    outWideTableName="",
                    Analysisid = "")
          {
            #browser()
            if(!isDeep(object)) return(list(table=object))
            connection <- getFLConnection(object)
              object <- setAlias(object,"")
              ## gk: please use the common mapping table!
            usedwidetablename <- paste0(getOption("ResultDatabaseFL"),".",
                                      gen_wide_table_name("MAP"))
            if(mapTable=="" || mapTable=="NULL"){
              if(Analysisid!="")
              {
                sqlstr1<-paste0("DELETE FROM ",usedwidetablename,"; \n ")
                sqlSendUpdate(connection,sqlstr1)
                sqlstr1<-paste0(" SELECT a.Final_VarID, \n  
                                        a.COLUMN_NAME, \n 
                                        a.FROM_TABLE \n 
                                 FROM ",getSystemTableMapping("fzzlRegrDataPrepMap")," a \n ",
                                 " WHERE a.AnalysisID = ",fquote(Analysisid))
                insertIntotbl(pTableName=usedwidetablename,
                            pSelect=sqlstr1)
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
            
              #deeptable <- paste0(sample(letters[1:26],1),round(as.numeric(Sys.time())))
              # sqlstr <- paste0("CREATE VIEW ",outWideTableDatabase,".",deeptable," AS ",constructSelect(object))
              # sqlSendUpdate(connection,sqlstr)
              deeptable <- createView(pViewName=gen_view_name(getTableNameSlot(object)),
                        pSelect=constructSelect(object))
              select <- new("FLSelectFrom",
                        connectionName = attr(connection,"name"), 
                        table_name = deeptable, 
                        variables = list(
                                obs_id_colname = "obs_id_colname",
                                var_id_colname = "var_id_colname",
                                cell_val_colname = "cell_val_colname"),
                        whereconditions="",
                        order = "")

              object <- newFLTable(
                            select = select,
                            dims = dim(object),
                            Dimnames = object@Dimnames,
                            isDeep = TRUE)
            # if(class(object@select)=="FLTableFunctionQuery" || length(whereconditions)>0)
            # object <- store(object)

            if(outWideTableName=="")
            outWideTableName <- gen_wide_table_name(getTableNameSlot(object))
            #outWideTableName <- paste0(sample(letters[1:26],1),round(as.numeric(Sys.time())))

            message <- sqlStoredProc(
                              connection,
                              "FLDeepToWide",
                              outputParameter=c(Message="Message"),
                              DeepTable=getTableNameSlot(object),
                              ObsIDCol="obs_id_colname",
                              VarIDCol="var_id_colname",
                              ValueCol="cell_val_colname",
                              MapTable=mapTable,
                              MapName=mapName,
                              WideTable=outWideTableName)

            # sqlstr<-paste0("CALL FLDeepToWide('",object@select@database,".",getTableNameSlot(object),"',
            #                                   'obs_id_colname',
            #                                   'var_id_colname',
            #                                   'cell_val_colname',",
            #                                   ifelse(mapTable=="NULL",mapTable,paste0("'",mapTable,"'")),",",
            #                                   ifelse(mapName=="NULL",mapName,paste0("'",mapName,"'")),",'",
            #                                   paste0(outWideTableDatabase,".",outWideTableName),
            #                                   "',MESSAGE);")
            #message <- sqlQuery(connection,sqlstr)
            message <- checkSqlQueryOutput(message)

            updateMetaTable(pTableName=outWideTableName,
                            pType="wideTable")

            table <- FLTable(
                           outWideTableName,
                           "obs_id_colname"
                          )
            return(list(table=table,
                        message = as.character(message[1,1])))
          }
        )

## move to file FLRegrDataPrep.R
#' Convert Wide Table to Deep Table in database.
#'
#' The DB Lytix function called is FLRegrDataPrep. In DB Lytix, data mining functions such as linear 
#' regression, logistic regression, Generalized Linear Model (GLM), etc. are performed using stored procedures on a deep table.
#' However, in most situations, the input data is usually stored in wide tables containing multiple columns. The
#' stored procedure FLRegrDataPrep facilitates the conversion of contents stored in wide tables or views to
#' deep tables and also prepares the data for regression analysis.
#'
#' @param object FLTable object
#' @param DepCol Name of the column in the wide table which represents the dependent variable
#' @param catToDummy Transform categorical variables to numerical values either using dummy variables or by using Empirical Logit.
#' @param performNorm Perform standardization of data. Standardization is done if the value of this parameter is 1.
#' @param performVarReduc Perform variable reduction.Elimination means that the corresponding column is not transformed from the
#' wide format to the deep format. Variables with standard deviation below the specified threshold are eliminated.
#' Similarly, if a pair of columns has correlation above the specified threshold, one of the columns is not transformed.
#' @param makeDataSparse Make data sparse i.e., only store non-zero values in the deep table for the independent variables. The
#' column values for those observations that are zero are not stored in the deep table. However, for the
#' dependent variable and the intercept, zero values are stored in the deep table. 
#' @param minStdDev Minimum acceptable standard deviation for elimination of variables. Any variable that has a
#' standard deviation below this threshold is eliminated. This parameter is only consequential if the parameter PerformVarReduc = 1.
#' @param maxCorrel Maximum acceptable absolute correlation between a pair of columns for eliminating variables. If the
#' absolute value of the correlation exceeds this threshold, one of the columns is not transformed. Again, this parameter is 
#' only consequential if the parameter PerformVarReduc = 1.
#' @param TrainOrTest if  0 => Create training data set; if 1 => Create test data set using the transformation
#' that has already been done for a prior training data set.
#' @param inAnalysisID provided in case we want to re-run the transformation of a training data set or run the
#' transformation of a testing data set else this value is NULL.
#' @param excludeCols character vector specifying columns to be excluded from conversion
#' @param classSpec list representing Class specification which identifies then value of the categorical variable to be used a reference
#' @param whereconditions character vector giving where conditions if any to reference the wide table
#' @param outDeepTableName name to be given to the output deep table, possibly including database
#' @param outObsIDCol name to give to the primary key column name of the output deep table
#' @param outVarIDCol name to give to the varibales name column of the output deep table
#' @param outValueCol name to give to the value column of the output deep table
#' @return \code{wideToDeep} returns a FLTableDeep referencing the deep table, the original table and \code{AnalysisID} giving the AnalysisID of conversion
#' @examples
#' widetable  <- FLTable("tblAbaloneWide", "ObsID")
#' deeptable <- FLRegrDataPrep(widetable,"Diameter")
#' analysisID <- deeptable@wideToDeepAnalysisID
#' @export
setGeneric("FLRegrDataPrep", function(object,
                                  depCol="NULL",
                                  # outDeepTableName,
                                  # outObsIDCol,
                                  # outVarIDCol,
                                  # outValueCol,
                                  # catToDummy,
                                  # performNorm,
                                  # performVarReduc,
                                  # makeDataSparse,
                                  # minStdDev,
                                  # maxCorrel,
                                  # TrainOrTest,
                                  # excludeCols,
                                  # classSpec,
                                  # whereconditions,
                                  # inAnalysisID,
                                  # outGroupIDCol,
                                  ...) {
    standardGeneric("FLRegrDataPrep")
})

## move to file FLRegrDataPrep.R
setMethod("FLRegrDataPrep",
          signature(object = "ANY"
                    ),
          function(object,
                  depCol="NULL",
                  fetchIDs=TRUE,
                  ...
                  )
            {
                object <- setAlias(object,"")
                vinputParams <- list(...)
                if(!"TrainOrTest" %in% names(vinputParams))
                    vinputParams[["TrainOrTest"]] <- 0

                if(vinputParams[["TrainOrTest"]]==0)
                    return(FLTrainDataPrep(object=object,
                                           DepCol=depCol,
                                           inputParams=vinputParams,
                                           fetchIDs=fetchIDs))
                else return(FLTestDataPrep(object=object,
                                           DepCol=depCol,
                                           inputParams=vinputParams,
                                           fetchIDs=fetchIDs))
            })

FLTrainDataPrep <- function(object,
                            DepCol,
                            inputParams,
                            fetchIDs=TRUE){
    UseMethod("FLTrainDataPrep")
}

setDefaultInputParams <- function(requiredParams,
                                  inputParams){
    vtemp <- sapply(names(requiredParams),
                    function(x){
                        if(!x %in% names(inputParams))
                            inputParams[[x]] <- requiredParams[[x]]
                    })
    return(append(inputParams,vtemp))
}

FLTrainDataPrep.default <- function(object,
                                    DepCol,
                                    inputParams,
                                    fetchIDs=TRUE){
    requiredParams <- list(InWideTable=getTableNameSlot(object),
                          ObsIDCol=getVariables(object)[["obs_id_colname"]],
                          DepCol=DepCol,
                          OutDeepTable=gen_deep_table_name(getTableNameSlot(object)),
                          OutObsIDCol="obs_id_colname",
                          OutVarIDCol="var_id_colname",
                          OutValueCol="cell_val_colname",
                          CatToDummy=0,
                          PerformNorm=0,
                          PerformVarReduc=0,
                          MakeDataSparse=1,
                          MinStdDev=0.0,
                          MaxCorrel=0.0,
                          ExcludeCols="",
                          ClassSpec=list(),
                          WhereClause="",
                          InAnalysisID=""
                          )
    inputParams <- setDefaultInputParams(requiredParams=requiredParams,
                                        inputParams=inputParams)
    
    return(FLGenericRegrDataPrep(object=object,
                                DepCol=DepCol,
                                inputParams=inputParams,
                                fetchIDs=fetchIDs,
                                TrainOrTest=0,
                                funcName="FLRegrDataPrep",
                                MDFlag=FALSE))
}

FLTrainDataPrep.FLTable.Hadoop <- function(object,
                                           DepCol,
                                           inputParams,
                                           fetchIDs=TRUE){
    inputParams[["TrainOrTest"]] <- NULL
    requiredParams <- list(InWideTable=getTableNameSlot(object),
                          ObsIDCol=getVariables(object)[["obs_id_colname"]],
                          DepCol=DepCol,
                          OutDeepTable=gen_deep_table_name(getTableNameSlot(object)),
                          OutObsIDCol="obs_id_colname",
                          OutVarIDCol="var_id_colname",
                          OutValueCol="cell_val_colname",
                          CatToDummy=FALSE,
                          PerformNorm=FALSE,
                          PerformVarReduc=FALSE,
                          MakeDataSparse=TRUE,
                          MinStdDev=0.0,
                          MaxCorrel=0.0,
                          ExcludeCols="",
                          ClassSpec=list(),
                          WhereClause="",
                          InAnalysisID=""
                          )
    inputParams <- setDefaultInputParams(requiredParams=requiredParams,
                                        inputParams=inputParams)
    return(FLGenericRegrDataPrep(object=object,
                                DepCol=DepCol,
                                inputParams=inputParams,
                                fetchIDs=fetchIDs,
                                TrainOrTest=0,
                                funcName="FLTrainDataPrep",
                                MDFlag=FALSE,
                                useBoolean=TRUE))
}


FLTrainDataPrep.FLTableMD.TD <- function(object,
                                    DepCol,
                                    inputParams,
                                    fetchIDs=TRUE){
    if(!"OutGroupIDCol" %in% names(inputParams))
        inputParams[["OutGroupIDCol"]] <- "group_id_colname"
    requiredParams <- list(InWideTable=getTableNameSlot(object),
                          GroupID=getVariables(object)[["group_id_colname"]],
                          ObsIDCol=getVariables(object)[["obs_id_colname"]],
                          DepCol=DepCol,
                          OutDeepTable=gen_deep_table_name(getTableNameSlot(object)),
                          OutGroupIDCol="group_id_colname",
                          OutObsIDCol="obs_id_colname",
                          OutVarIDCol="var_id_colname",
                          OutValueCol="cell_val_colname",
                          CatToDummy=0,
                          PerformNorm=0,
                          PerformVarReduc=0,
                          MakeDataSparse=1,
                          MinStdDev=0.0,
                          MaxCorrel=0.0,
                          ExcludeCols="",
                          ClassSpec=list(),
                          WhereClause="",
                          InAnalysisID=""
                          )
    inputParams <- setDefaultInputParams(requiredParams=requiredParams,
                                        inputParams=inputParams)
    return(FLGenericRegrDataPrep(object=object,
                                DepCol=DepCol,
                                inputParams=inputParams,
                                fetchIDs=fetchIDs,
                                TrainOrTest=0,
                                funcName="FLRegrDataPrepMD",
                                MDFlag=TRUE))
}

FLTrainDataPrep.FLTableMD.Hadoop <- function(object,
                                           DepCol,
                                           inputParams,
                                           fetchIDs=TRUE){
    stop("currently not supported \n ")
    # inputParams[["TrainOrTest"]] <- NULL
    # return(FLGenericRegrDataPrep(object=object,
    #                             DepCol=DepCol,
    #                             inputParams=inputParams,
    #                             fetchIDs=fetchIDs,
    #                             TrainOrTest=0,
    #                             funcName="FLTrainDataPrepMD",
    #                             MDFlag=TRUE))
}

FLTrainDataPrep.FLTableMD.TDAster <- FLTrainDataPrep.FLTableMD.Hadoop

## Test Data Preparation in-Database
FLTestDataPrep <- function(object,
                           DepCol="NULL",
                           inputParams,
                           fetchIDs=TRUE){
    UseMethod("FLTestDataPrep")
}

FLTestDataPrep.default <- FLTrainDataPrep.default

FLTestDataPrep.FLTable.Hadoop <- function(object,
                                           DepCol="NULL",
                                           inputParams,
                                           fetchIDs=TRUE){
    inputParams[["TrainOrTest"]] <- NULL
    requiredParams <- list(InWideTable=getTableNameSlot(object),
                          ObsIDCol=getVariables(object)[["obs_id_colname"]],
                          OutDeepTable=gen_deep_table_name(getTableNameSlot(object)),
                          OutObsIDCol="obs_id_colname",
                          OutVarIDCol="var_id_colname",
                          OutValueCol="cell_val_colname",
                          WhereClause="",
                          MakeDataSparse=TRUE,
                          InAnalysisID=""
                          )
    inputParams <- setDefaultInputParams(requiredParams=requiredParams,
                                        inputParams=inputParams)
    return(FLGenericRegrDataPrep(object=object,
                                DepCol=DepCol,
                                inputParams=inputParams,
                                fetchIDs=fetchIDs,
                                TrainOrTest=1,
                                funcName="FLTestDataPrep",
                                MDFlag=FALSE,
                                useBoolean=TRUE))
}

checkInputParamsRegrDataPrep <- function(object,
                                        DepCol,
                                        inputParams,
                                        TrainOrTest=0,
                                        useBoolean=FALSE){

    ##Set defaults to all variables commonly used in
    ##DataPrep
    ##browser()
    vdefaults <- list(OutDeepTable=gen_deep_table_name("AR"),
                      OutObsIDCol="obs_id_colname",
                      OutVarIDCol="var_id_colname",
                      OutValueCol="cell_val_colname",
                      CatToDummy=0,
                      PerformNorm=0,
                      PerformVarReduc=0,
                      MakeDataSparse=1,
                      MinStdDev=0.0,
                      MaxCorrel=0.0,
                      ExcludeCols="",
                      ClassSpec=list(),
                      WhereClause="",
                      InAnalysisID="",
                      OutGroupIDCol="group_id_colname")

    vinputParams <- inputParams
    vtemp <- sapply(names(vdefaults),
                    function(x){
                        if(!x %in% names(vinputParams))
                            assign(x,vdefaults[[x]],envir=parent.env(environment()))
                    })
    vtemp <- sapply(names(vinputParams),
                function(x){
                    assign(x,vinputParams[[x]],
                            envir=parent.env(environment()))
                })

    if(OutDeepTable == "")
        OutDeepTable <- gen_deep_table_name("AR")

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
    checkParamsLM(c(CatToDummy,PerformNorm,PerformVarReduc,
      MakeDataSparse,TrainOrTest),
                    list(c(0,1),c(0,1),c(0,1),c(0,1,2),c(0,1)))

    vIn <- c("CatToDummy","PerformNorm","PerformVarReduc",
              "MakeDataSparse","TrainOrTest")

    vtemp <- sapply(1:5,function(x){
      assign(vIn[x],vresult[x],envir=parent.env(environment()))
      })

    if(!is.numeric(MinStdDev) || !MinStdDev>=0)
    MinStdDev <- 0.0
    if(!is.numeric(MaxCorrel) || MaxCorrel<=0 || MaxCorrel>1)
    MaxCorrel <- 0.0

    if(TrainOrTest==1) DepCol <- "NULL"
    else if(!(DepCol %in% colnames(object)))
    stop(DepCol," not in colnames of input table for FLRegrDataPrep")

    # if(TrainOrTest==1 && InAnalysisID %in% c("NULL",""))
    # stop("inAnalysisID should be valid when TrainOrTest=1")
    if(InAnalysisID=="" || is.null(InAnalysisID)) InAnalysisID <- "NULL"
    else InAnalysisID <- InAnalysisID

    if(length(ClassSpec)==0 || ClassSpec=="") ClassSpec <- "NULL"
    else{
      ClassSpec <- paste0(list_to_class_spec(ClassSpec))
      CatToDummy <- 1
    }
    WhereClause <- c(WhereClause,object@select@whereconditions)
    WhereClause <- constructWhere(WhereClause,
                                includeWhere=getStoredProcMapping("includeWhere"))
    if(WhereClause=="") WhereClause <- "NULL"
    else
    WhereClause <- paste0(WhereClause)
    if(ExcludeCols=="" || length(ExcludeCols)==0) ExcludeCols <- "NULL"
    else
    ExcludeCols <- paste0(ExcludeCols)

    if(useBoolean){
        sapply(c("CatToDummy","PerformNorm",
                "PerformVarReduc","MakeDataSparse"),
            function(x){
                assign(x,as.logical(get(x)),envir=parent.env(environment()))
      })
    }
    if(OutObsIDCol=="") OutObsIDCol <- "obs_id_colname"
    if(OutVarIDCol=="") OutVarIDCol <- "var_id_colname"
    if(OutValueCol=="") OutValueCol <- "cell_val_colname"
    if(OutGroupIDCol=="") OutGroupIDCol <- "group_id_colname"
    vinputParams <- lapply(1:length(inputParams),
                        function(x){
                            get(names(inputParams)[x])
                        })
    names(vinputParams) <- names(inputParams)
    vinputParams
}

createViewDataPrep <- function(object){
    object <- setAlias(object,"")
    if(class(object@select)=="FLTableFunctionQuery")
    {
      ## Views are not working  in FLDeepToWide and FLWideToDeep
      widetable <- createView(pViewName=gen_view_name(getTableNameSlot(object)),
                              pSelect=constructSelect(object))
      select <- new("FLSelectFrom",
                connectionName = attr(connection,"name"), 
                table_name = widetable, 
                variables = list(
                        obs_id_colname = getVariables(object)[["obs_id_colname"]]),
                whereconditions="",
                order = "")

      object <- new(class(object),
                    select = select,
                    Dimnames = object@Dimnames,
                    isDeep = FALSE)
      #object <- store(object)
    }
    return(object)
}

FLGenericRegrDataPrep <- function(object,
                                  DepCol,
                                  inputParams,
                                  fetchIDs=TRUE,
                                  TrainOrTest=0,
                                  funcName="FLRegrDataPrep",
                                  MDFlag=FALSE,
                                  useBoolean=FALSE
                                  )
{
    if(isDeep(object)) return(list(table=object))
    connection <- getFLConnection(object)
    
    object <- createViewDataPrep(object)
    inputParams <- checkInputParamsRegrDataPrep(object=object,
                                                DepCol=DepCol,
                                                inputParams=inputParams,
                                                TrainOrTest=TrainOrTest,
                                                useBoolean=useBoolean)
    deeptablename <- inputParams[["OutDeepTable"]]
    retobj <- sqlStoredProc(connection,
                            query=funcName,
                            outputParameter=c(AnalysisID="AnalysisID"),
                            pInputParams=inputParams
                            )
        
    dataprepID <- as.vector(retobj[1,1])
    
    updateMetaTable(pTableName=deeptablename, pType="deepTableMD")

    if(MDFlag)
        table <- FLTableMD(table=deeptablename,
                           group_id_colname=inputParams[["OutGroupIDCol"]],
                           obs_id_colname=inputParams[["OutObsIDCol"]],
                           var_id_colname=inputParams[["OutVarIDCol"]],
                           cell_val_colname=inputParams[["OutValueCol"]],
                           group_id=object@Dimnames[[3]],
                           fetchIDs=fetchIDs,
                           wideToDeepAnalysisID=dataprepID,
                           wideTable=object
                           )
    else
        table <- FLTable(table=deeptablename,
                         obs_id_colname=inputParams[["OutObsIDCol"]],
                         var_id_colname=inputParams[["OutVarIDCol"]],
                         cell_val_colname=inputParams[["OutValueCol"]],
                         fetchIDs=fetchIDs,
                         wideToDeepAnalysisID=dataprepID,
                         wideTable=object
                         )
    return(table)
}

#' @export
SampleData <- function(pTableName,
                         pObsIDColumn,
                         pTrainDataRatio=0.7,
                         pTrainTableName=paste0(pTableName,
                                                "Train"),
                         pTestTableName=paste0(pTableName,
                                              "Test"),
                         pTemporary=getOption("temporaryFL"),
                         pDrop=TRUE,
                         ...
                         ){

    if(is.Hadoop())
        vsqlstr <- paste0(" SELECT  a.* FROM ",pTableName," a ",
                    " WHERE  RAND() < ",
                      pTrainDataRatio," ")
    else
        vsqlstr <- paste0(" SELECT  a.* FROM ",pTableName," a ",
                        " WHERE   FLSimUniform(RANDOM(1, 10000), 0, 1) < ",
                          pTrainDataRatio," ")
    vtemp <- createTable(pTableName=pTrainTableName,
                      pPrimaryKey=pObsIDColumn,
                      pTemporary=pTemporary,
                      pDrop=pDrop,
                      pSelect=vsqlstr)

    vsqlstr <- paste0(" SELECT  a.* FROM ",pTableName," a \n ",
                    " WHERE NOT EXISTS \n (SELECT 1 FROM ",
                      pTrainTableName," b WHERE b.",
                      pObsIDColumn,"=a.",pObsIDColumn," \n ) ")
    vtemp <- createTable(pTableName=pTestTableName,
                      pPrimaryKey=pObsIDColumn,
                      pTemporary=pTemporary,
                      pDrop=pDrop,
                      pSelect=vsqlstr)
    return(c(TrainTableName=pTrainTableName,
            TestTableName=pTestTableName))
}


#' Data Preparation
#'
#' Process deep table to have consistent 
#' obs and var ids
#' @param data name of table in the database
#' @param formula should be of form column1~column2
#' @param value.var column containing the values
#' @param outTable name of output table
#' @param deepOutput TRUE if output has to be a deep table
#' @return \code{list} of \code{table} FLTable object, \code{Dimnames} Mappings
#' @examples
#' resultList <- FLReshape(data="medEconomicData",
#'                         formula=CountryName ~ IndicatorCode,
#'                         value.var="TimeSeriesVal",
#'                         subset="IndicatorCode in ('NY.GDP.MKTP.KD.ZG','FP.CPI.TOTL.ZG') and Year=2010",
#'                         outTable="tbl1",
#'                         drop=TRUE)
#' @export
FLReshape <- function(data,formula,
                     value.var,subset=NULL,
                     outTable=paste0("ARBase",data,"Reshape"),
                     deepOutput=TRUE,
                     ...){
    vallVars <- all.vars(formula)
    vobsid <- vallVars[1]
    vvarid <- vallVars[2]

    if("drop" %in% names(list(...)))
        if(list(...)$drop)
            vres <- dropTable(pTableName=outTable)

    if("temporary" %in% names(list(...)))
        vtemporary <- list(...)$temporary
    else vtemporary <- FALSE

    vdepColname <- NULL
    if("dependentColumn" %in% names(list(...))){
        vdepColname <- setdiff(list(...)[["dependentColumn"]],"")
    }
    vIncludeIntercept <- FALSE
    if("includeIntercept" %in% names(list(...))){
        vIncludeIntercept <- list(...)[["includeIntercept"]]
    }

    if(length(vdepColname)>0){
        vWhereClause <- constructWhere(c(subset,
                                        paste0(vvarid," NOT IN(",
                                            fquote(vdepColname),")")))
    }
    if(deepOutput){
        sqlstr <- paste0(" SELECT DENSE_RANK()OVER(PARTITION BY b.varid ORDER BY b.obsid) as obsid, \n ",
                                "DENSE_RANK()OVER(PARTITION BY b.obsid ORDER BY b.varid) as varid, \n ",
                                "b.num_val as num_val, \n ",
                                "b.obsid as obsidnames, \n ",
                                "b.varid as varidnames \n ",
                        " FROM ( \n ",
                            " SELECT ",vobsid," as obsid, count(DISTINCT ",vvarid,") as varidcount \n ",
                            " FROM ",data," \n ",vWhereClause,
                            " \n GROUP BY ",vobsid,") a, \n ",
                            " (SELECT COUNT(DISTINCT ",vvarid,") as maxvarid FROM ",data," \n ",vWhereClause,
                            " \n ) c, \n ",
                            " (SELECT ",vobsid," as obsid,",vvarid," as varid,",value.var," as num_val \n ",
                                " FROM ",data," \n ",vWhereClause,") b \n ",
                        " WHERE a.obsid = b.obsid AND a.varidcount = c.maxvarid "
                        )

        vres <- createTable(pTableName=outTable,
                            pSelect=sqlstr,
                            pTemporary=vtemporary,
                            pDrop=TRUE)

        if(length(vdepColname)>0){
            vres <- insertIntotbl(pTableName=outTable,
                                  pSelect=paste0("SELECT ROW_NUMBER()OVER(ORDER BY ",vobsid,"), -1, ",
                                                        value.var,",",vobsid,",",fquote(vdepColname)," FROM \n ",
                                                    data," \n WHERE ",vvarid," IN (",fquote(vdepColname),")"))
        }
        if(vIncludeIntercept){
            vres <- insertIntotbl(pTableName=outTable,
                                  pSelect=paste0("SELECT ROW_NUMBER()OVER(ORDER BY ",vobsid,"), 0, 1, ",
                                                        vobsid,", 'Intercept' FROM \n ",
                                                    data," \n WHERE ",vvarid," IN (",fquote(vdepColname),")"))
        }
        ## TODO: standardization of data

        vres <- sqlQuery(getFLConnection(),
                        paste0("SELECT MAX(obsid) as vrows, MAX(varid) as vcols FROM ",outTable))
        rows <- vres[["vrows"]]
        cols <- vres[["vcols"]]

        ## Mappings
        sqlstr <- paste0("SELECT DISTINCT '%insertIDhere%' AS vectorIdColumn, \n ",
                            " obsid AS vectorIndexColumn, \n ",
                            " obsidnames AS vectorValueColumn \n ",
                        " FROM ",outTable)

        tblfunqueryobj <- new("FLTableFunctionQuery",
                                  connectionName = attr(getFLConnection(),"name"),
                                  variables = list(
                                      obs_id_colname = "vectorIndexColumn",
                                      cell_val_colname = "vectorValueColumn"),
                                  whereconditions="",
                                  order = "",
                                  SQLquery=sqlstr)
        Rownames <- newFLVector(
                       select = tblfunqueryobj,
                       Dimnames = list(1:rows,"vectorValueColumn"),
                       isDeep = FALSE,
                       type="character")

        sqlstr <- paste0("SELECT DISTINCT '%insertIDhere%' AS vectorIdColumn, \n ",
                            " varid AS vectorIndexColumn, \n ",
                            " varidnames AS vectorValueColumn \n ",
                        " FROM ",outTable)

        tblfunqueryobj <- new("FLTableFunctionQuery",
                                  connectionName = attr(getFLConnection(),"name"),
                                  variables = list(
                                      obs_id_colname = "vectorIndexColumn",
                                      cell_val_colname = "vectorValueColumn"),
                                  whereconditions="",
                                  order = "",
                                  SQLquery=sqlstr)
        Colnames <- newFLVector(
                       select = tblfunqueryobj,
                       Dimnames = list(1:cols,"vectorValueColumn"),
                       isDeep = FALSE,
                       type="character")

        ##

        select <- new("FLSelectFrom",
                      connectionName = attr(getFLConnection(),"name"), 
                      table_name = outTable, 
                      variables = list(
                          obs_id_colname = "obsid",
                          var_id_colname = "varid",
                          cell_val_colname = "num_val"),
                      whereconditions="",
                      order = "")
        
        deepTable <- newFLTable(
                        select = select,
                        Dimnames = list(1:rows,1:cols),
                        dims = as.integer(c(rows,cols)),
                        isDeep = TRUE,
                        type="double",
                        dimColumns=c("obs_id_colname","var_id_colname","cell_val_colname")
                        )
        return(list(table=deepTable,
                    Dimnames=list(Rownames,Colnames)))

    }
    else stop("yet to be implemented.Please leave a comment on github. \n ")

}
