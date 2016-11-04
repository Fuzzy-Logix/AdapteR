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
            type=type)
	}
	else
	{
        R <- sqlQuery(connection,
                      limitRowsSQL(paste0("select * from ",tableAndAlias(table)),1))
        cols <- names(R)
        if(!changeAlias(obs_id_colname,"","") %in% cols && !is.TDAster())
          stop(paste0(changeAlias(obs_id_colname,"",""),
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
                 type=type)
	}
}

##' Gives the column names of FLTable object
##'
##' @param object 
#' @export
names.FLTable <- function(object) object@Dimnames[[2]]
#' @export
colnames.FLTable <- function(object) object@Dimnames[[2]]
#' @export
rownames.FLTable <- function(object) object@Dimnames[[1]]

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
  vtablename <- x@select@table_name
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
  if(!x@isDeep){
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
#' resultList <- wideToDeep(widetable)
#' deeptable <- resultList$table
#' analysisID <- resultList$AnalysisID
#' @export
setGeneric("wideToDeep", function(object,excludeCols,classSpec,whereconditions,
                                  outDeepTableName,
                                  outObsIDCol,outVarIDCol,outValueCol) {
    standardGeneric("wideToDeep")
})

## move to file FLWidetoDeep.R
setMethod("wideToDeep",
          signature(object = "FLTable",
                    excludeCols="character",
                    classSpec="list",
                    whereconditions="character",
                    outDeepTableName="character",
                    outObsIDCol="character",
                    outVarIDCol="character",
                    outValueCol="character"
                    ),
          function(object,
                  excludeCols,
                  classSpec,
                  whereconditions,
                  outDeepTableName,
                  outObsIDCol,
                  outVarIDCol,
                  outValueCol)
          {
            if(object@isDeep) return(list(table=object))
            connection <- getFLConnection(object)
            object <- setAlias(object,"")
            if(outDeepTableName == "")
            deeptablename <- gen_deep_table_name(object@select@table_name)
            #deeptablename <- genRandVarName()
            else deeptablename <- outDeepTableName
            if(class(object@select)=="FLTableFunctionQuery")
            {
              ## Views are not working  in FLDeepToWide and FLWideToDeep
              # sqlstr <- paste0("CREATE VIEW ",outDeepTableDatabase,".",widetable," AS ",constructSelect(object))
              # sqlSendUpdate(connection,sqlstr)
              widetable <- createView(pViewName=gen_view_name(object@select@table_name),
                        pSelect=constructSelect(object))
              select <- new("FLSelectFrom",
                        connectionName = attr(connection,"name"), 
                        table_name = widetable, 
                        variables = list(
                                obs_id_colname = obs_id_colname),
                        whereconditions="",
                        order = "")

              object <- newFLTable(
                            select = select,
                            Dimnames = object@Dimnames,
                            dims = dim(object),
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
                              InWideTable=object@select@table_name,
                              ObsIDCol=getVariables(object)[["obs_id_colname"]],
                              OutDeepTable=deeptablename,
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

            updateMetaTable(pTableName=deeptablename,pType="deepTable")

            table <- FLTable(deeptablename,
                           outObsIDCol,
                           outVarIDCol,
                           outValueCol
                          )
            return(list(table=table,
                        AnalysisID=dataprepID))

          }
        )

## move to file FLWidetoDeep.R
setMethod("wideToDeep",
          signature(object = "FLTable",
                    excludeCols="missing",
                    classSpec="missing",
                    whereconditions="missing",
                    outDeepTableName="missing",
                    outObsIDCol="missing",
                    outVarIDCol="missing",
                    outValueCol="missing"),
          function(object)
          wideToDeep(object,
                    excludeCols="",
                    classSpec=list(),
                    whereconditions="",
                    outDeepTableName="",
                    outObsIDCol="",
                    outVarIDCol="",
                    outValueCol=""))

## move to file FLWidetoDeep.R
setMethod("wideToDeep",
          signature(object = "FLTable",
                    excludeCols="character",
                    classSpec="list",
                    whereconditions="character",
                    outDeepTableName="missing",
                    outObsIDCol="missing",
                    outVarIDCol="missing",
                    outValueCol="missing"),
          function(object,excludeCols,classSpec,whereconditions)
          wideToDeep(object,
                    excludeCols=excludeCols,
                    classSpec=classSpec,
                    whereconditions=whereconditions,
                    outDeepTableName="",
                    outObsIDCol="",
                    outVarIDCol="",
                    outValueCol=""))

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
                                  whereconditions,
                                  mapTable,
                                  mapName,
                                  outWideTableName,
                                  Analysisid
                                  ) {
    standardGeneric("deepToWide")
})

## move to file FLDeepToWide.R
setMethod("deepToWide",
          signature(object = "FLTable",
                    whereconditions="character",
                    mapTable="character",
                    mapName="character",
                    outWideTableName="character",
                    Analysisid = "character"),
          function(object,
                  whereconditions,
                  mapTable,
                  mapName,
                  outWideTableName,
                  Analysisid
                  )
          {
            #browser()
            if(!object@isDeep) return(list(table=object))
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
                                        a.FROM_TABLE
                                 FROM fzzlRegrDataPrepMap a 
                                 WHERE a.AnalysisID = '",Analysisid,"';")
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
              deeptable <- createView(pViewName=gen_view_name(object@select@table_name),
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
            outWideTableName <- gen_wide_table_name(object@select@table_name)
            #outWideTableName <- paste0(sample(letters[1:26],1),round(as.numeric(Sys.time())))

            message <- sqlStoredProc(
                              connection,
                              "FLDeepToWide",
                              outputParameter=c(MESSAGE="a"),
                              InDeepTable=object@select@table_name,
                              ObsIDCol="obs_id_colname",
                              VarIDCol="var_id_colname",
                              ValueCol="cell_val_colname",
                              MapTable=mapTable,
                              MapName=mapName,
                              WideTable=outWideTableName)

            # sqlstr<-paste0("CALL FLDeepToWide('",object@select@database,".",object@select@table_name,"',
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

## move to file FLDeepToWide.R
setMethod("deepToWide",
          signature(object = "FLTable",
                    whereconditions="missing",
                    mapTable="missing",
                    mapName="missing",
                    outWideTableName="missing",
                    Analysisid = "missing"
                   ),
          function(object)
          deepToWide(object,
                    whereconditions="",
                    mapTable="",
                    mapName="",
                    outWideTableName="",
                    Analysisid = ""
                    ))


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
#' @param depcol Name of the column in the wide table which represents the dependent variable
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
#' @param trainOrTest if  0 => Create training data set; if 1 => Create test data set using the transformation
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
#' @return \code{wideToDeep} returns a list containing components 
#' \code{table} which is the FLTable referencing the deep table and \code{AnalysisID} giving the AnalysisID of conversion
#' @examples
#' widetable  <- FLTable("tblAbaloneWide", "ObsID")
#' resultList <- wideToDeep(widetable)
#' deeptable <- resultList$table
#' analysisID <- resultList$AnalysisID
#' @export
setGeneric("FLRegrDataPrep", function(object,
                                  depCol,
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
                                  # trainOrTest,
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
          signature(object = "ANY",
                    depCol="character"
                    ),
          function(object,
                  depCol,
                  outDeepTableName=gen_deep_table_name(object@select@table_name),
                  outObsIDCol="obs_id_colname",
                  outVarIDCol="var_id_colname",
                  outValueCol="var_id_colname",
                  catToDummy=0,
                  performNorm=0,
                  performVarReduc=0,
                  makeDataSparse=1,
                  minStdDev=0,
                  maxCorrel=0,
                  trainOrTest=0,
                  excludeCols="",
                  classSpec=list(),
                  whereconditions="",
                  inAnalysisID="",
                  outGroupIDCol="group_id_colname",
                  fetchIDs=TRUE
                                        # ,...
                  )
          {
            if(object@isDeep) return(list(table=object))
            connection <- getFLConnection(object)
            object <- setAlias(object,"")
            if(outDeepTableName == "")
            deeptablename <- gen_deep_table_name(object@select@table_name)
            #deeptablename <- genRandVarName()
            else deeptablename <- outDeepTableName
            if(class(object@select)=="FLTableFunctionQuery")
            {
              ## Views are not working  in FLDeepToWide and FLWideToDeep
              widetable <- createView(pViewName=gen_view_name(object@select@table_name),
                                      pSelect=constructSelect(object))
              select <- new("FLSelectFrom",
                        connectionName = attr(connection,"name"), 
                        table_name = widetable, 
                        variables = list(
                                obs_id_colname = obs_id_colname),
                        whereconditions="",
                        order = "")

              object <- new(class(object),
                            select = select,
                            Dimnames = object@Dimnames,
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
            if(outGroupIDCol=="") outGroupIDCol <- "group_id_colname"

            if(is.FLTable(object)){
              vinputParams <- list(InWideTable=object@select@table_name,
                                ObsIDCol=getVariables(object)[["obs_id_colname"]],
                                DepCol=depCol,
                                OutDeepTable= deeptablename,
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
                                InAnalysisID=inAnalysisID)
              vfunName <- "FLRegrDataPrep"
            }
            if(is.FLTableMD(object)){
              vinputParams <- list(InWideTable=object@select@table_name,
                                    GroupID=getVariables(object)[["group_id_colname"]],
                                    ObsIDCol=getVariables(object)[["obs_id_colname"]],
                                    DepCol=depCol,
                                    OutDeepTable=deeptablename,
                                    OutGroupIDCol=outGroupIDCol,
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
                                    InAnalysisID=inAnalysisID)
              vfunName <- "FLRegrDataPrepMD"
            }

            retobj <- sqlStoredProc(connection,
                                    query=vfunName,
                                    outputParameter=c(AnalysisID="a"),
                                    pInputParams=vinputParams
                                    )
                
            dataprepID <- as.vector(retobj[1,1])

            updateMetaTable(pTableName=deeptablename, pType="deepTableMD")

            if(is.FLTableMD(object))
              table <- FLTableMD(deeptablename,
                               outGroupIDCol,
                               outObsIDCol,
                               outVarIDCol,
                               outValueCol,
                               group_id=object@Dimnames[[3]],
                               fetchIDs=fetchIDs
                              )
            else if(is.FLTable(object))
              table <- FLTable(deeptablename,
                               outObsIDCol,
                               outVarIDCol,
                               outValueCol,
                               # ObsID=rownames(object),
                               fetchIDs=fetchIDs
                              )
            return(list(table=table,
                        AnalysisID=dataprepID))
          }
        )

#' @export
FLSampleData <- function(pTableName,
                         pObsIDColumn,
                         pTrainDataRatio=0.7,
                         pTrainTableName=paste0(pTableName,
                                                "Train"),
                         pTestTableName=paste0(pTableName,
                                              "Test"),
                         pTemporary=getOption("temporaryFL"),
                         pDrop=TRUE
                         ){

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

  # vsqlstr <- paste0("CREATE ",ifelse(pTempTables,"VOLATILE TABLE ","TABLE "),
  #                   pTrainTableName," AS  ( ",
  #                   " SELECT  a.* FROM ",pTableName," a ",
  #                   " WHERE   FLSimUniform(RANDOM(1, 10000), 0, 1) < ",
  #                     pTrainDataRatio,") WITH DATA ",
  #                     " PRIMARY INDEX (",pObsIDColumn,")",
  #                     ifelse(pTempTables," ON COMMIT PRESERVE ROWS ","")
  #                     )
  # vtemp <- sqlSendUpdate(getFLConnection(),vsqlstr)

  # vsqlstr <- paste0("CREATE ",ifelse(pTempTables,"VOLATILE TABLE ","TABLE "),
  #                   pTestTableName," \n AS  ( \n ",
  #                   " SELECT  a.* FROM ",pTableName," a \n ",
  #                   " WHERE NOT EXISTS \n (SELECT 1 FROM ",
  #                     pTableName," b WHERE b.",
  #                     pObsIDColumn,"=a.",pObsIDColumn," \n ) \n )",
  #                   " WITH DATA \n PRIMARY INDEX(",pObsIDColumn,")",
  #                     ifelse(pTempTables," ON COMMIT PRESERVE ROWS ",""))
  # vtemp <- sqlSendUpdate(getFLConnection(),vsqlstr)
  return(c(TrainTableName=pTrainTableName,
          TestTableName=pTestTableName))
}
