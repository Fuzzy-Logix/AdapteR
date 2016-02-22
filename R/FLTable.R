#' @include utilities.R
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
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' widetable  <- FLTable(connection, "FL_Deep", "tblAbaloneWide", "ObsID")
#' deeptable <- FLTable(connection,"FL_DEMO","tblUSArrests","ObsID","VarID","Num_Val")
#' names(widetable)
#' @export
FLTable <- function(connection,
				    database, 
				    table,
				    obs_id_colname,
                    var_id_colnames=character(0), 
				    cell_val_colname=character(0),
                    whereconditions=character(0))
{

    ##browser()
    ## validate_args( 	list(database = database, table = table),
    ## 				list(database = "character", table = "character")
    ## )
	## if(xor(length(var_id_colnames) , length(cell_val_colname)))
	## {
	## 	stop("Unable to identify whether table is deep or wide")
	## }
    if(length(var_id_colnames) && length(cell_val_colname))
	{
        ##browser()
        ##R <- sqlQuery(connection,paste0("HELP table ",remoteTable(database,table)))
		##cols <- R[["Column Name"]]
        # R <- sqlQuery(connection,paste0("select top 1 * from ",remoteTable(database,table)))
        # cols <- names(R)
        ##R <- sqlQuery(connection,paste0("HELP table ",remoteTable(database,table)))
		##cols <- R[["Column Name"]]
        cols <- sort(sqlQuery(connection,
                         paste0("SELECT DISTINCT(",
                                var_id_colnames,") as VarID
                          FROM ",remoteTable(database,table),
                          " ",constructWhere(whereconditions)))$VarID)
        rows <- sort(sqlQuery(connection,
                         paste0("SELECT DISTINCT(",
                                obs_id_colname,") as VarID
						  FROM ",remoteTable(database,table),
                          " ",constructWhere(whereconditions)))$VarID)
        cols <- gsub("^ +| +$","",cols)
        rows <- gsub("^ +| +$","",rows)

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
        ##browser()
        ##R <- sqlQuery(connection,paste0("HELP table ",remoteTable(database,table)))
		##cols <- R[["Column Name"]]
        R <- sqlQuery(connection,paste0("select top 1 * from ",remoteTable(database,table)))
        cols <- names(R)
        rows <- sort(sqlQuery(connection,
                            paste0("SELECT DISTINCT(",
                                obs_id_colname,") as VarID
						  FROM ",remoteTable(database,table),
                          " ",constructWhere(whereconditions)))$VarID)
        cols <- gsub("^ +| +$","",cols)
        rows <- gsub("^ +| +$","",rows)
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
names.FLTable <- function(object) object@dimnames[[2]]
colnames.FLTable <- function(object) object@dimnames[[2]]
rownames.FLTable <- function(object) object@dimnames[[1]]


setMethod("show","FLTable",function(object) print(as.data.frame(object)))

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
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' widetable  <- FLTable(connection, "FL_DEMO", "tblAbaloneWide", "ObsID")
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
            if(outDeepTableName == "")
            deeptablename <- gen_deep_table_name(object@select@table_name)
            #deeptablename <- genRandVarName()
            else deeptablename <- outDeepTableName
            if(outDeepTableDatabase == "")
            outDeepTableDatabase <- getOption("ResultDatabaseFL")
            if(class(object@select)=="FLTableFunctionQuery")
            {
              ## Views are not working  in FLDeepToWide and FLWideToDeep
              # widetable <- gen_wide_table_name(object@select@table_name)
              # sqlstr <- paste0("CREATE VIEW ",outDeepTableDatabase,".",widetable," AS ",constructSelect(object))
              # sqlSendUpdate(connection,sqlstr)
              # select <- new("FLSelectFrom",
              #           connection = connection, 
              #           database = outDeepTableDatabase, 
              #           table_name = widetable, 
              #           variables = list(
              #                   obs_id_colname = obs_id_colname),
              #           whereconditions="",
              #           order = "")

              # object <- new("FLTable",
              #               select = select,
              #               dimnames = object@dimnames,
              #               isDeep = FALSE)
              object <- store(object)
            }

            if(length(classSpec)==0) classSpec <- "NULL"
            else
            classSpec <- paste0("'",list_to_class_spec(classSpec),"'")
            whereconditions <- c(whereconditions,object@select@whereconditions)
            #whereconditions <- whereconditions[whereconditions!="" && whereconditions!="NULL"]
            #print(whereconditions)
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

            sqlstr<-paste0("CALL FLWideToDeep('",object@select@database,".",object@select@table_name,"','",
                                              getVariables(object)[["obs_id_colname"]],"','",
                                              outDeepTableDatabase,".",deeptablename,
                                              "','",outObsIDCol,"',
                                              '",outVarIDCol,"',
                                              '",outValueCol,"',",
                                              excludeCols,",",
                                              classSpec,",",
                                              whereClause,
                                              ",AnalysisID);")
            t<- sqlQuery(connection,sqlstr)
                
            dataprepID <- as.vector(t[1,1])

            table <- FLTable(connection,
                           outDeepTableDatabase,
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
#' library(RODBC)
#' connection <- odbcConnect("Gandalf")
#' deeptable  <- FLTable(connection, "FL_DEMO", "tblUSArrests", "ObsID","VarID","Num_Val")
#' resultList <- deepToWide(deeptable)
#' widetable <- resultList$table
#' analysisID <- resultList$AnalysisID
#' @export
setGeneric("deepToWide", function(object,whereconditions,
                                  mapTable,mapName,outWideTableDatabase,outWideTableName) {
    standardGeneric("deepToWide")
})
setMethod("deepToWide",
          signature(object = "FLTable",
                    whereconditions="character",
                    mapTable="character",
                    mapName="character",
                    outWideTableDatabase="character",
                    outWideTableName="character"),
          function(object,
                  whereconditions,
                  mapTable,
                  mapName,
                  outWideTableDatabase,
                  outWideTableName)
          {
            if(!object@isDeep) return(list(table=object))
            connection <- getConnection(object)
            
            if(outWideTableDatabase=="")
            outWideTableDatabase <- getOption("ResultDatabaseFL")
            if(mapTable=="" || mapTable=="NULL"){
              mapTable <- "NULL"
              mapName <- "NULL"}
            else if(mapName == "") mapName <- "NULL"

            whereconditions <- c(whereconditions,object@select@whereconditions)
            #whereconditions <- whereconditions[whereconditions!="" && whereconditions!="NULL"]
            object@select@whereconditions <- whereconditions
            
              #deeptable <- gen_deep_table_name(object@select@table_name)
              deeptable <- paste0(sample(letters[1:26],1),round(as.numeric(Sys.time())))
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
            #outWideTableName <- gen_wide_table_name(object@select@table_name)
            #outWideTableName <- paste0(sample(letters[1:26],1),sample(1:1000,1),sample(1:99,1))
            outWideTableName <- paste0(sample(letters[1:26],1),round(as.numeric(Sys.time())))

            sqlstr<-paste0("CALL FLDeepToWide('",object@select@database,".",object@select@table_name,"',
                                              'obs_id_colname',
                                              'var_id_colname',
                                              'cell_val_colname',",
                                              ifelse(mapTable=="NULL",mapTable,paste0("'",mapTable,"'")),",",
                                              ifelse(mapName=="NULL",mapName,paste0("'",mapName,"'")),",'",
                                              paste0(outWideTableDatabase,".",outWideTableName),
                                              "',MESSAGE);")
            message <- sqlQuery(connection,sqlstr)
            if(length(message)>1)
            {
              cat("output table already exists: Trying another random name for output table")
              outWideTableName <- paste0(sample(letters[1:26],1),sample(1:1000,1),sample(1:100,1))
              sqlstr<-paste0("CALL FLDeepToWide('",object@select@database,".",object@select@table_name,"',
                                              'obs_id_colname',
                                              'var_id_colname',
                                              'cell_val_colname',",
                                              ifelse(mapTable=="NULL",mapTable,paste0("'",mapTable,"'")),",",
                                              ifelse(mapName=="NULL",mapName,paste0("'",mapName,"'")),",'",
                                              paste0(outWideTableDatabase,".",outWideTableName),
                                              "',MESSAGE);")
              message <- sqlQuery(connection,sqlstr)
            }
            if(length(message)>1) stop(message)

            table <- FLTable(connection,
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
                    outWideTableName="missing"),
          function(object)
          deepToWide(object,
                    whereconditions="",
                    mapTable="",
                    mapName="",
                    outWideTableDatabase="",
                    outWideTableName=""))
