
# SELECT
#      ROW_NUMBER()OVER(ORDER BY flt.final_varid) obsid,
#      flt.column_name columnName,
#      flt.final_varid varid
#  FROM fzzlRegrDataPrepMap AS flt 
#  WHERE    (flt.final_varid is not null)   AND    (flt.Analysisid='A189032') 
#  ORDER BY ob

## 
#' @export
subset.FLTableDeep <- function(x,
                            subset,
                            select=NULL,
                            drop=FALSE,
                            ...){
    vtblname <- getTableNameSlot(x)
    x <- setAlias(x,"a")
    vmappings <- sqlQuery(getFLConnection(),constructSelect(x@mapSelect))
    colnames(vmappings) <- tolower(colnames(vmappings))
    vmapVector <- as.vector(vmappings[,"varid"])
    names(vmapVector) <- as.character(vmappings[,"columnname"])
    vVarIdColname <- getVarIdSQLName(x)
    vValueColname <- getValueSQLName(x)
    vObsIdColname <- getObsIdSQLName(x)
    vWhere <- c(subset,where(x))
    vWhereVarID <- c()
    for(i in names(vmapVector)){
        if(grepl(i,vWhere,ignore.case=TRUE)){
            vWhere <- gsub(i,paste0("b.",vValueColname),vWhere,ignore.case=TRUE)
            vWhereVarID <- c(vWhereVarID,vmapVector[i])
        }
    }

    vWhere <- c(vWhere,paste0(" b.",vVarIdColname," IN(",
                            paste0(vWhereVarID,collapse=","),") "),
                paste0(" b.",vObsIdColname," = a.",vObsIdColname))

    names(vtblname) <- "b"

    vexistsClause <- new("FLSimpleVector",
                        select=new("FLSelectFrom",
                                   table_name=vtblname,
                                   connectionName=getFLConnectionName(),
                                   variables=list(obsid=1),
                                   whereconditions=vWhere,
                                   order=""),
                        dimColumns = c("obsid"),
                        ##names=NULL,
                        Dimnames = list(NULL),
                        dims    = c(),
                        type       = "integer"
                        )
    cat(constructSelect(vexistsClause))
    where(x) <- paste0("EXISTS (",constructSelect(vexistsClause),")")

    nrows <- sqlQuery(getFLConnection(),
                        paste0("SELECT COUNT(DISTINCT ",vObsIdColname,") \n ",
                                " FROM (",constructSelect(x),") a "))[1,1]
    x@dims <- c(nrows,ncol(x))
    x
}
