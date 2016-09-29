#' @include FLMatrix.R
NULL


#' Constructor function for FLTableMD.
#'
#' \code{FLTableMD} constructs an object of class \code{FLTableMD}.
#'
#' \code{FLTableMD} refers to an in-database table with multiple data sets.
#' This object is commonly used as input for some data mining functions
#' which operate parallelly on all the datasets
#' @param database name of the database
#' @param table name of the table
#' @param group_id_colname column name identifying the datasets
#' @param obs_id_colname column name set as primary key
#' @param var_id_colname column name where variable id's are stored if \code{FLTableMD} is deep
#' @param cell_val_colname column name where cell values are stored if \code{FLTableMD} is deep
#' @param group_id vector of dataset IDs'to be considered.
#' Default is all those contained in the table
#' @return \code{FLTableMD} returns an object of class FLTableMD mapped to a table
#' in database
#' @examples
#' widetableMD <- FLTableMD(table="tblAutoMPGMD",
#'                       group_id_colname="GroupID",
#'                       obs_id_colname="ObsID",
#'                       group_id = c(2,4))
#' deeptableMD <- FLTableMD(table="LinRegrMultiMD",
#'                       group_id_colname="DatasetID",
#'                       obs_id_colname="ObsID",
#'                       var_id_colname="VarID",
#'                       cell_val_colname="Num_Val")
#' head(widetableMD)
#' head(deeptableMD)
#' @export
FLTableMD <- function(table,
                      group_id_colname,
                      obs_id_colname,
                      var_id_colnames=character(0), 
                      cell_val_colname=character(0),
                      connection=getFLConnection(),
                      group_id=c())
{
    whereconditions <- ""
    vgrp <- group_id
    if(length(vgrp)>0)
        vgrp <- sort(vgrp)
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
                                      c(getTablename(table),
                                        table,
                                        oldalias))
    }
    names(table) <- "flt"
    if(length(var_id_colnames) && length(cell_val_colname))
    {
        #browser()
        ## Wrong Assumption
        ## ObsIDs can be non-continuous
        vdims <- sqlQuery(connection,
                         paste0("SELECT ",group_id_colname," AS grpID, \n ",
                                        "MIN(",var_id_colnames,") AS MinID, \n ",
                                        "Max(",var_id_colnames,") AS MaxID \n ",
                                " FROM ",tableAndAlias(table),
                                constructWhere(whereconditions)," \n ",
                                " GROUP BY ",group_id_colname,
                                " ORDER BY ",group_id_colname))

        vdims1 <- sqlQuery(connection,
                         paste0("SELECT DISTINCT ",group_id_colname," AS grpID, \n ",
                                        obs_id_colname," AS ObsID \n ",
                                " FROM ",tableAndAlias(table),
                                constructWhere(whereconditions)," \n ",
                                " ORDER BY ",group_id_colname,",",obs_id_colname))
        # if(length(unique(vdims[["MinID"]]))!=1 
        #     ||length(unique(vdims[["MaxID"]]))!=1)
        #     stop("currently each set should have same columns \n ")

        if(length(vgrp)==0)
            vgrp <- unique(vdims[["grpID"]])

        cols<-dlply(vdims,"grpID",
                    function(x){
                        c(x[["MinID"]],x[["MaxID"]])
                    })
        attributes(cols)<-NULL
        names(cols)<-as.character(vgrp)

        rows<-dlply(vdims1,"grpID",
                    function(x){
                        if(is.factor(x[["ObsID"]]))
                            return(as.character(x[["ObsID"]]))
                        else return(x[["ObsID"]])
                    })
        attributes(rows)<-NULL
        # cols <- gsub("^ +| +$","",cols)
        # rows <- gsub("^ +| +$","",rows)

        ##change factors to strings
        # vdimnames <- lapply(list(rows,cols),
        #                           function(x){
        #                               if(is.factor(x))
        #                               as.numeric(x)
        #                               else x
        #                           })
        # rows <- vdimnames[[1]]
        # cols <- vdimnames[[2]]

        # if(is.character(rows) || is.character(cols))
        #     stop("obsIDs and varIDs cannot be characters \n ")

        select <- new("FLSelectFrom",
                      connection = connection, 
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
            Dimnames = list(rows,cols,vgrp),
            isDeep = TRUE)
    }
    else
    {
        #browser()
        R <- sqlQuery(connection,
                      limitRowsSQL(paste0("select * from ",tableAndAlias(table)),1))
        cols <- names(R)
        vdims <- sqlQuery(connection,
                         paste0("SELECT DISTINCT ",group_id_colname," AS grpID, \n ",
                                        obs_id_colname," AS ObsID \n ",
                                " FROM ",tableAndAlias(table),
                                constructWhere(whereconditions)," \n ",
                                " ORDER BY ",group_id_colname,",",obs_id_colname))
        rows<-dlply(vdims,"grpID",
                    function(x){
                        if(is.factor(x[["ObsID"]]))
                            return(as.character(x[["ObsID"]]))
                        else return(x[["ObsID"]])
                    })
        attributes(rows)<-NULL
        # ##change factors to strings
        # vdimnames <- lapply(list(rows,cols),
        #                           function(x){
        #                               if(is.factor(x))
        #                               as.numeric(x)
        #                               else x
        #                           })
        # rows <- vdimnames[[1]]
        # cols <- vdimnames[[2]]
        if(length(vgrp)==0)
            vgrp <- unique(vdims[["grpID"]])
        # if(is.character(rows))
        #     stop("obsIDs cannot be characters \n ")

        cols <- setdiff(cols,changeAlias(obs_id_colname,"","flt"))

        select <- new("FLSelectFrom",
                      connection = connection, 
                      table_name = table, 
                      variables = list(
                          group_id_colname=group_id_colname,
                          obs_id_colname = obs_id_colname,
                                        #var_id_colname = var_id_colnames,
                          cell_val_colname = cell_val_colname),
                      whereconditions=whereconditions,
                      order = "")

        T <- new("FLTableMD", 
                 select = select,
                 Dimnames = list(rows,cols,vgrp),
                 isDeep = FALSE)
    }
}

