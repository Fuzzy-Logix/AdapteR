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
                      group_id=c(),
                      fetchIDs=TRUE,
                      ...
                      )
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
        if(!fetchIDs) {
            warning("todo: implement fetcching of IDs in MD deep case")
            ## gk @ phani:  this needs serious optimization!
            ##browser()
            rows <- NULL
            cols <- NULL
            vgrp <- NULL
            vdims <- sqlQuery(connection,
                              paste0("SELECT ",group_id_colname," AS grpid, \n ",
                                     "MIN(",var_id_colnames,") AS minid, \n ",
                                     "Max(",var_id_colnames,") AS maxid \n ",
                                     " FROM ",tableAndAlias(table),
                                     constructWhere(whereconditions)," \n ",
                                     " GROUP BY ",group_id_colname,
                                     " ORDER BY ",group_id_colname))
            if(length(vgrp)==0)
                vgrp <- unique(vdims[["grpid"]])
            cols<-dlply(vdims,"grpid",
                        function(x){
                c(vdims[["minid"]],vdims[["maxid"]])
            })
            names(cols)<-as.character(vgrp)
        } else {
                                        #browser()
            ## Wrong Assumption
            ## ObsIDs can be non-continuous
            vdims <- sqlQuery(connection,
                              paste0("SELECT ",group_id_colname," AS grpid, \n ",
                                     "MIN(",var_id_colnames,") AS minid, \n ",
                                     "Max(",var_id_colnames,") AS maxid \n ",
                                     " FROM ",tableAndAlias(table),
                                     constructWhere(whereconditions)," \n ",
                                     " GROUP BY ",group_id_colname,
                                     " ORDER BY ",group_id_colname))

            vdims1 <- sqlQuery(connection,
                               paste0("SELECT DISTINCT ",group_id_colname," AS grpid, \n ",
                                      obs_id_colname," AS obsid \n ",
                                      " FROM ",tableAndAlias(table),
                                      constructWhere(whereconditions)," \n ",
                                      " ORDER BY ",group_id_colname,",",obs_id_colname))
                                        # if(length(unique(vdims[["MinID"]]))!=1 
                                        #     ||length(unique(vdims[["MaxID"]]))!=1)
                                        #     stop("currently each set should have same columns \n ")

            if(length(vgrp)==0)
                vgrp <- unique(vdims[["grpid"]])

            cols<-dlply(vdims,"grpid",
                        function(x){
                c(x[["minid"]],x[["maxid"]])
            })
            attributes(cols)<-NULL
            names(cols)<-as.character(vgrp)

            rows<-dlply(vdims1,"grpid",
                        function(x){
                if(is.factor(x[["obsid"]]))
                    return(as.character(x[["obsid"]]))
                else return(x[["obsid"]])
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
        }
        select <- new("FLSelectFrom",
                      connectionName = attr(connection,"name"), 
                      table_name = table, 
                      variables = list(
                          group_id_colname= group_id_colname,
                          obs_id_colname = obs_id_colname,
                          var_id_colname = var_id_colnames,
                          cell_val_colname = cell_val_colname),
                      whereconditions=whereconditions,
                      order = "")
        T <- newFLTableMD( 
                 select = select,
                 Dimnames = list(rows,cols,vgrp),
                 dims = as.integer(c(length(rows),length(cols),length(vgrp))),
                 isDeep = TRUE,
                 dimColumns=c("group_id_colname","obs_id_colname",
                              "var_id_colname","cell_val_colname"),
            ...
                )
        # new("FLTableMD",
        #     select = select,
        #     Dimnames = list(rows,cols,vgrp),
        #     isDeep = TRUE)
    }
    else
    {
        #browser()
        R <- sqlQuery(connection,
                      limitRowsSQL(paste0("select * from ",tableAndAlias(table)),1))
        cols <- names(R)

        if(!is.null(list(...)[["ObsID"]])){
          rows <- list(...)[["ObsID"]]
          nrow <- length(rows)
        }
        else if(fetchIDs) {
          rows <- sort(sqlQuery(connection,
                            paste0("SELECT DISTINCT(",
                                        obs_id_colname,") as varid
                                    FROM ",tableAndAlias(table),
                                    " ",constructWhere(whereconditions)))[[1]])
          rows <- cleanNames(rows)
          nrow <- length(rows)
          vdims <- sqlQuery(connection,
                            paste0("SELECT DISTINCT ",group_id_colname," AS grpid, \n ",
                                   obs_id_colname," AS obsid \n ",
                                   " FROM ",tableAndAlias(table),
                                   constructWhere(whereconditions)," \n ",
                                   " ORDER BY ",group_id_colname,",",obs_id_colname))
          rows<-dlply(vdims,"grpid",
                      function(x){
              if(is.factor(x[["obsid"]]))
                  return(as.character(x[["obsid"]]))
              else return(x[["obsid"]])
          })
          attributes(rows)<-NULL
          if(length(vgrp)==0)
              vgrp <- unique(vdims[["grpid"]])
            nMD <- length(vgrp)
        } else {
            rows <- NULL
            vdims <- sqlQuery(connection,
                            paste0("SELECT count(DISTINCT ",obs_id_colname,") as nrows,
                                           count(DISTINCT ",group_id_colname,") as ngroups
                                    FROM ",tableAndAlias(table),
                                   " ",constructWhere(whereconditions)))
            nrow <- vdims$nrows
            nMD <- vdims$ngroups
            vgrp <- NULL
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
                          group_id_colname=group_id_colname,
                          obs_id_colname = obs_id_colname,
                          cell_val_colname = cell_val_colname),
                      whereconditions=whereconditions,
                      order = "")

        T <- newFLTableMD( 
                 select = select,
                 Dimnames = list(rows,cols,vgrp),
                 dims = as.integer(c(nrow,ncol,nMD)),
            isDeep = FALSE,
            ...
            ##type=type ## todo
        )
    }
}

