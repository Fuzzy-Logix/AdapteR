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
#'                       obs_id_colname="ObsID")
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
                      whereconditions=character(0),
                      connection=getFLConnection(),
                      # group_id=c(),
                      # fetchIDs=TRUE,
                      dims=list(0,0,0),
                      sparse=TRUE,
                      dimnames=list(list(NULL),list(NULL),list(NULL)),
                      ...
                      )
{
    # vgrp <- group_id
    vgrp <- dimnames[[1]][[1]]
    if("group_id" %in% names(list(...)))
      vgrp <- list(...)[["group_id"]]
    if(length(vgrp)>0)
        vgrp <- sort(vgrp)

    if(missing(dims)){
        dims <- llply(dimnames,function(x)sapply(x,length))
        ## Assumption: Varid's are continuous in deepTableMD
        if(dims[[3]])
        dims[[3]] <- sapply(dimnames[[3]],
                            function(x)length(min(x):max(x)))
    }
    ## If alias already exists, change it to flt.
    if(length(names(table))>0)
    oldalias <- names(table)[1]
    else oldalias <- ""
    var_id_colnames <- changeAlias(var_id_colnames,"flt",oldalias)
    obs_id_colname <- changeAlias(obs_id_colname,"flt",oldalias)
    cell_val_colname <- changeAlias(cell_val_colname,"flt",oldalias)
    group_id_colname <- changeAlias(group_id_colname,"flt",oldalias)

    if(length(vgrp)>0){
        whereconditions <- c(whereconditions, 
                            paste0(group_id_colname," IN (",
                                paste0(unlist(vgrp),collapse=","),")"))

        whereconditions <- changeAlias(whereconditions,
                                      "flt",
                                      c(getTablename(table),
                                        table,
                                        oldalias))
    }
    names(table) <- "flt"
    if(length(var_id_colnames) && length(cell_val_colname))
    {
        if(dims[[2]]) {
            ## If dimnames are not given and dims are given,
            ## then dimnames are assumed to be continuous

            rows <- dimnames[[2]]
            cols <- dimnames[[3]]

            if(!(length(cols)>0 && length(vgrp)>0)){
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
            }
            
        } else {
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

        attributes(cols)<-NULL
        names(cols)<-as.character(vgrp)
        names(rows)<-as.character(vgrp)
        dimnames <- list(list(vgrp),rows,cols)
        dims <- llply(dimnames,function(x)sapply(x,length))
        ## Assumption: Varid's are continuous in deepTableMD
        dims[[3]] <- sapply(dimnames[[3]],
                            function(x)length(min(x):max(x)))

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
                 Dimnames = dimnames,
                 dims = dims,
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
        rows <- dimnames[[2]]
        cols <- dimnames[[3]]

        if(!length(cols[[1]])>0){
            R <- sqlQuery(connection,
                      limitRowsSQL(paste0("select * from ",tableAndAlias(table)),1))
            cols <- names(R)
        }
        
        if(!is.list(cols))
          cols <- list(cols)


        # if(!is.null(list(...)[["ObsID"]])){
        #   rows <- list(...)[["ObsID"]]
        #   nrows <- llply(rows,length)
        # }
        # else if(fetchIDs) {
          # rows <- sort(sqlQuery(connection,
          #                   paste0("SELECT DISTINCT(",
          #                               obs_id_colname,") as varid
          #                           FROM ",tableAndAlias(table),
          #                           " ",constructWhere(whereconditions)))[[1]])
          # rows <- cleanNames(rows)
          # nrow <- length(rows)
        if(!dims[[2]]){
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
            # nrows <- llply(rows,length)

            if(length(vgrp)==0)
              vgrp <- unique(vdims[["grpid"]])
            # nMD <- length(vgrp)
        } else {
            # vdims <- sqlQuery(connection,
            #                 paste0("SELECT count(DISTINCT ",obs_id_colname,") as nrows,
            #                                count(DISTINCT ",group_id_colname,") as ngroups
            #                         FROM ",tableAndAlias(table),
            #                        " ",constructWhere(whereconditions)))
            # nrows <- vdims$nrows
            # nMD <- vdims$ngroups
            # vgrp <- NULL
            if(length(vgrp)==0){
                vdims <- sqlQuery(connection,
                              paste0("SELECT DISTINCT ",group_id_colname," AS grpid \n ",
                                     " FROM ",tableAndAlias(table),
                                     constructWhere(whereconditions)," \n ",
                                     " ORDER BY ",group_id_colname))
                vgrp <- unique(vdims[["grpid"]])
            }
        }
        # cols <- cleanNames(cols)
        
        # if(length(var_id_colnames)==0)
        #     var_id_colnames <- cols
        # if(length(setdiff(var_id_colnames,cols)))
        #     stop(paste0("columns do not exist: "))

        # ncols <- length(var_id_colnames)

        # mydimnames <- list(rows,var_id_colnames)
        if(length(cols)==1)
            cols <- rep(cols,length(vgrp))

        names(rows) <- as.character(vgrp)
        dimnames <- list(list(vgrp),rows,cols)
        dims <- llply(dimnames,function(x)sapply(x,length))

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
                Dimnames = dimnames,
                dims = dims,
                isDeep = FALSE,
                ...
            )
    }
}

#' @export
colnames.FLTableMD <- function(object){
    if(!isDeep(object)){
        return(dimnames(object)[[3]][[1]])
    }
    else{
        return(llply(dimnames(object)[[3]],
                    function(x) return(x[1]:x[2])))
    }
}

#' @export
rownames.FLTableMD <- function(object)
    return(dimnames(object)[[2]])

#' @export
print.FLTableMD <- function(object,head=TRUE,...){
    return(as.data.frame(x=object,head=head,...))
}
