## todo:
##  1. refactor results according to this schema, move generic into FLconstructSQL.R
##  2. document requirements of constructed sql query for FLTableFunctionQuery in results getter function

constructSelectFLHKmeansClusters <- function(AnalysisID, where="")
    paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
           "     ObsID AS vectorIndexColumn, \n ",
           "    DENSE_RANK()OVER(ORDER BY ClusterID) AS vectorValueColumn \n ",
           " FROM fzzlKMeansClusterID \n ",
           " WHERE AnalysisID = ",fquote(AnalysisID),
           " \n ",where,
           " ORDER BY vectorIndexColumn")
constructSelectFLHKmeansSize <- function(AnalysisID, where="")
    paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                            "     DENSE_RANK()OVER(ORDER BY ClusterID) AS vectorIndexColumn, \n ",
                            "    COUNT(ObsID) AS vectorValueColumn \n ",
                        "  FROM  fzzlKMeansClusterID \n ",
                        " WHERE AnalysisID = ",fquote(AnalysisID)," \n ",   
                          where,
                          " GROUP BY ClusterID ")
constructSelectFLHKmeansLevels <- function(AnalysisID, where="")
    paste0(" SELECT COUNT(DISTINCT ClusterID) FROM fzzlKMeansClusterID \n ",
            " WHERE AnalysisID=",fquote(AnalysisID)," \n ",
            where)

setGeneric("constructSelectResult", 
    function(object, result, deeptable, ...){
    standardGeneric("constructSelectResult")
})
setMethod("constructSelectResult", 
    signature(object="FLHKMeans",
            result="character",
            deeptable="missing"),
          function(object, result, ...){
    constructSelectResult(object,result,
                        deeptable=object@deeptable,...)
})
setMethod("constructSelectResult", 
            signature(object="FLHKMeans",
                result="character",
                deeptable="FLTableDeep.Hadoop"),
            function(object,
                    result=c("centers","withinss",
                            "fitted","size","levels",
                            "clusters","totss"),
                    deeptable,...){
    result <- match.arg(result)
    AnalysisID <- object@AnalysisID
    deeptablename <- getTableNameSlot(deeptable)
    obs_id_colname <- getIndexSQLExpression(deeptable,1)
    var_id_colname <- getIndexSQLExpression(deeptable,2)
    cell_val_colname <- getIndexSQLExpression(deeptable,3)
    whereconditions <- deeptable@select@whereconditions
    if(result=="withinss"){
        return(paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                      " DENSE_RANK()OVER(ORDER BY a.ClusterID) AS vectorIndexColumn, \n ",
                      " CAST(sum(power((b.",cell_val_colname,
                      " - c.Centroid ),2)) AS FLOAT) AS vectorValueColumn \n ",
                      " FROM fzzlKMeansClusterID a, \n ",deeptablename," b , \n fzzlKMeansCentroid c \n ",
                      " WHERE c.AnalysisID = ",fquote(AnalysisID)," \n ",
                      " AND a.AnalysisID = ",fquote(AnalysisID)," \n ",
                      " AND b.",var_id_colname,"=c.DimID \n ",
                      " AND a.ClusterID = c.ClusterID \n ",
                      " AND a.ObsID = b.",obs_id_colname," \n ",
                      ifelse(length(whereconditions)>0, 
                            paste0(" AND ",whereconditions,collapse=" \n "),
                            ""), 
                      " GROUP BY a.ClusterID "))
    } else if(result=="clusters"){
        return(constructSelectFLHKmeansClusters(AnalysisID))
    }
    else if(result=="centers"){
        return(paste0("SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
                            "  DENSE_RANK()OVER(ORDER BY ClusterID) AS rowIdColumn, \n ",
                            "  DimID AS colIdColumn, \n ",
                            "   Centroid AS valueColumn \n ",
                        " FROM fzzlKMeansCentroid \n ",
                        " WHERE AnalysisID = ",fquote(AnalysisID)," \n "))
    }
    else if(result=="totss"){
        return(paste0("SELECT CAST(sum(power((b.",
                        cell_val_colname," - a.valavg),2)) AS FLOAT) \n ",
                        " FROM (SELECT ",var_id_colname,",FLMean(",cell_val_colname,") AS valavg \n ",
                            "  FROM ",deeptablename,
                              constructWhere(whereconditions)," \n ",
                              " GROUP BY ",var_id_colname,") AS a, \n ",
                             deeptablename," b \n ",
                        " WHERE a.",var_id_colname," = b.",var_id_colname,
                        ifelse(length(whereconditions)>0, 
                            paste0(" AND ",whereconditions,collapse=" \n "),
                            "")
                    ))
    }
    else if(result=="size")
        return(constructSelectFLHKmeansSize(AnalysisID))
    else if(result=="fitted"){
        return(paste0("SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
                        " b.ObsID AS rowIdColumn, \n ",
                        " a.DimID AS colIdColumn, \n ",
                        " a.Centroid AS valueColumn \n ",
                    " FROM fzzlKMeansCentroid a, \n ",
                            "fzzlkmeansclusterid b \n ",
                    " WHERE a.AnalysisID = ",fquote(AnalysisID)," \n ",
                    " AND a.AnalysisID = b.AnalysisID \n ",
                    " AND a.ClusterID = b.ClusterID "))
    }
    else if(result=="levels")
        return(constructSelectFLHKmeansLevels(AnalysisID))
})

setMethod("constructSelectResult", 
            signature(object="FLHKMeans",
                    result="character",
                    deeptable="FLTableDeep.TD"),
          function(object,
                    result=c("centers","withinss",
                            "fitted","size","levels",
                            "clusters","totss"),
                    deeptable,...){
    result <- match.arg(result)
    AnalysisID <- object@AnalysisID
    deeptablename <- getTableNameSlot(deeptable)
    obs_id_colname <- getIndexSQLExpression(deeptable,1)
    var_id_colname <- getIndexSQLExpression(deeptable,2)
    cell_val_colname <- getIndexSQLExpression(deeptable,3)
    whereconditions <- deeptable@select@whereconditions
    if(result=="withinss"){
        ## flag3Check(connection)
        return(paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                                " DENSE_RANK()OVER(ORDER BY a.ClusterID) AS vectorIndexColumn, \n ",
                                " CAST(sum(power((b.",cell_val_colname,
                                    " - c.Centroid ),2)) AS NUMBER) AS vectorValueColumn \n ",
                        " FROM fzzlKMeansClusterID a, \n ",deeptablename," b, \n fzzlKMeansDendrogram c \n ",
                        " WHERE c.AnalysisID = ",fquote(AnalysisID)," \n ",
                        " AND a.AnalysisID = ",fquote(AnalysisID)," \n ",
                        " AND b.",var_id_colname,"=c.VarID \n ",
                        " AND a.ClusterID = c.ClusterID \n ",
                        " AND a.ObsID = b.",obs_id_colname," \n ",
                        " AND a.HypothesisID = ",object@nstart," \n ",
                        " AND c.HypothesisID = ",object@nstart," \n ",
                        ifelse(length(whereconditions)>0, 
                                paste0(" AND ",whereconditions,collapse=" \n "),
                                ""), 
                        " GROUP BY a.ClusterID "))

        # return(paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
        #               " DENSE_RANK()OVER(ORDER BY a.ClusterID) AS vectorIndexColumn, \n ",
        #               " CAST(sum(power((b.",cell_val_colname,
        #               " - c.Centroid ),2)) AS NUMBER) AS vectorValueColumn \n ",
        #               " FROM fzzlKMeansClusterID a, \n ",deeptablename," b , \n fzzlKMeansDendrogram c \n ",
        #               " WHERE c.AnalysisID = ",fquote(AnalysisID)," \n ",
        #               " AND a.AnalysisID = ",fquote(AnalysisID)," \n ",
        #               " AND b.",var_id_colname,"=c.DimID \n ",
        #               " AND a.ClusterID = c.ClusterID \n ",
        #               " AND a.ObsID = b.",obs_id_colname," \n ",
        #               ifelse(length(whereconditions)>0, 
        #                     paste0(" AND ",whereconditions,collapse=" \n "),
        #                     ""), 
        #               " GROUP BY a.ClusterID "))
    } else if(result=="clusters"){
        return(constructSelectFLHKmeansClusters(AnalysisID,
                                                paste0(" AND HypothesisID = ",
                                                        object@nstart," \n ")))
    }
    else if(result=="centers"){
        return(paste0("SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
                            "  DENSE_RANK()OVER(ORDER BY ClusterID) AS rowIdColumn, \n ",
                            "  VarID AS colIdColumn, \n ",
                            "   Centroid AS valueColumn \n ",
                        " FROM fzzlKMeansDendrogram \n ",
                        " WHERE AnalysisID = ",fquote(AnalysisID)," \n ",
                        " AND HypothesisID = ",object@nstart," \n ",
                        " AND Level = ",object@levels))
    }
    else if(result=="totss"){
        return(paste0("SELECT CAST(sum(power((b.",
                        cell_val_colname," - a.valavg),2)) AS NUMBER) \n ",
                        " FROM (SELECT ",var_id_colname,",FLMean(",cell_val_colname,") AS valavg \n ",
                            "  FROM ",deeptablename,
                              constructWhere(whereconditions)," \n ",
                              " GROUP BY ",var_id_colname,") AS a, \n ",
                             deeptablename," b \n ",
                        " WHERE a.",var_id_colname," = b.",var_id_colname,
                        ifelse(length(whereconditions)>0, 
                                paste0(" AND ",whereconditions,collapse=" \n "),
                                "")
                        ))
    }
    else if(result=="size")
        return(constructSelectFLHKmeansSize(AnalysisID,
                                            paste0(" AND HypothesisID = ",
                                                    object@nstart," \n ")))
    else if(result=="fitted"){
        return(paste0("SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
                        " b.ObsID AS rowIdColumn, \n ",
                        " a.VarID AS colIdColumn, \n ",
                        " a.Centroid AS valueColumn \n ",
                    " FROM fzzlKMeansDendrogram a, \n ",
                            "fzzlkmeansclusterid b \n ",
                    " WHERE a.AnalysisID = ",fquote(AnalysisID)," \n ",
                    " AND a.HypothesisID = ",object@nstart," \n ",
                    " AND a.Level = ",object@levels," \n ",
                    " AND a.HypothesisID = b.HypothesisID \n ",
                    " AND a.AnalysisID = b.AnalysisID \n ",
                    " AND a.ClusterID = b.ClusterID "))
    }
    else if(result=="levels")
        return(constructSelectFLHKmeansLevels(AnalysisID,
                                            paste0(" AND HypothesisID = ",object@nstart)))
})

setMethod("constructSelectResult", 
            signature(object="FLHKMeans",
                    result="character",
                    deeptable="FLTableDeep.TDAster"),
          function(object,
                    result=c("centers","withinss",
                            "fitted","size","levels",
                            "clusters","totss"),
                    deeptable,...){
    result <- match.arg(result)
    AnalysisID <- object@AnalysisID
    deeptablename <- getTableNameSlot(deeptable)
    obs_id_colname <- getIndexSQLExpression(deeptable,1)
    var_id_colname <- getIndexSQLExpression(deeptable,2)
    cell_val_colname <- getIndexSQLExpression(deeptable,3)
    whereconditions <- deeptable@select@whereconditions

    ##@phani: Needed to cast to VARCHAR to match cluster IDs in Aster system tables !!
    if(result=="withinss"){
        ## flag3Check(connection)
        return(paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
                                " DENSE_RANK()OVER(ORDER BY a.ClusterID) AS vectorIndexColumn, \n ",
                                " CAST(sum(power((b.",cell_val_colname,
                                    " - c.Centroid ),2)) AS FLOAT) AS vectorValueColumn \n ",
                        " FROM fzzlKMeansClusterID a, \n ",deeptablename," b, \n fzzlKMeansDendrogram c \n ",
                        " WHERE c.AnalysisID = ",fquote(AnalysisID)," \n ",
                        " AND a.AnalysisID = ",fquote(AnalysisID)," \n ",
                        " AND b.",var_id_colname,"=c.VarID \n ",
                        " AND CAST(a.ClusterID AS VARCHAR) = CAST(c.ClusterID AS VARCHAR) \n ",
                        " AND a.ObsID = b.",obs_id_colname," \n ",
                        " AND a.HypothesisID = ",object@nstart," \n ",
                        " AND c.HypothesisID = ",object@nstart," \n ",
                        ifelse(length(whereconditions)>0, 
                                paste0(" AND ",whereconditions,collapse=" \n "),
                                ""), 
                        " GROUP BY a.ClusterID "))

        # return(paste0("SELECT '%insertIDhere%' AS vectorIdColumn, \n ",
        #               " DENSE_RANK()OVER(ORDER BY a.ClusterID) AS vectorIndexColumn, \n ",
        #               " CAST(sum(power((b.",cell_val_colname,
        #               " - c.Centroid ),2)) AS NUMBER) AS vectorValueColumn \n ",
        #               " FROM fzzlKMeansClusterID a, \n ",deeptablename," b , \n fzzlKMeansDendrogram c \n ",
        #               " WHERE c.AnalysisID = ",fquote(AnalysisID)," \n ",
        #               " AND a.AnalysisID = ",fquote(AnalysisID)," \n ",
        #               " AND b.",var_id_colname,"=c.DimID \n ",
        #               " AND a.ClusterID = c.ClusterID \n ",
        #               " AND a.ObsID = b.",obs_id_colname," \n ",
        #               ifelse(length(whereconditions)>0, 
        #                     paste0(" AND ",whereconditions,collapse=" \n "),
        #                     ""), 
        #               " GROUP BY a.ClusterID "))
    } else if(result=="clusters"){
        return(constructSelectFLHKmeansClusters(AnalysisID,
                                                paste0(" AND HypothesisID = ",
                                                        object@nstart," \n ")))
    }
    else if(result=="centers"){
        return(paste0("SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
                            "  DENSE_RANK()OVER(ORDER BY ClusterID) AS rowIdColumn, \n ",
                            "  VarID AS colIdColumn, \n ",
                            "   Centroid AS valueColumn \n ",
                        " FROM fzzlKMeansDendrogram \n ",
                        " WHERE AnalysisID = ",fquote(AnalysisID)," \n ",
                        " AND HypothesisID = ",object@nstart," \n ",
                        " AND Level = ",object@levels))
    }
    else if(result=="totss"){
        return(paste0("SELECT CAST(sum(power((b.",
                        cell_val_colname," - a.valavg),2)) AS FLOAT) \n ",
                        " FROM (SELECT ",var_id_colname,",FLMean(",cell_val_colname,") AS valavg \n ",
                            "  FROM ",deeptablename,
                              constructWhere(whereconditions)," \n ",
                              " GROUP BY ",var_id_colname,") AS a, \n ",
                             deeptablename," b \n ",
                        " WHERE a.",var_id_colname," = b.",var_id_colname,
                        ifelse(length(whereconditions)>0, 
                                paste0(" AND ",whereconditions,collapse=" \n "),
                                "")
                        ))
    }
    else if(result=="size")
        return(constructSelectFLHKmeansSize(AnalysisID,
                                            paste0(" AND HypothesisID = ",
                                                    object@nstart," \n ")))
    else if(result=="fitted"){
        return(paste0("SELECT '%insertIDhere%' AS MATRIX_ID, \n ",
                        " b.ObsID AS rowIdColumn, \n ",
                        " a.VarID AS colIdColumn, \n ",
                        " a.Centroid AS valueColumn \n ",
                    " FROM fzzlKMeansDendrogram a, \n ",
                            "fzzlkmeansclusterid b \n ",
                    " WHERE a.AnalysisID = ",fquote(AnalysisID)," \n ",
                    " AND a.HypothesisID = ",object@nstart," \n ",
                    " AND a.Level = ",object@levels," \n ",
                    " AND a.HypothesisID = b.HypothesisID \n ",
                    " AND a.AnalysisID = b.AnalysisID \n ",
                    " AND CAST(a.ClusterID AS VARCHAR) = CAST(b.ClusterID AS VARCHAR)"))
    }
    else if(result=="levels")
        return(constructSelectFLHKmeansLevels(AnalysisID,
                                            paste0(" AND HypothesisID = ",object@nstart)))
})
