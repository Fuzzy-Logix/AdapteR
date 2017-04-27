
#' Performs the Wald-Wolfowitz runs test of randomness for continuous data.
#' @param x a FLVector containing the observations.
#' @param threshold the cut-point to transform the data into a dichotomous vector.
#' @section Constraints: alternative, plot, pvalue isnt't currently supported for FL objects.
#' @return A list with class "htest".
#' @examples
#' WaldWolfowitz test 1s:
#' data(sweetpotato)
#' yield <- as.FLVector(sweetpotato$yield)
#' WaldWolftest1s(yield)
#' 
#' Data is transformed into a dichotomous vector according as each values is above or below a given threshold. Values equal to the level are removed from the sample.
#' The default threshold value used in applications is the sample median which give us the special case of this test with n1 = n2, the runs test above and below the median.
#' @export
WaldWolftest1s  <- function(vFLvector,threshold = median(vFLvector)) {
    if(!is.FLVector(vFLvector))
        stop("Only take FLVector")
    else{
        vviewName <- gen_view_name("ww1sTest")
        sqlstr <- paste0("SELECT t.vectorValueColumn AS Num_Val,
                                        1 AS DatasetID,
                                        1 AS GroupID,
                                        t.vectorindexcolumn AS ObsID,
                                CASE WHEN t.vectorValueColumn > ",threshold,"
                                        THEN 1
                                     WHEN t.vectorValueColumn < ",threshold,"
                                        THEN -1
                                     ELSE 0
                                END AS Sign
                                FROM (",constructSelect(vFLvector),") AS t")
        l <- createView(vviewName,sqlstr)
        vcall <- as.list(sys.call())[2]
        ret <- sqlStoredProc(connection,
                             "FLWWTest1S",
                             TableName = vviewName,
                             ObsIDColName = "ObsID",
                             Sign= "Sign",
                             WhereClause = "NULL" ,
                             GroupBy = "DatasetID",
                             TableOutput = 1,
                             outputParameter = c(ResultTable = 'a')
                             )
        colnames(ret) <- tolower(colnames(ret))

        if(!is.null(ret$resulttable)){
            sqlstr <- paste0("SELECT q.Z AS z, q.P_Value AS p_value  FROM ",
                         ret$resulttable," AS q")
            res_1 <- sqlQuery(connection , sqlstr)
        }
        else res_1 <- ret

        
        result <- list(statistic = c(Z = res_1$z),
                       p.value = res_1$p_value,
                       data.name = as.character(vcall),
                       method = "Wald Wolfowitz test"
                       )
        class(result) <- "htest"
        dropView(vviewName)
        return(result)      
    }
}





#' @export
WaldWolftest2s <- function(vFLvector, vFLvector2)
{
    vviewName <- gen_view_name("wwt2s")
    vcall <- as.list(sys.call())
    dname <- paste0(vcall[2]," and ",vcall[3])

    t <- constructUnionSQL(pFrom = c(a = constructSelect(vFLvector),
                                     b = constructSelect(vFLvector2)),
                           pSelect = list(a = c(DatasetID=1,
                                                GroupID = 1,
                                                Num_Val = "a.vectorValueColumn"),
                                          b = c(DatasetID=1,
                                                GroupID = 2,
                                                Num_Val = "b.vectorValueColumn"))
                           )
    view <- createView(vviewName, t)
    ret <- sqlStoredProc(connection,
                         "FLWWtest2s",
                         TableName = vviewName,
                         ValueColName = "Num_Val",
                         SampleIDColName = "GroupID",
                         WhereClause = "NULL",
                         GroupBy = "DatasetID",
                         TableOutput = 1,
                         outputParameter = c(ResultTable = 'a')
                         )
    colnames(ret) <- tolower(colnames(ret))
    if(!is.null(ret$resulttable)){
        sqlstr <- paste0("SELECT r.Z AS z,
                             r.P_VALUE AS p_value
                     FROM ",ret$resulttable," AS r")
        res_1 <- sqlQuery(connection, sqlstr)
    }
    else res_1 <- ret
    
    result <- list(statistic = c(statistic = res_1$z),
                   p.value = res_1$p_value,
                   alternative = "two-sided",
                   method = "Two-sample Wald-Wolfowitz test",
                   data.name = dname                             
                   )
    class(result) <- "htest"
    dropView(vviewName)
    return(result)
}
