WaldWolftest1s  <- function(vFLvector,threshold = median(vFLvector)) {
    if(!is.FLVector(vFLvector))
        stop("Only take FLVector")
    else{
        vviewName <- gen_view_name("ww1sTest")
        sqlstr <- paste0("SELECT t.vectorValueColumn AS Num_Val,
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
                             WhereClause = NULL ,
                             GroupBy = NULL,
                             TableOutput = 1,
                             outputParameter = c(ResultTable = 'a')
                             )
        sqlstr <- paste0("SELECT q.Z AS Z, q.P_Value AS P  FROM ",
                         ret$ResultTable," AS q")
        res_1 <- sqlQuery(connection , sqlstr)
        result <- list(statistics = c(Z = res_1$Z),
                       p.value = res_1$P,
                       data.name = as.character(vcall),
                       method = "Wald Wolfowitz test"
                       )
        class(result) <- "htest"
        return(result)      
    }
}

WaldWolftest2s <- function(vFLvector, vFLvector2)
{
    vviewName <- gen_view_name("wwt2s")
    vcall <- as.list(sys.call())
    dname <- paste0(vcall[2]," and ",vcall[3])

    t <- constructUnionSQL(pFrom = c(a = constructSelect(vFLvector),
                                     b = constructSelect(vFLvector2)),
                           pSelect = list(a = c(GroupID = 1,
                                                Num_Val = "a.vectorValueColumn"),
                                          b = c(GroupID = 2,
                                                Num_Val = "b.vectorValueColumn"))
                           )
    view <- createView(vviewName, t)
    ret <- sqlStoredProc(connection,
                         "FLWWtest2s",
                         TableName = vviewName,
                         ValueColName = "Num_Val",
                         SampleIDColName = "GroupID",
                         WhereClause = NULL,
                         GroupBy = NULL,
                         TableOutput = 1,
                         outputParameter = c(ResultTable = 'a')
                         )
    sqlstr <- paste0("SELECT r.Z AS Z,
                             r.P_VALUE AS P_Value
                     FROM ",ret$ResultTable," AS r")
    res_1 <- sqlQuery(connection, sqlstr)
    result <- list(statistics = c(Z = res_1$Z),
                   p.value = res_1$P_Value,
                   alternative = "two-sided",
                   method = "Two-sample Wald-Wolfowitz test",
                   data.name = dname                             
                   )
    class(result) <- "htest"
    return(result)
}
