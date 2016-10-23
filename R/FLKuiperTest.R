NULL
#' Kuiper Test
#'
#' 
#' Kuiper test is used to test if two groups of data come from same distribution.
#' Kuiper test is similar to two-sample Kolmogorov-Smirnov test. Compare to two-sample Kolmogorov-Smirnov test, Kuiper test is as sensitive in the tails as at the median and invariant under cyclic transformations of the independent variable.
#' 
#' @param vFLvector1 a FLVector of data values
#' @param vFLvector2 a FLVector of data values
#' @return A list with class "htest".
#' @export
kuip.test <- function(vFLvector1, vFLvector2)          {
    dname <- as.list(sys.call())
        vcall <- paste0(dname[2], " and ", dname[3])
    vviewName <- gen_view_name("kuiptest")
    t <- constructUnionSQL(pFrom =c(a = constructSelect(vFLvector1),
                                    b = constructSelect(vFLvector2)),
                           pSelect = list(a = c(GroupID = 1,
                                                Num_Val = "a.vectorValueColumn"),
                                          b = c(GroupID = 2,
                                                Num_Val = "b.vectorValueColumn"))
                           )
    view <- createView(vviewName, t)

    ret <- sqlStoredProc(connection,
                         "FLKuiperTest",
                         TableName = vviewName,
                         ValueColName = "Num_Val",
                         SampleIDColName = "GroupID",
                         WhereClause = NULL,
                         GroupBy = NULL,
                         TableOutput = 1,
                         outputParameter = c(OutTable = 'a')
                         )
    sqlstr <- paste0("SELECT q.TEST_STAT AS TStat,
                                       q.P_VALUE AS P_Value
                               FROM ",ret$OutTable," AS q")
    res_1 <- sqlQuery(connection, sqlstr)
    if(!class(res_1$P_Value) == "numeric")
    {                    pval <- as.numeric(gsub("^[[:space:]]*[[:punct:]]*[[:space:]]*","",res_1$P_Value))
    }
    else
        pval <- res_1$P_Value
    
    result <- list(statistics = c(Stat = res_1$TStat),
                   p.value = pval,
                   method = "2-Sample Kuiper Test",
                   data.name = vcall                             
                   )
    class(result) <- "htest"
    dropView(vviewName)
    return(result)            
}
