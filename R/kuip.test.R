NULL
#' Kuiper Test used to test if two groups of data come from same distribution.
#'
#' 
#' Kuiper test is similar to two-sample Kolmogorov-Smirnov test. Compare to two-sample Kolmogorov-Smirnov test, Kuiper test is as sensitive in the tails as at the median and invariant under cyclic transformations of the independent variable.
#' 
#' @param vFLvector1 a FLVector of data values
#' @param vFLvector2 a FLVector of data values
#' @return A list with class "htest".
#' @return A list with class "htest" outputting the corresponding test Stat and P Values.
#' @examples
#' ## running on random FLvector
#' flx<-as.FLVector(rnorm(100))
#' fly<-as.FLVector(rnorm(100))
#' kuip.test(flx, fly)
#'
#' ## running on in-Database FLVector
#' fltbl1 <- FLTable(getTestTableName("tblKuiperTest"),
#'                    "obsid", 
#'                   whereconditions = "groupid=1
#'                    and datasetid=1")
#' fltbl2 <- FLTable(getTestTableName("tblKuiperTest"),
#'                    "obsid", 
#'                   whereconditions = "groupid=2
#'                    and datasetid=1")
#' flx <- fltbl1$num_val
#' fly <- fltbl2$num_val
#' kuip.test(flx, fly)
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
                         WhereClause = "NULL",
                         GroupBy = "NULL",
                         TableOutput = 1,
                         outputParameter = c(OutTable = 'a')
                        )
    colnames(ret) <- tolower(colnames(ret))
    if(!is.null(ret$resulttable)){
        sqlstr <- paste0("SELECT q.TEST_STAT AS TEST_STAT,
                                       q.P_VALUE AS P_Value
                               FROM ",ret[1,1]," AS q")
        res_1 <- sqlQuery(connection, sqlstr)
    }
    else res_1 <- ret
    colnames(res_1) <- tolower(colnames(res_1))

    if(!class(res_1$p_value) == "numeric")
    {  
        pval <- as.numeric(gsub("^[[:space:]]*[[:punct:]]*[[:space:]]*",
                            "",
                            res_1$p_value))
    }
    else
        pval <- res_1$p_value
    
    result <- list(statistics = c(Stat = res_1$test_stat),
                   p.value = pval,
                   method = "2-Sample Kuiper Test",
                   data.name = vcall                             
                   )
    class(result) <- "htest"
    dropView(vviewName)
    return(result)            
}
