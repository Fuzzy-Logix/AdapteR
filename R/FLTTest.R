#' @export
#FLFuncName <- c("FLtTest1s","FLtTest1s")
#FLStatistics <- c("T_STAT", "P_VALUE")
#vdf <- data.frame(FLFuncName = FLFuncName, FLStatistic = FLStatistics)
#flt <- as.FLTanle(vdf, tableName ="fzzlARHypTestStatsMap",drop = TRUE)
#'
#' 

 # adjust for user input 1 or 2 tail (done), output visibility(to-do),
 # mean to calculate in SQL query.


t.test.FLVector <- function(x, y= NULL, mu = 0, alternative= "two.sided"
                           , conf.level = .95,...)
{



    if(alternative == "two.sided")
        tail <- 2
    else
        tail <- 1

#    vcall <- paste(all.vars(sys.call())[1:2])

    

    sqlstr <- constructAggregateSQL(pFuncName = "FLtTest1s",
                                pFuncArgs = c("c.FLStatistic",
                                               "1.0","a.vectorValueColumn",
                                              tail),
                                pAddSelect = c(df = "COUNT(DISTINCT a.vectorValueColumn)"),
                                                                              
                                pFrom = c(a = constructSelect(x),
                                          c = "fzzlARHypTestStatsMap"),
                                pWhereConditions = c("c.FLFuncName = 'FLtTest1s'"),
                                pGroupBy = "c.FLStatistic")
                                             
                    
    result <- sqlQuery(connection, sqlstr)
   
      
    res <- list(data = NULL,
                statistic =c(t = as.numeric(result[1,1])),
                p.value = result[2,1],
                estimate =c("mean of x" = mean(x)),
                ymean = NULL,
                conf.int = 95,
                parameter = c(df = result[1,2]-1)
            #    data.name =  vcall
                )



    class(res) <- "htest"
    return(res)
    
}

    
    

    

