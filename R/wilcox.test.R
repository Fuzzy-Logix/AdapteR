wilcox.test.FLVector <- function(x,y = NULL,paired = TRUE, mu = 0,...)
{
    if(!is.FLVector(x) || !is.FLVector(y))
        stop("Must be FLVector")
    else {
        if(paired) {
            vviewName <- gen_view_name("wsrTest")
            if(length(x)> length(y))
                res <- sqlSendUpdate(connection, createHypoView(y,x,vviewName))
            else
                res <- sqlSendUpdate(connection, createHypoView(x,y,vviewName))
            ##
            vcall <- as.list(sys.call())
            dname = paste0(vcall[2]," and ",vcall[3])
            ##  Using Stored Proc Query.
            ret <- sqlStoredProc(connection,
                                 "FLWSRTest",
                                 TableName = vviewName,
                                 Val1ColName = "Num_Val1",
                                 Val2ColName = "Num_Val2",
                                 WhereClause = NULL ,
                                 GroupBy = NULL,
                                 TableOutput = 1,
                                 outputParameter = c(ResultTable = 'a'))
            ##
            sqlstr <- paste0( "SELECT q.W_STAT AS W,
                                      q.P_VALUE AS p,
                                      q.W_STAT_Neg AS W_Neg,
                                      q.W_STAT_Posi AS W_Pos
                           FROM ",ret$ResultTable," AS q")
            result <-  sqlQuery(connection,sqlstr)
            if(result$W_Pos > result$W_Neg) {
                stats = c(V = result$W_Pos)
            } else
                stats <- c(W = result$W_Pos)
            ##
            res <- list(statistic = stats,
                        p.value = result$p,
                        data.name =dname,
                        alternative = "two.sided",
                        method = "Wilcoxon rank sum test"
           #            call=vcall
                        )
            class(res) <- "htest"
            return(res)
        } else {
            vviewName <- gen_view_name("MWTest")
            sqlstr <- paste0("CREATE VIEW ",vviewName," AS
                        SELECT t.vectorValueColumn AS Num_Val,
                               1 AS GroupID
                        FROM (",constructSelect(x),") AS t
                   UNION ALL
                        SELECT l.vectorValueColumn AS Num_Val,
                              2 AS GroupID
                        FROM (",constructSelect(y),") AS l
                      ")
            
            vcall <- as.list(sys.call())
            dname = paste0(vcall[2]," and ",vcall[3])

            t <- sqlSendUpdate(connection,sqlstr)
            ret <- sqlStoredProc(connection,
                                 "FLMWTest",
                                 TableName = vviewName,
                                 ValColName = "Num_Val",
                                 GroupColName = "GroupID",
                                 WhereClause = NULL ,
                                 GroupBy = NULL,
                                 TableOutput = 1,
                                 outputParameter = c(ResultTable = 'a'))

            sqlstr <- paste0("SELECT U_STAT AS W,
                             P_VALUE AS P
                     FROM ",ret$ResultTable)
            result <- sqlQuery(connection, sqlstr)

            res <- list(statistic = c(W = result$W),
                        p.value = result$P,
                        data.name = dname,
                        alternative = "two.sided",
                        method = "Wilcoxon rank sum test")
            class(res) <- "htest"
            return(res)
        }
    }
}
