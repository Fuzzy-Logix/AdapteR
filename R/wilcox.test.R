wilcox.test.FLVector <- function(x,y = NULL,paired = TRUE, mu = 0,...)
{
    if(!is.FLVector(x) || !is.FLVector(y))

        stop("Must be FLVector")

    else
    {
        if(paired)
        {
            vviewName <- gen_view_name("wsrTest")
            if(length(x)> length(y))
                res <- sqlSendUpdate(connection, createHypoView(y,x,vviewName))
            else
                res <- sqlSendUpdate(connection, createHypoView(x,y,vviewName))
            
                                        #  Using Stored Proc Query.

            ret <- sqlStoredProc(connection,
                                 "FLWSRTest",
                                 TableName = vviewName,
                                 Val1ColName = "Num_Val1",
                                 Val2ColName = "Num_Val2",
                                 WhereClause = NULL ,
                                 GroupBy = NULL,
                                 TableOutput = 1,
                                 outputParameter = c(ResultTable = 'a'))

            sqlstr <- paste0( "SELECT q.W_STAT AS W,
                                  q.P_VALUE AS p  
                           FROM ",ret$ResultTable," AS q")
            result <-  sqlQuery(connection,sqlstr)       
            
                                        # Extracting the result.
         #   vcall <-all.vars(sys.call())
          #  print(vcall)
            res <- list(statistics = c(W = result$W),
                        p.value = result$p,
                                        #data.name = paste0((x)," and ",substitute(y)),
                        alternative = "two.sided",
                        method = "Wilcoxon rank sum test",
           #            call=vcall
                        )
            class(res) <- "htest"
            return(res)
        }
       
        else
        {
            
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

            t <- sqlSendUpdate(connection,sqlstr)
            str <- paste0("SELECT * FROM ",vviewName)
            retu <- sqlQuery(connection, str)
            

            str <- paste0("CALL FLMWTest('",vviewName,"', 'Num_Val',
                                           'GroupID', NULL, NULL, 1,
                                         ResultTable);")
            res_1 <- sqlQuery(connection, str)
                #        ret <- sqlStoredProc(connection,
     #                            "FLMWTest",
      #                           TableName = vviewName,
       #                          GroupColName = "GroupID",
        #                         Val1ColName = "Num_Val",
         #                        WhereClause = NULL ,
          #                       GroupBy = NULL,
           #                      TableOutput = 1,
            #                     outputParameter = c(ResultTable = 'a'))

            sqlstr <- paste0("SELECT U_STAT AS W,
                             P_VALUE AS P
                     FROM ",res_1$ResultTable)
            result <- sqlQuery(connection, sqlstr)

            res <- list(statistics = c(W = result$W),
                        p.value = result$P,
                                        #data.name = paste0((x)," and ",substitute(y)),
                        alternative = "two.sided",
                        method = "Wilcoxon rank sum test")
            class(res) <- "htest"
            return(res)
        }
    }
}
