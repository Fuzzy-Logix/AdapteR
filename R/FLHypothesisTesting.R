
cint<-function(x,conf.level,alternative="two.sided"){
    if (alternative=="two.sided")
    {
        df<-length(x)-1
        sd<-sd(x)/sqrt(length(x))
        qt<-qt(conf.level+(1-conf.level)/2,df)*sd
        res<-mean(x)+c(-qt,qt)
    }
    else stop("Not available for others")
    return(res)
}


## gk: please move to file wald.wolfowitz.test.R
WaldWolftest1s  <- function(vFLvector,threshold = 0) {
    if(!is.FLVector(vFLvector))
       stop("Only take FLVector")
       else
       {
           vviewName <- gen_view_name("ww1sTest")
           if(threshold)
           {
               t <- sqlSendUpdate(connection,
                                  createSignview(vviewName, threshold, vFLvector))
}
           else{
               sqlstr <- paste0("SELECT FLMedianUDT(q.GroupID,q.Num_Val)
                                FROM ",vviewName)
               partition = sqlQuery(connection, sqlstr)
               t <- sqlSendUpdate(connection,
                                  createSignview(vviewName, threshold, vFLvector))
           }

           vcall <- as.list(sys.call())[[2]]
           
                                        #Testing the code part
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
                          data.name = vcall,
                          method = "Wald Wolfowitz test"
                          )
           class(result) <- "htest"
           return(result)


                                        #print(res_1)
                                        # print(paste0("P-Value is ",res_1$P,"Z Value is ",res_1$Z))
           
       }
    }


## join FLVecotor assign Sign Value and create a table name.
createSignview <- function(vName, vpart, vFLVector)
{
    
    sqlstr <- paste0("CREATE VIEW ",vName," AS
                        SELECT t.vectorValueColumn AS Num_Val,
                               1 AS GroupID,
                               t.vectorindexcolumn AS ObsID,
                              CASE WHEN t.vectorValueColumn > ",vpart,"
                                        THEN 1
                                   WHEN t.vectorValueColumn < ",vpart,"
                                        THEN -1
                                   ELSE 0
                              END AS Sign
                                FROM (",constructSelect(vFLVector),") AS t
                      ")
    return(sqlstr)
}

## gk: please use constructUnionSQL
## Joining two FLVectors and creating a volatile table.
createHypoView <- function(q,r,pViewName)
{
    vminLength <- length(q)
    
    sqlstr0 <- paste0("CREATE VIEW ",pViewName," AS
                           SELECT b.vectorindexcolumn AS ObsID,
                                  a.vectorValueColumn AS Num_Val1,
                                  b.vectorvalueColumn AS Num_Val2
                           FROM (",constructSelect(q),") a, (",constructSelect(r),") b
                           WHERE a.vectorindexcolumn = b.vectorindexcolumn
                           UNION ALL
                           SELECT b.vectorindexcolumn AS ObsID,
                                  NULL AS Num_Val1,
                                  b.vectorValueColumn AS Num_Val2 
                           FROM (",constructSelect(r),") b 
                           WHERE b.vectorindexcolumn >",vminLength)
    return(sqlstr0)         
}
