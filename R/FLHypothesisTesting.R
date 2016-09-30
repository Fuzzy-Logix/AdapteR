
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

