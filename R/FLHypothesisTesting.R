
cint<-function(x,y=NULL,conf.level=0.95,mu=0,var.equal=FALSE,alternative="two.sided"){
    if (alternative=="two.sided"){
      if(length(y)==0){
        df<-length(x)-1
        sd<-sd(x)/sqrt(length(x))
        qt<-qt(conf.level+(1-conf.level)/2,df)*sd
        res<-mean(x)+c(-qt,qt)}
      else{
        if(var.equal){
          nx<-length(x)
          ny<-length(y)
          df<-nx+ny-2
          v<-0
          if(nx > 1) v <- v + (nx-1)*var(x)
          if(ny > 1) v <- v + (ny-1)*var(y)
          v<-v/df
          stder<-sqrt(v*(1/nx+1/ny))
        }
        else{ 
          stderx<-sqrt(var(x)/length(x))
          stdery<-sqrt(var(y)/length(y))
          stder<-sqrt(stderx^2+stdery^2)
          df<-stder^4/(stderx^4/(length(x)-1) + stdery^4/(length(y)-1))
        }
        tstat<-(mean(x)-mean(y)-mu)/stder
          pval <- 2 * pt(-abs(tstat), df)
          alpha <- 1 - conf.level
          res<- qt(1 - alpha/2, df)
          res<- tstat + c(-res,res)
          res<-mu+res*stder
        }
        result<-c(res,df)
        return(result)
      }
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

