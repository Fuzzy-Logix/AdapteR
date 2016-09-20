t.test.FLVector <- function(x,
                            y=NULL,
                            mu = 0,
                            tails=2,
                            conf.level =.95,
                            var.equal=FALSE,
                            alternative="two.sided",...)
{       
        if(is.null(x)||!is.FLVector(x))
        stop("Only FLVector is supported")

        if(!tails %in% c("1","2")) stop("Please enter 1 or 2 as tails")

        if(length(y)==0){
        sqlstr <- constructAggregateSQL(pFuncName = "FLtTest1S",
                                        pFuncArgs = c("c.FLStatistic",
                                                        mu,
                                                      "a.vectorValueColumn",
                                                       tails),
                                        pAddSelect = c(stat="c.FLStatistic",
                                                       df = "COUNT(DISTINCT a.vectorValueColumn)"),
                                                                              
                                        pFrom = c(a = constructSelect(x),
                                                  c = "fzzlARHypTestStatsMap"),
                                        pWhereConditions = c("c.FLFuncName = 'FLtTest1S'"),
                                        pGroupBy = "c.FLStatistic")
        vcall<-paste(all.vars(sys.call())[1],collapse=" and ")
        alter<-paste0("true mean is not equal to ",mu )
        estimate<-c("mean of x"=mean(x))
        method<-"One Sample t-test"}                                         
           
        else{
            if(var.equal==TRUE) {
                    var<-"EQUAL_VAR"
                    method<-"Two Sample t-test"}
            else{
                var<-"UNEQUAL_VAR"
                method<-"Welch Two Sample t-test"}
            vunionSelect <- constructUnionSQL(pFrom=c(a=constructSelect(x),b=constructSelect(y)),
                                              pSelect=list(a=c(groupID=1,num_val="a.vectorValueColumn"),
                                                           b=c(groupID=2,num_val="b.vectorValueColumn")))

            sqlstr<-constructAggregateSQL(pFuncName="FLtTest2S",
                                        pFuncArgs=c("c.FLStatistic",
                                                     fquote(var),
                                                    "a.groupID",
                                                    "a.num_val",
                                                    tails),
                                        pAddSelect=c(stat="c.FLStatistic"),
                                        pFrom=c(a=vunionSelect,
                                                c="fzzlARHypTestStatsMap"),
                                        pWhereConditions=c("c.FLFuncName='FLtTest2S'"),
                                        pGroupBy="c.FLStatistic")
            vcall<-paste(all.vars(sys.call())[1:2],collapse=" and ")
            alter<-"true difference in means is not equal to 0 "
            estimate <-c("mean of x" = mean(x),"mean of y" = mean(y))
            }  
    result <<- sqlQuery(connection, sqlstr)
    cint<-cint(x,conf.level,alternative)
    attr(cint,"conf.level") <- conf.level
    res <- list(data.name = vcall,
                statistic =c(t = as.numeric(result[1,1])),
                p.value=   c("p-value"=as.numeric(result[2,1])),
                alternative=alter,
                estimate =estimate,
                method=method,
                conf.int = cint)                
    class(res) <- "htest"
    return(res)
}

