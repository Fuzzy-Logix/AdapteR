#' @include FLMatrix.R
NULL

setGeneric("z.test",function(x,y=NULL,test_val=0,tails=2,conf.level=0.95,prob=0)
                standardGeneric("z.test"))
setMethod("z.test",signature(x="FLVector"),
    function(x,
            y=NULL,
            test_val=0,
            tails=2,
            conf.level=0.95,
            prob=0){
        if(is.null(x)||!is.FLVector(x))
            stop("Only FLVector is supported")

        if(!tails %in% c("1","2")) stop("Please enter 1 or 2 as tails")

        vcall<-paste(all.vars(sys.call())[1:2],collapse=" and ")

        if(length(y)==0){
            if(!test_val) stop("The testing value is missing")
            if(prob==0)
            {
                pFuncName<-"FLzTest1S"
                vsqlstr<- constructAggregateSQL(pFuncName=pFuncName,
                                                pFuncArgs=c("c.FLStatistic",
                                                            test_val,
                                                            "a.vectorValueColumn",
                                                                tails),
                                                pAddSelect=c(stat="c.FLStatistic"),
                                                pFrom=c(a=constructSelect(x),
                                                        c="fzzlARHypTestStatsMap"),
                                                pWhereConditions="c.FLFuncName='FLzTest1S'",
                                                pGroupBy="c.FLStatistic")

         }

            else { 
                pFuncName<-"FLzTest1P"
                vsqlstr<- constructAggregateSQL(pFuncName=pFuncName,
                                                pFuncArgs=c("c.FLStatistic",
                                                            test_val,
                                                            "a.vectorValueColumn",
                                                                tails),
                                                pAddSelect=c(stat="c.FLStatistic"),
                                                pFrom=c(a=constructSelect(x),
                                                        c="fzzlARHypTestStatsMap"),
                                                pWhereConditions="c.FLFuncName='FLzTest1P'",
                                                pGroupBy="c.FLStatistic")

            }
        }

        else{
            if(prob==0)
            {
            pFuncName<-"FLzTest2S"
            vsqlstr<-constructAggregateSQL(pFuncName=pFuncName,
                                        pFuncArgs=c("c.FLStatistic",
                                                    "a.Num_Val1",
                                                    "a.Num_Val2",
                                                    tails),
                                        pAddSelect=c(stat="c.FLStatistic"),
                                        pFrom=c(a=constructSelect(createHypoView(x,y)),
                                                c="fzzlARHypTestStatsMap"),
                                        pWhereConditions=c("c.FLFuncName='FLzTest2S'"),
                                        pGroupBy="c.FLStatistic")
            }

            else {

            pFuncName<-"FLzTest2P"
            vsqlstr<-constructAggregateSQL(pFuncName=pFuncName,
                                        pFuncArgs=c("c.FLStatistic",
                                                    "a.Num_Val1",
                                                    "a.Num_Val2",
                                                    tails),
                                        pAddSelect=c(stat="c.FLStatistic"),
                                        pFrom=c(a=constructSelect(createHypoView(x,y)),
                                                c="fzzlARHypTestStatsMap"),
                                        pWhereConditions=c("c.FLFuncName='FLzTest2P'"),
                                        pGroupBy="c.FLStatistic")



            }        
         }

    vres<-sqlQuery(connection,vsqlstr)
    cint<-cint(x,conf.level)
    attr(cint,"conf.level") <- conf.level
    vresList<-list(statistic=c("P value"=vres[1,1]),
                   parameter=c("Z stat"=vres[2,1]),
                   data.name=vcall,
                   alternative=paste0("true mean is not equal to ",test_val),
                   estimate =c("mean of x" = mean(x)),
                   method="One Sample z-test",
                   conf.int = cint
                   )
    class(vresList)<-"htest"
    return(vresList)
    }
  )