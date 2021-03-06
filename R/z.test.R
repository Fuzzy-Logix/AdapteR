#' @include FLMatrix.R
NULL

#' Z test for 1 and 2 samples
#'
#' Performs z test for samples and sample proportions on vector values
#'
#' @param x FLVector
#' @param y FLVector
#' @param test_val The mean value for which the z test has to be performed.
#' The default value is 0.
#' @param prob The values is 1 if the sample set is for proportions. Otherwise, by default it's 0.
#' @return A list with class "htest" outputting the corresponding Z Stat and P Values.
#' @examples
#' flx<-as.FLVector(rnorm(100))
#' fly<-as.FLVector(rnorm(100))
#' z.test(flx)
#'     ## Two- sided one sample z-test.
#'     ## mean for 'x' is zero. The alternative hypothesis states 
#'     ## that it is either greater or less than zero.
#'
#' z.test(flx,fly,2)
#'     ## Two- sided two sample z-test.
#'     ## The null hypothesis is that the population mean for 'x'
#'     ## less that for 'y' is 2. 
#'     ## The alternative hypothesis is that this difference is not 2.
#'
#' ## z- test for population proportion.
#' fla<-as.FLVector(sample(c(0,1),replace=T,size=100))
#' flb<-as.FLVector(sample(c(0,1),replace=T,size=100))
#'  ##test_val= 0.4 implies that null hypothesis is
#'  ##population proportion is 0.4
#' z.test(fla,prob=1, test_val= 0.4)
#' z.test(fla,flb,prob=1)
#' @export
setGeneric("z.test",function(x,y=NULL,test_val=0,tails=2,
                            conf.level=0.95,prob=0,...)
                standardGeneric("z.test"))

setMethod("z.test",signature(x="FLVector"),
    function(x,
            y=NULL,
            test_val=0,
            tails=2,
            conf.level=0.95,
            prob=0){
        checkHypoSystemTableExists()
        if(is.null(x)||!is.FLVector(x))
            stop("Only FLVector is supported")

        if(!tails %in% c("1","2")) stop("Please enter 1 or 2 as tails")

        if(length(y)==0){
            if(prob==0)
            {
                pFuncName<-"FLzTest1S"
                vsqlstr <- constructHypoTestsScalarQuery(pFuncName = pFuncName,
                                                        pFuncArgs = c(test_val,
                                                                    "a.vectorValueColumn",
                                                                    tails),
                                                        pFrom=c(a=constructSelect(x)),
                                                        pStats=c("P_VALUE","Z_STAT"))
                # vsqlstr<- constructAggregateSQL(pFuncName=pFuncName,
                #                                 pFuncArgs=c(test_val,
                #                                             "a.vectorValueColumn",
                #                                             tails),
                #                                 pAddSelect=c(stat="c.FLStatistic"),
                #                                 pFrom=c(a=constructSelect(x),
                #                                         c="fzzlARHypTestStatsMap"),
                #                                 pWhereConditions="c.FLFuncName='FLzTest1S'",
                #                                 pGroupBy="c.FLStatistic")

         }

            else { 
                pFuncName<-"FLzTest1P"
                # vsqlstr<- constructAggregateSQL(pFuncName=pFuncName,
                #                                 pFuncArgs=c("c.FLStatistic",
                #                                             test_val,
                #                                             "a.vectorValueColumn",
                #                                                 tails),
                #                                 pAddSelect=c(stat="c.FLStatistic"),
                #                                 pFrom=c(a=constructSelect(x),
                #                                         c="fzzlARHypTestStatsMap"),
                #                                 pWhereConditions="c.FLFuncName='FLzTest1P'",
                #                                 pGroupBy="c.FLStatistic")
                vsqlstr <- constructHypoTestsScalarQuery(pFuncName = pFuncName,
                                                        pFuncArgs = c(test_val,
                                                                    "a.vectorValueColumn",
                                                                    tails),
                                                        pFrom=c(a=constructSelect(x)),
                                                        pStats=c("P_VALUE","Z_STAT"))

            }
            vcall<-paste(all.vars(sys.call())[1])
            alter<-paste0("true mean is not equal to ",test_val)
            estimate<-c("mean of x"=mean(x))
            method<-"One sample z-test"
        }

        else{
            if(prob==0)
            {
            pFuncName<-"FLzTest2S"
            vunionSelect <- constructUnionSQL(pFrom=c(a=constructSelect(x),b=constructSelect(y)),
                                              pSelect=list(a=c(groupID=1,num_val="a.vectorValueColumn"),
                                                           b=c(groupID=2,num_val="b.vectorValueColumn")))
            # vsqlstr<-constructAggregateSQL(pFuncName=pFuncName,
            #                             pFuncArgs=c("c.FLStatistic",
            #                                         "a.groupID",
            #                                         "a.num_val",
            #                                         tails),
            #                             pAddSelect=c(stat="c.FLStatistic"),
            #                             pFrom=c(a=vunionSelect,
            #                                     c="fzzlARHypTestStatsMap"),
            #                             pWhereConditions=c("c.FLFuncName='FLzTest2S'"),
            #                             pGroupBy="c.FLStatistic")
            vsqlstr <- constructHypoTestsScalarQuery(pFuncName = pFuncName,
                                                    pFuncArgs = c("a.groupID",
                                                                "a.num_val",
                                                                tails),
                                                    pFrom=c(a=vunionSelect),
                                                    pStats=c("P_VALUE","Z_STAT"))
            }

            else {

            pFuncName<-"FLzTest2P"
            vunionSelect <- constructUnionSQL(pFrom=c(a=constructSelect(x),b=constructSelect(y)),
                                              pSelect=list(a=c(groupID=1,num_val="a.vectorValueColumn"),
                                                           b=c(groupID=2,num_val="b.vectorValueColumn")))
            # vsqlstr<-constructAggregateSQL(pFuncName=pFuncName,
            #                             pFuncArgs=c("c.FLStatistic",
            #                                         "a.groupID",
            #                                         "a.num_val",
            #                                         tails),
            #                             pAddSelect=c(stat="c.FLStatistic"),
            #                             pFrom=c(a=vunionSelect,
            #                                     c="fzzlARHypTestStatsMap"),
            #                             pWhereConditions=c("c.FLFuncName='FLzTest2P'"),
            #                             pGroupBy="c.FLStatistic")
            vsqlstr <- constructHypoTestsScalarQuery(pFuncName = pFuncName,
                                                    pFuncArgs = c("a.groupID",
                                                                "a.num_val",
                                                                tails),
                                                    pFrom=c(a=vunionSelect),
                                                    pStats=c("P_VALUE","Z_STAT"))

            }
            vcall<-paste(all.vars(sys.call())[1:2],collapse=" and ")
            method<-"Two sample z-test"
            alter<-"true difference in means is not equal to 0 "
            estimate <-c("mean of x" = mean(x),"mean of y" = mean(y))       
         }

    vres<-sqlQuery(connection,vsqlstr)
    colnames(vres) <- tolower(colnames(vres))
    cint<-cint(x,conf.level)
    attr(cint,"conf.level") <- conf.level
    vresList<-list(statistic=c("Z stat"=vres$z_stat),
                   p.value=c("p-value"=vres$p_value),
                   data.name=vcall,
                   alternative=alter,
                   estimate =estimate,
                   method=method,
                   conf.int = cint)
    class(vresList)<-"htest"
    return(vresList)
    }
  )

setMethod("z.test",signature(x="ANY"),
    function(x,y,...){
        if (!requireNamespace("PASWR", quietly = TRUE)){
                stop("PASWR package needed for z.test. Please install it.",
                call. = FALSE)}
        else return(PASWR::z.test(x,y,...))})
