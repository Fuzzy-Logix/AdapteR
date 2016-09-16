#setGeneric("ks.test",function(x,y=NULL,mean = NULL, sd = NULL, ...,
 #                             alternative="two.sided",
  #                            exact=NULL)
  #  standardGeneric("ks.test"))

#setMethod("ks.test",signature(x="ANY", y="ANY"),
#          function(x,
#                   y, ...,
#                   alternative = c("two.sided", "less", "greater"),
#                   exact = NULL)
#          {
#              return(stats::ks.test(x = x,
#                                    y = y, ...,
#                                    alternative = alternative,
#                                    exact = exact)
#                     )
#          }
#          )


setMethod("ks.test",signature(x="FLVector"),
          function(x,
                   y,
                   mean = NULL,
                   sd = NULL)

          {
              dname <- deparse(substitute(x))
              
              if(!is.FLVector(x))
                  stop("Only take FLVector")
              vviewName <- gen_view_name("kstest1s")
              sqlstr <- paste0("SELECT t.vectorValueColumn AS Num_Val
                     FROM (",constructSelect(x),") AS t")
              t <- createView(vviewName, sqlstr)
              ret <- sqlStoredProc(connection,
                                   "FLKSTest1s",
                                   TestName = 'NORMAL',
                                   TableName = vviewName,
                                   ValueCol = "Num_Val",
                                   Mean = mean,
                                   StdDev = sd, 
                                   WhereClause = NULL,
                                   GroupBy = NULL,
                                   TableOutput = 1,
                                   outputParameter = c(ResultTable = 'a')
                                   )
             # dname <- as.list(sys.call(sys.parent()))[[2]]
              sqlstr <- paste0("SELECT q.D_STAT AS D,
                                       q.D_PValue AS P_Value
                               FROM ",ret$OutTable," AS q")
              res_1 <- sqlQuery(connection, sqlstr)
              if(! class(res_1$P_Value) == "numeric")
                  pval <- as.character(res_1$P_Value)
              else
                  pval <- res_1$P_Value

              result <- list(statistics = c(D = res_1$D),
                             p.value = pval,
                             alternative = "two-sided",
                             method = "One-sample Kolmogorov-Smirnov test",
                             data.name = as.character(dname))
              class(result) <- "htest"
              return(result)              
          }
          )

setMethod("ks.test",signature(x="FLVector", y = "FLVector"),
          function(x,
                   y,
                   alternative ="two.sided",
                   exact = FALSE)

          {
              dname <- paste0(deparse(substitute(x))," and ",deparse(substitute(y)))
              if(!is.FLVector(x) || !is.FLVector(y))
                  stop("Only takes FLVector")
              vviewName <- gen_view_name("kstest2s")
              t <- constructUnionSQL(pFrom= c(a = constructSelect(x),
                                              b = constructSelect(y)),
                                     pSelect = list(a = c(GroupID = 1,
                                                          Num_Val = "a.vectorValueColumn"),
                                                    b = c(GroupID = 2,
                                                          Num_Val = "b.vectorValueCOlumn")
                                                    ))

              q <- createView(vviewName, t)
              ret <- sqlStoredProc(connection,
                                   "FLKSTest2s",
                                   TableName = vviewName,
                                   ValueCol = "Num_Val",
                                   GroupCol = "GroupID", 
                                   WhereClause = NULL,
                                   GroupBy = NULL,
                                   TableOutput = 1,
                                   outputParameter = c(ResultTable = 'a')
                                   )
              vcall <- as.list(sys.call(sys.parent()))
#              dname <- paste0(vcall[2]," and ",vcall[3])
              sqlstr <- paste0("SELECT q.D_STAT AS D,
                                       q.P_Value AS P_Value
                               FROM ",ret$OutTable," AS q")
              res_1 <- sqlQuery(connection, sqlstr)

              if(!class(res_1$P_Value) == "numeric")
                  pval <- as.character(res_1$P_Value)
              else
                  pval <- res_1$P_Value

              
              result <- list(statistics = c(D = res_1$D),
                             p.value = pval,
                             alternative = "two-sided",
                             method = "Two-sample Kolmogorov-Smirnov test",
                             data.name = dname                             
                             )
              class(result) <- "htest"
              return(result)            
          }
          )




constructUnionSQL <- function(pFrom,
                              pSelect=NULL){
    vFrom <- as.list(pFrom)
    vSelects <- sapply(1:length(vFrom),
                       function(x){
                           if(is.null(pSelect))
                               vinnerSelect <- "*"
                           else{
                               vinnerSelect <- pSelect[[names(vFrom)[[x]]]]
                               vinnerSelect <- ifelse(!is.null(names(vinnerSelect)),
                                                      paste0(vinnerSelect," AS ",names(vinnerSelect),collapse=","),
                                                      paste0(vinnerSelect,collapse=","))
                           }
                           return(paste0("SELECT ",vinnerSelect," \n ",
                                         " FROM (",vFrom[[x]],") AS ",
                                         names(vFrom)[[x]]))
                       })
    return(paste0(vSelects, collapse= " \n UNION ALL \n "))
}
