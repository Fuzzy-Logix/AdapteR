NULL


#' Kolmogorov-Smirnov test
#'
#' Perform a one- or two-sample Kolmogorov-Smirnov test.
#' @param x a FLVector of data values.
#' @param y either a FLVector of data values, or a character string naming a cumulative distribution function or
#' an actual cumulative distribution function such as pnorm. Only continuous CDFs are valid.
#' @section Constraints: As of now only supports normal disctribution, alternative and exact isn't supported for FL objects
#' @return A list with class "htest".
#' @examples
#' set.seed(100)
#' p <- as.FLVector(rnorm(50))
#' q <- as.FLVector(runif(30))
#' ks.test(p, y= "NORMAL" , mean=0, sd=1)
#'          ## One sample K-S test.
#' res <- ks.test(p, q)
#'          ## Two sample K-S Test.
#'
#' #If y is a FLVector, a two-sample test of the null hypothesis that x and y were drawn from the 
#' #same continuous distribution is performed. Alternatively, y can be a character string naming 
#' #a continuous (cumulative) distribution function, or such a function. In this case, a one-sample 
#' #test is carried out of the null that the distribution function which generated x is distribution
#' #y with parameters.
#' @seealso \code{\link[stats]{ks.test}} for corresponding R function reference.
#' @export
setGeneric("ks.test",function(x,y=NULL,mean = NULL, sd = NULL, ...,
                              alternative="two.sided",
                              exact=NULL)
    standardGeneric("ks.test"))

#' @export
setMethod("ks.test",signature(x="ANY", y="ANY"),
         function(x,
                  y, ...,
                  alternative = c("two.sided", "less", "greater"),
                  exact = NULL)
         {
             return(stats::ks.test(x = x,
                                   y = y, ...,
                                   alternative = alternative,
                                   exact = exact)
                    )
         }
         )

#' @export
setMethod("ks.test",signature(x="FLVector"),
          function(x,
                   y,
                   mean = NULL,
                   sd = NULL)

          {
              dname <- deparse(substitute(x))
              ##browser()
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
                                   outputParameter = c(OutTable = 'a')
                                   )
             # dname <- as.list(sys.call(sys.parent()))[[2]]
              colnames(ret) <- tolower(colnames(ret))
             if(!is.null(ret$outtable)){
                sqlstr <- paste0("SELECT q.D_STAT AS D_STAT,
                                       q.D_PValue AS D_PValue \n ",
                                 " FROM ",ret$outtable," AS q")
                res_1 <- sqlQuery(connection, sqlstr)
             }
             else res_1 <- ret
              colnames(res_1) <- tolower(colnames(res_1))
              if(! class(res_1$d_pvalue) == "numeric")
                  pval <- as.numeric(gsub("^[[:space:]]*[[:punct:]]*[[:space:]]*","",res_1$d_pvalue))
              else
                  pval <- res_1$d_pvalue

              result <- list(statistic = c(D = res_1$d_stat),
                             p.value = pval,
                             alternative = "two-sided",
                             method = "One-sample Kolmogorov-Smirnov test",
                             data.name = as.character(dname))
              class(result) <- "htest"
              dropView(vviewName)
              return(result)
          }
          )

#' @export
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
                                   WhereClause = "NULL",
                                   GroupBy = "NULL",
                                   TableOutput = 1,
                                   outputParameter = c(OutTable = 'a')
                                   )
              vcall <- as.list(sys.call(sys.parent()))
#              dname <- paste0(vcall[2]," and ",vcall[3])
            colnames(ret) <- tolower(colnames(ret))
            if(!is.null(ret$outtable)){
                sqlstr <- paste0("SELECT q.D_STAT AS D,
                                       q.P_Value AS P_Value
                               FROM ",ret$outtable," AS q")
                res_1 <- sqlQuery(connection, sqlstr)
            }
            else res_1 <- ret
              colnames(res_1) <- tolower(colnames(res_1))

              res_1 <- ModifyHypoResultColnames("FLKSTest2sResults",res_1)
              if(!class(res_1$p_value) == "numeric")
                  pval <- as.numeric(gsub("^[[:space:]]*[[:punct:]]*[[:space:]]*","",res_1$p_value))
              else
                  pval <- res_1$p_value


              result <- list(statistic = c(D = res_1$d_stat),
                             p.value = pval,
                             alternative = "two-sided",
                             method = "Two-sample Kolmogorov-Smirnov test",
                             data.name = dname
                             )
              class(result) <- "htest"
              dropView(vviewName)
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
