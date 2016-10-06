#' @include FLMatrix.R
NULL
#' Anderson-Darling Test
#'
#' 
#' Performs the Anderson-Darling test for the composite hypothesis of normality, see e.g. Thode (2002, Sec. 5.1.4).
#'The Anderson-Darling test is an EDF omnibus test for the composite hypothesis of normality. The Anderson-Darling test is the recommended EDF test by Stephens (1986). Compared to the Cramer-von Mises test (as second choice) it gives more weight to the tails of the distribution.
#' 
#' @param x: a FLVector of data values.
#' 
#' @examples
#' set.seed(200)
#' a <- as.FL(rnorm(100, mean = 5, sd = 3))
#' res <- ad.test(a)
#'
#' @export
setGeneric("ad.test",function(x, ...)
    standardGeneric("ad.test"))




setMethod("ad.test",signature(x="ANY"),
          function(x, ...)
          {
              if (!requireNamespace("nortest", quietly = TRUE)){
                  stop("nortest package needed for ad.test.Please install it.",
                       call. = FALSE)
              }
              else return(nortest::ad.test(x,...))
              
              })

setMethod("ad.test",signature(x="FLVector"),
          function(x, mean = NULL, sd = NULL, ...)

          {
              dname <- deparse(substitute(x))
              vviewName <- gen_view_name("adtest")
              sqlstr <- paste0("SELECT p.vectorValueColumn AS Num_Val
                                FROM (",constructSelect(x),") AS p")
              q <- createView(vviewName,sqlstr)

              ret <- sqlStoredProc(connection,
                                   "FLADTest",
                                   TableName = vviewName,
                                   ValueCol = "Num_Val",
                                   Mean = mean,
                                   StdDev = sd,
                                   WhereClause = NULL,
                                   GroupBy = NULL,
                                   TableOutput = 1,
                                   outputParameter = c(OutTable = 'a')
                                   )
              sqlstr <- paste0("SELECT q.TEST_STAT AS TStat,
                                       q.P_VALUE AS P_Value
                               FROM ",ret$OutTable," AS q")
              res_1 <- sqlQuery(connection, sqlstr)
              result <- list(statistics = c(A = res_1$TStat))
              if(!class(res_1$P_Value) == "numeric"){
                  result$p.value <- as.numeric(gsub("^[[:space:]]*[[:punct:]]*[[:space:]]*","",res_1$P_Value))
                  result$alternative <- paste0("Note: rounded p-value ", res_1$P_Value)
              } else {
                  result$p.value <- res_1$P_Value
              }
              result$method = "Anderson-Darling normality test"
              result$data.name = dname

              class(result) <- "htest"
              dropView(vviewName)
              return(result)
          }
          )
