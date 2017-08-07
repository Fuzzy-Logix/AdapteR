#' Wilcox Test.
#' 
#' An S3 class to represent wilcox signed Rank test Performs one- and
#' two-sample Wilcoxon tests on vectors of data; the latter is also
#' known as \code{Mann-Whitney} test.
#' If only x is given, or if both x and y are given and paired is
#' TRUE, a Wilcoxon signed rank test of the null that the distribution
#' of x (in the one sample case) or of x - y (in the paired two sample
#' case) is symmetric about mu is performed. Otherwise, if both x and
#' y are given and paired is FALSE, a Wilcoxon rank sum test
#' (equivalent to the Mann-Whitney test: see the Note) is carried out.
#' In this case, the null hypothesis is that the distributions of x
#' and y differ by a location shift of mu and the alternative is that
#' they differ by some other location shift (and the one-sided
#' alternative "greater" is that x is shifted to the right of y).
#' @param x FLvector of data values. Non-finite (e.g., infinite or
#'     missing) values will be omitted.
#' @param y an optional FLVector of data values: as with x non-finite values will be omitted.
#' @param paired a logical indicating whether you want a paired test.
#' @section Constraints: conf.level, conf.int is not supported currently for FL objects.
#' @seealso \code{\link[stats]{wilcox.test}} for R reference implementation.
#' @return A list with class "htest".
#' @examples
#' Wilcoxon Signed Rank test:
#' a <- as.FLVector(c(85,70,40,65,80,75,55,20))
#' b <- as.FLVector(c(75,50,50,40,20,65,40,25))
#' res <- wilcox.test(a, b, paired = TRUE)
#'
#' Mann-Whitney test:
#' a <- as.FLVector(c(6, 8, 2, 4, 4, 5))
#' b <- as.FLVector(c(7, 10, 4, 3, 5, 6))
#' res <- wilcox.test(a, b, paired = FALSE)
#' @export
setGeneric("wilcox.test",function(x, ...)
    standardGeneric("wilcox.test"))

setMethod("wilcox.test",signature(x="ANY"),
    function(x,...){
        return(stats::wilcox.test(x,...))
})

setMethod("wilcox.test",signature(x="FLVector"),
    function(x,y = NULL,paired = TRUE, mu = 0,...)
    {
        if(!is.FLVector(x) || !is.FLVector(y))
            stop("Must be FLVector")
        else {
            if(paired) {
                vviewName <- gen_view_name("wsrTest")
                if(length(x)> length(y))
                    res <- sqlSendUpdate(connection, createHypoView(y,x,vviewName))
                else
                    res <- sqlSendUpdate(connection, createHypoView(x,y,vviewName))
                ##
                vcall <- as.list(sys.call())
                dname = paste0(vcall[2]," and ",vcall[3])
                ##  Using Stored Proc Query.
                ret <- sqlStoredProc(connection,
                                     "FLWSRTest",
                                     TableName = vviewName,
                                     Val1ColName = "Num_Val1",
                                     Val2ColName = "Num_Val2",
                                     WhereClause = "NULL" ,
                                     GroupBy = "DatasetID",
                                     TableOutput = 1,
                                     outputParameter = c(ResultTable = 'a'))
                colnames(ret) <- tolower(colnames(ret))
                if(!is.null(ret$resulttable)){
                    sqlstr <- paste0( "SELECT q.W_STAT AS w_stat,
                                              q.P_VALUE AS p_value,
                                              q.W_STAT_Neg AS w_stat_neg,
                                              q.W_STAT_Posi AS w_stat_posi
                                   FROM ",ret$resulttable," AS q")
                    result <-  sqlQuery(connection,sqlstr)
                }
                else result <- ret
                stats <- c(V = result$w_stat_posi)  
                ##
                res <- list(statistic = stats,
                            parameter = NULL,
                            p.value = result$p_value,
                            null.value = c("location shift"=0),
                            alternative = "two.sided",
                            method = "Wilcoxon signed rank test",
                            data.name =dname
                            
                                            #            call=vcall
                            )
                class(res) <- "htest"
                dropView(vviewName)
                return(res)
            } else {
                vviewName <- gen_view_name("MWTest")
                t <- constructUnionSQL(pFrom = c(a = constructSelect(x),
                                                 b = constructSelect(y)),
                                       pSelect = list(a = c(DatasetID=1,
                                                            GroupID = 1,
                                                            Num_Val = "a.vectorValueColumn"),
                                                      b = c(DatasetID=1,
                                                            GroupID = 2,
                                                            Num_Val = "b.vectorValueColumn")))
                q <- createView(vviewName,t)
                
                vcall <- as.list(sys.call())
                dname = paste0(vcall[2]," and ",vcall[3])
                ret <- sqlStoredProc(connection,
                                     "FLMWTest",
                                     TableName = vviewName,
                                     ValColName = "Num_Val",
                                     GroupColName = "GroupID",
                                     WhereClause = "NULL" ,
                                     GroupBy = "DatasetID",
                                     TableOutput = 1,
                                     outputParameter = c(ResultTable = 'a'))

                colnames(ret) <- tolower(colnames(ret))
                if(!is.null(ret$resulttable)){
                    sqlstr <- paste0("SELECT U_STAT AS u_stat, \n ",
                                    " P_VALUE AS p_value \n ",
                                    " FROM ",ret$resulttable)
                    result <- sqlQuery(connection, sqlstr)
                }
                else result <- ret
                
                res <- list(statistic = c(W = result$u_stat),
                            parameter = NULL,
                            p.value = result$p_value,
                            null.value = c("location shift"=0),
                            alternative = "two.sided",
                            method = "Wilcoxon rank sum test",
                            data.name = dname
                            )
                class(res) <- "htest"
                dropView(vviewName)
                return(res)
            }
        }
    })

