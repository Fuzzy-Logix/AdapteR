NULL

## gk: please add see also 
#' F Test.
#' 
#'
#' Performs an F test to compare the variances of two samples from normal populations.
#' The null hypothesis is that the ratio of the variances of the populations from which
#' x and y were drawn, or in the data to which the linear models x and y were fitted,
#' is equal to ratio.
#' @param x: FLVector of data values
#' @param y: FLVector of data values
#' @param alternative: a character string specifying the alternative hypothesis,
#'                    must be one of '"two.sided"' (default), '"greater"' or
#'                   '"less"'.
#' @section Constraints: conf.level, ratio, formula, data aren't supported currently for FL objects.
#'
#' @return A list with class "htest".
#'
#' @examples
#' set.seed(450)
#' x <- as.FLVector(rnorm(50, mean = 0, sd = 2))
#' y <- as.FLVector(rnorm(30, mean = 1, sd = 1))
#' var.test(x, y, "two.sided")
#' @export
var.test.FLVector <- function(x, y, ratio = 1, alternative = "two.sided", conf.level = .95){
    if(!is.FLVector(x) || !is.FLVector(y))
        print("only takes FLVector")
    else{
        vmapping <- c(two.sided = "Two_sided", less = "Less", greater = "Greater")
        t <- constructUnionSQL(pFrom = c(a = constructSelect(x),
                                         b = constructSelect(y)),
                               pSelect = list(a = c(DataSetID = 1,
                                                    GroupID = 1,
                                                    Num_Val = "a.vectorValueColumn"
                                                    ),
                                              b = c(DataSetID = 1,
                                                    GroupID = 2,
                                                    Num_Val = "b.vectorValueColumn")))
        tName <- gen_unique_table_name("ftest")
        p <- createTable(tName,pSelect=t)
        vcall <- as.list(sys.call())
        dname <- paste0(vcall[[2]]," and ",vcall[[3]])
        ## gk: please use a generator function
        ## constructUDTSQL("DataSetID, GroupID, Num_Val", "FLFTestUdt", "*")
        str <- paste0("WITH z(DataSetID, GroupID, Num_Val) AS
                                 (SELECT q.* FROM ",p," AS q)
SELECT a.* FROM TABLE (FLFTestUdt(z.DataSetID, '",vmapping[alternative],"', z.GroupID, z.Num_Val)
HASH BY z.DataSetID
LOCAL ORDER BY z.DataSetID)
AS a;")
        ret <- as.data.frame(sqlQuery(connection, str))
        
        res <- list(statistics = c(F = ret$FStat),
                    parameter = c("num df" = ret$DF1,"denom df" = ret$DF2),
                    p.value = ret$P_Value,
                    conf.int = NULL,
                    estimate = c("ratio of variances"=ret$FStat),
                    null.value = c("ratio of variances"=1),
                    alternative = alternative,
                    method = "F test to compare two variances",
                    data.name = dname)

        class(res) <- "htest"
        return(res)  

    }}

