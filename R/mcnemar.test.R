#' @include FLMatrix.R
NULL

# FLFuncName <- c("FLMcNemarTest","FLMcNemarTest","FLMcNemarTest")
# FLStatistic <- c("T_STAT","CHI_SQ","BINOMIAL_EXACT")
# vdf <- data.frame(FLFuncName=FLFuncName,
#                 FLStatistic=FLStatistic)
# flt <- as.FLTable(vdf,tableName="fzzlARHypTestStatsMap",drop=TRUE)

#' McNemar's Chi-squared Test for Count Data.
#'
#' Performs McNemar's chi-squared test for 
#' symmetry of rows and columns in a two-dimensional contingency table.
#'
#' @param x FLVector
#' @param y FLVector
#' @param correct a logical or character indicating whether to apply 
#' continuity correction when computing the test statistic.
#' If TRUE,Available methods are EDWARDS(default),YATES.
#' @return A list with class "htest"
#' @examples
#' flTable <- FLTable("tblMcNemarMulti", "obsid",whereconditions="datasetID=1")
#' x <- flTable[,"predicted"]
#' y <- flTable[,"observed"]
#' ResulthtestObject <- mcnemar.test(x,y)
#' ResulthtestObject <- mcnemar.test(x,y,FALSE)
#' ResulthtestObject <- mcnemar.test(x,y,"YATES")
#' @export
setGeneric("mcnemar.test",function(x,y=NULL,correct=TRUE)
                standardGeneric("mcnemar.test"))
setMethod("mcnemar.test",signature(x="FLVector"),
    function(x,
            y=NULL,
            correct=TRUE){
        checkHypoSystemTableExists()
        if(is.null(y) || !is.FLVector(y))
            stop("contingency table not supported. x and y should be FLVectors. \n ")
        if(is.logical(correct)){
            if(correct) vcorrection <- "EDWARDS"
            else vcorrection <- "NONE"
        }
        else if(toupper(correct) %in% c("EDWARDS","YATES"))
            vcorrection <- toupper(correct)
        else stop("correct can be logical or character(YATES) \n ")

        vcall <- paste(all.vars(sys.call())[1:2],collapse=" and ")

        ## Casting to BYTEINT
        tryCatch(x <- as.FLByteInt(x),
                error=function(e) 
                warning("could not cast inputs to BYTEINT. Running as it is \n ")
                )
        tryCatch(y <- as.FLByteInt(y),
                error=function(e) 
                warning("could not cast inputs to BYTEINT. Running as it is \n ")
                )

        # vsqlstr <- constructAggregateSQL(pFuncName="FLMcNemarTest",
        #                                 pFuncArgs=c("c.FLStatistic",
        #                                             fquote(vcorrection),
        #                                             "a.vectorValueColumn",
        #                                             "b.vectorValueColumn"),
        #                                 pAddSelect=c(stat="c.FLStatistic",
        #                                             df="COUNT(DISTINCT a.vectorValueColumn)"),
        #                                 pFrom=c(a=constructSelect(x),
        #                                         b=constructSelect(y),
        #                                         c="fzzlARHypTestStatsMap"),
        #                                 pWhereConditions=c("a.vectorIndexColumn=b.vectorIndexColumn",
        #                                                     "c.FLFuncName='FLMcNemarTest'"),
        #                                 pGroupBy="c.FLStatistic")

        vstats <- c("t_stat","chi_sq","binomial_exact")
        vstats <- toupper(vstats)
        pAddSelect <- c()
        pFuncArgs <- c(fquote(vcorrection),
                        "a.vectorValueColumn",
                        "b.vectorValueColumn")
        pFuncArgs <- c("'stat'",pFuncArgs)
        for(i in vstats){
            pFuncArgs[1] <- fquote(i)
            pAddSelect <- c(pAddSelect,paste0("FLMcNemarTest(",paste0(pFuncArgs,collapse=","),")"))
        }
        names(pAddSelect) <- vstats

        vsqlstr <- constructAggregateSQL(pFuncName="FLMcNemarTest",
                                        pFuncArgs=pFuncArgs,
                                        pAddSelect=c(pAddSelect,
                                                    df="COUNT(DISTINCT a.vectorValueColumn)"),
                                        pFrom=c(a=constructSelect(x),
                                                b=constructSelect(y)),
                                        includeFuncCall=FALSE,
                                        pWhereConditions=c("a.vectorIndexColumn=b.vectorIndexColumn"))

        # vsqlstr <- constructHypoTestsScalarQuery(pFuncName = "FLMcNemarTest",
        #                                         pFuncArgs = c(fquote(vcorrection),
        #                                                     "a.vectorValueColumn",
        #                                                     "b.vectorValueColumn"),
        #                                         pFrom=c(a=constructSelect(x)),
        #                                         pStats=c("p_value","t_stat"))

        # vsqlstr <- paste0("SELECT FLMcNemarTest(c.FLStatistic,",fquote(vcorrection),
        #                                         ",a.vectorValueColumn,b.vectorValueColumn) AS val, \n ",
        #                             "c.FLStatistic as stat, \n ",
        #                             "count(distinct a.vectorValueColumn) as df \n ",
        #                   " FROM (",constructSelect(x),") a, \n ",
        #                         "(",constructSelect(y),") b, \n ",
        #                         "fzzlARHypTestStatsMap c \n ",
        #                   " WHERE a.vectorIndexColumn=b.vectorIndexColumn \n ",
        #                   " AND c.FLFuncName='FLMcNemarTest' ",
        #                   " GROUP BY c.FLStatistic ")

        vres <- sqlQuery(connection,vsqlstr)
        colnames(vres) <- tolower(colnames(vres))
        r <- vres[["df"]][1]

        vresList <- list(statistic=c("McNemar's chi-squared"=as.vector(vres$t_stat)),
                        parameter=c(df=r*(r-1)/2),
                        p.value=as.vector(vres$chi_sq),
                        data.name=vcall,
                        binomial_exact=c(binomial_exact = as.vector(vres$binomial_exact)))
        class(vresList) <- "htest"
        return(vresList)
    })
