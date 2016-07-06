library(testthat)
library(AdapteR)

##' runs a test file.
##'
##' @param f the test file to run
##' @param ask if TRUE, will ask for each file
##' @param skip a regexp on the filename whether the test should be skipped
##' @param ... passed on to test_file
##' @return the test_file report
runMyTestFile <- function(f, ask=TRUE, skip=NULL,...){
    if(!is.null(skip))
        if(grepl(skip,f)) {
            cat(paste0("skipped ",f,"\n"))
            return(data.frame())
        }
    cat(paste0("testing ",f,"\n"))
    if(ask)
        run <- readline("start (y?)")=="y"
    else
        run <- TRUE
    if(run)
        tryCatch({
            options(debugSQL=FALSE)
            test_file(f,...)
        },
        error=function(e) print(e))
}

Results <- llply(
    find_test_scripts("AdapteR/tests/testsuite"),
    runMyTestFile,
    ask=FALSE,
    skip="agnes|clustering|chol|cor|det|diag|dim|hclust|ginv|head|icMean|_hkmeans|_kmeans|length|_log|_lu.R|_mean|_MatrixDistance|multiplication|_median|_mode|_operators|_pam|_percent|_qr.R|_sd.R|_rowcolsum|_sort|_solve|_store|_rankMatrix|_rank.R|subset|_sum.R|_tr.R|_trace|_transpose|_var|_WeightedMean|_eigen|_cov.R|fanny|identical|quantile|MatrixRepr")


llply(find_test_scripts("AdapteR/tests/limitations"),
      runMyTestFile)
