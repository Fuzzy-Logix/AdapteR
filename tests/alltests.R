library(testthat)
library(AdapteR)

##' runs a test file.
##'
##' @param f the test file to run
##' @param ask if TRUE, will ask for each file
##' @param skip a regexp on the filename whether the test should be skipped
##' @param ... passed on to test_file
##' @return the test_file report
runMyTestFile <- function(f, ask=FALSE, skip=NULL,...){
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

drop3ok <- "stringdist|t_r$|_trace$|_transpose|_var$|MatrixRepres|WeightedMean|clustering|"
results <- list()
results$testsuite <- llply(
    find_test_scripts("AdapteR/tests/testsuite"),
    runMyTestFile,
    ask=FALSE)

results$testthat <- llply(
    find_test_scripts("AdapteR/tests/testthat"),
    runMyTestFile,
    ask=FALSE)

results$limitations <- llply(find_test_scripts("AdapteR/tests/limitations"),
      runMyTestFile)

