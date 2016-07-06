library(testthat)
library(AdapteR)

runMyTestFile <- function(f, ask=TRUE){
    cat(paste0("testing ",f,"\n"))
    if(ask)
        run <- readline("start (y or n)")=="y"
    else
        run <- TRUE
    if(run)
        tryCatch({
            options(debugSQL=FALSE)
            test_file(f)
        },
        error=function(e) print(e))
}

llply(find_test_scripts("AdapteR/tests/testsuite"), runMyTestFile, ask=FALSE)


llply(find_test_scripts("AdapteR/tests/limitations"), runMyTestFile)
