library(testthat)
library(AdapteR)

runMyTestFile <- function(f){
    cat(paste0("testing ",f,"\n"))
    if(readline("start (y or n)")=="y")
        tryCatch({
            options(debugSQL=FALSE)
            test_file(f)
        },
        error=function(e) print(e))
}

llply(find_test_scripts("AdapteR/tests/testsuite"), runMyTestFile)


llply(find_test_scripts("AdapteR/tests/limitations"), runMyTestFile)

