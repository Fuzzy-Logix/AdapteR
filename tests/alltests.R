library(testthat)
library(AdapteR)

llply(find_test_scripts("AdapteR/tests/testsuite"),
      function(f){
        cat(paste0("testing ",f,"\n"))
        tryCatch({
          options(debugSQL=FALSE)
          test_file(f)
        },
        error=function(e) print(e))
      })


llply(find_test_scripts("AdapteR/tests/limitations"),
      function(f){
        cat(paste0("testing ",f,"\n"))
        tryCatch({
          options(debugSQL=FALSE)
          test_file(f)
        },
        error=function(e) print(e))
      })

