
require(RJDBC)
## if you want to clear the workspace
rm(list=ls())

## if you want to unload the package after making changes:
detach("package:AdapteR", unload = TRUE)

## rebuild documentation and load as source package
setwd("/Users/gregor/fuzzylogix/AdapteR/RWrappers/AdapteR")
devtools::document()
devtools::load_all(".")
## devtools::test()

require(test_that)
FLStartSession(connection)
