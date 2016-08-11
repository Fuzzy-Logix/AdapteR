## Download R and R-Studio.
## For R:  https://cran.r-project.org/
## For R-Studio: https://www.rstudio.com/products/rstudio/download/
## Install R. While Installing, check 32/64 bit according to system.
## Install R Studio.
## Add R and Rcript executables to PATH variable.
## Open Shell and Run:
    # Rscipt yourPathToThisFile
## Or Alternatively, open and run this file in R-Studio.


install.packages("roxygen2") ## Required for devtools
install.packages("devtools") ## Required for install_github
install.packages("testthat") ## Required to run test suite
install.packages("RODBC") ## Required for ODBC Connection.Need to setup odbc Source
install.packages("rJava") ## Required for RJDBC
install.packages("RJDBC") ## Required for JDBC connection
library(RODBC)
library(devtools)
library(RJDBC)

## if this throws JavaSetHome error(In Windows usually),
    # try skipping this step and proceeding to see 
    # if demoScript runs successfully.
    # Else come back and jdk needs to be installed,path variable setup
    # and Sys.setenv(JAVA_HOME="yourPathTojdk")  
    # --may need help.Then proceed forward.

remove.packages("AdapteR") ## Remove older Version

## If installing from .tar.gz:
    # adptrdir <- "C:/Users/phani/Desktop/AdapteR_2.0.tar.gz" ##where the adapteR source file(tar.gz) is.
    # install.packages(adptrdir,repos = NULL,type = "source")
install_github("Fuzzy-Logix/AdapteR") ## Will also install dependencies
library(AdapteR)
