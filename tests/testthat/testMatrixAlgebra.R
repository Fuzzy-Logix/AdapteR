## Hi Partha, hi Phani
##
## I added a system of select classes, and
## also added a union class for cbind.
## look at FLMatrix.R and FLMatrixBind.R.
##
## The code contains your name at places where we might want to discuss
## 
## Most is working, please investigate 
## by running from here and 
## involved functions.
##
## Also, the correlation demo is functional (demo-cor.R in this folder)
##



################################################################################
## Load AdapteR
##
## if you want to clear the workspace
##rm(list=ls())

## if you want to unload the package after making changes:
detach("package:AdapteR", unload = TRUE)

## rebuild documentation and load as source package
setwd("/Users/gregor/fuzzylogix/AdapteR/RWrappers/AdapteR")
devtools::document()
devtools::load_all(".")
## devtools::test()


################################################################################
## Connect to Gandalf
##
##################
##
## library(RODBC) 
## connection <- odbcConnect(“Gandalf”)

library(RJDBC) 

 ## add jdbc driver and security jars to classpath
.jaddClassPath("/Users/gregor/fuzzylogix/terajdbc4.jar")
.jaddClassPath("/Users/gregor/fuzzylogix/tdgssconfig.jar")
library(teradataR)


if (exists("connection")) {
    ## reconnect to database (e.g. after VPN disconnects)
    dbDisconnect(connection)
    rm(connection)
}
connection <- tdConnect(host,user,passwd,database,"jdbc")
## I need to add class path twice (recurring problem in MAC as of:
## http://forums.teradata.com/forum/analytics/connecting-to-teradata-in-r-via-the-teradatar-package
## note: wait for some time before rerunning?


########################################
## Demo of the construction of selects
## for rbind and cbind

##options(debugSQL=FALSE)
options(debugSQL=TRUE)
## This needs to be eliminated,
## legacy code setting singletons
## for now, some parts need it
FLStartSession(connection, persistent="test")

## A remote deep matrix is easily referenced by specifying 
eqnRtn <- FLMatrix(
    connection,
    database          = "FL_DEMO",
    matrix_table      = "finEquityReturns",
    matrix_id_value   = "",
    matrix_id_colname = "",
    row_id_colname    = "TxnDate",
    col_id_colname    = "TickerSymbol",
    cell_val_colname  = "EquityReturn")

## Subsetting is easy
a <- eqnRtn[2001:2010,"MSFT"]
b <- eqnRtn[2001:2010,"ORCL"]
a2 <- eqnRtn[2011:2020,"MSFT"]
b2 <- eqnRtn[2011:2020,"ORCL"]

cat(constructSelect(a,"a"))

## print the Matrix
a

##############################
## bind for matrices with character dimnames
## note: no data movement.
ab <- cbind(a,b)

cat(constructSelect(ab))

ab

## note: currently only works for unique row and col ids (dimnames)
dimnames(ab)

# fable0906 alala

require(testthat)

expect_equal(
    dim(ab),
    c(nrow(a), ncol(a)+ncol(b)))

## cbind of 2 rbinds:
a2b2 <- cbind(a2,b2)
AB <- rbind(ab, a2b2)
dimnames(AB)

cat(constructSelect(AB))

expect_equal(dim(AB),
             c(nrow(a) + nrow(a2),
               ncol(a) + ncol(b)))

AB

ABs <- store(AB)


## Partha:  do you see the reason for the sql error here?
AB %*% t(AB)


## Testing Subsetting
## Non-symmetric singular matrix of dimension 5x5
## in R memory
rMatrix <- matrix(1:25,5)
rMatrix
dim(rMatrix)
diag(rMatrix)

## converting the R matrix into 
## an in-DB object,
## CAREFUL: DATA IS TRANSFERED THROUGH NETWORK
m <- as.FLMatrix(rMatrix,connection)

dim(m)
m
rMatrix
##diag(m)




