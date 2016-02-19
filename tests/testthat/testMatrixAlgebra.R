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

library(RJDBC) 
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


require(testthat)


## A remote deep matrix is easily referenced by specifying 
eqnRtn <- FLMatrix(
    connection,
    database          = "FL_DEMO",
    table_name = "finEquityReturns",
    matrix_id_value   = "",
    matrix_id_colname = "",
    row_id_colname    = "TxnDate",
    col_id_colname    = "TickerSymbol",
    cell_val_colname  = "EquityReturn")

test_that("Named matrix rows and columns",{

    a <- eqnRtn[2001:2010,"MSFT"]
    b <- eqnRtn[2001:2010,"ORCL"]
    a2 <- eqnRtn[2011:2020,"MSFT"]
    b2 <- eqnRtn[2011:2020,"ORCL"]
    
    cat(constructSelect(a))


##############################
    ## bind for matrices with character dimnames
    ## note: no data movement.
    ab <- cbind(a,b)
    
    cat(constructSelect(ab))

    ab

    ## note: currently only works for unique row and col ids (dimnames)
    dimnames(ab)


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
    ##ABs <- store(AB)
})

test_that("Named matrix rows and columns",{
    
    cat(constructSelect(a,"a"))


##############################
    ## bind for matrices with character dimnames
    ## note: no data movement.
    ab <- cbind(a,b)
    
    cat(constructSelect(ab))

    ab

    ## note: currently only works for unique row and col ids (dimnames)
    dimnames(ab)


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
    ##ABs <- store(AB)
})


## dimnames mapping

###############################################################
############# POSITIVE TEST CASES #############################
###############################################################
## gk: todo: fix rownames
# test_that("Casting base R matrix <---> in-database Matrices",{
    ## Creating simple base R matrix


matrix1 <- matrix(1:25,5)
matrix2 <- matrix(1:25,5)
rownames(matrix1) <- c("a","b","c","d","e")
colnames(matrix1) <- c("p","q","r","s","t")
rownames(matrix2) <- c("A","B","C","D","E")
colnames(matrix2) <- c("P","Q","R","S","T")

##  FLMatrices from R matrices
m1 <- as.FLMatrix(matrix1, connection)

m1


    m2 <- as.FLMatrix(matrix2, connection)
    expect_equal(dim(m1),c(5,5))
    expect_equal(dim(m2),c(5,5))
    expect_equal(as.vector(m1), as.vector(matrix1))
    expect_equal(as.vector(m2), as.vector(matrix2))
    ##
    ##
    ## FLMatrix -> R matrix
    matrix3 <- as.matrix(m2)
    expect_equal(dim(matrix3),
                 c(5,5))
    expect_equal(as.vector(m2),
                 as.vector(matrix3))
    if(!ignoreDimNames)
    test_that({
        expect_equal(rownames(m2),
                     rownames(matrix3))
        expect_equal(colnames(m2),
                     colnames(matrix3))
        expect_equal(rownames(m1),
                     rownames(matrix1))
        expect_equal(colnames(m1),
                     colnames(matrix1))
        expect_equal(rownames(m2),
                     rownames(matrix2))
        expect_equal(colnames(m2),
                     colnames(matrix2))
    })
})


dbGetQuery(connection,"
WITH z (Matrix_ID, Row_ID, Col_ID, NumVal) AS
(
SELECT a.Matrix_ID,
a.Row_ID,
a.Col_ID,
a.Cell_Val
FROM tblMatrixMulti a
WHERE a.Matrix_ID = 5
)
WITH z2 (Matrix_ID, Row_ID, Col_ID, NumVal) AS
(
SELECT a.*
FROM TABLE (
FLMatrixInvUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.NumVal)
HASH BY z.Matrix_ID
LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID
) AS a
)
SELECT a2.*
FROM TABLE (
FLMatrixInvUdt(z2.Matrix_ID, z2.Row_ID, z2.Col_ID, z2.NumVal)
HASH BY z2.Matrix_ID
LOCAL ORDER BY z2.Matrix_ID, z2.Row_ID, z2.Col_ID
) AS a2
ORDER BY 1,2,3;
")

dbGetQuery(connection,"WITH z (Matrix_ID, Row_ID, Col_ID, NumVal) AS
(
SELECT
100,
b.Row_ID,
b.Col_ID,
a.Cell_Val + b.Cell_Val * .5
FROM tblMatrixMulti a, tblMatrixMulti b
WHERE a.Matrix_ID = 5
AND b.Matrix_ID = 5
and a.Row_ID=b.Row_ID
AND a.Col_ID=b.Col_ID
)
SELECT a.*
FROM TABLE (
FLMatrixInvUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.NumVal)
LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID
) AS a
ORDER BY 1,2,3;
")

a

SELECT a.Matrix_ID,
a.Row_ID,
a.Col_ID,
a.Cell_Val
FROM tblMatrixMulti a
WHERE a.Matrix_ID = 5

b

SELECT b.Matrix_ID,
b.Row_ID,
b.Col_ID,
b.Cell_Val
FROM tblMatrixMulti b
WHERE b.Matrix_ID = 5


a + b * .5


dbGetQuery(connection,"SELECT top 10 * from fzzlRegrDataPrepMap")
dbSendUpdate(connection," INSERT INTO FL_DEMO.tblMatrixMultiResult_test
SELECT
 0,
 TxnDate,
 TickerSymbol,
 EquityReturn
FROM FL_DEMO.finEquityReturns
WHERE (TxnDate IN ('2007-11-01', '2007-11-02', '2007-11-05', '2007-11-06', '2007-11-07', '2007-11-08', '2007-11-09', '2007-11-12', '2007-11-13', '2007-11-14')) AND
 (TickerSymbol IN ('MSFT'))
UNION ALL
SELECT
 0,
 TxnDate,
 TickerSymbol,
 EquityReturn
FROM FL_DEMO.finEquityReturns
WHERE (TxnDate IN ('2007-11-01', '2007-11-02', '2007-11-05', '2007-11-06', '2007-11-07', '2007-11-08', '2007-11-09', '2007-11-12', '2007-11-13', '2007-11-14')) AND
 (TickerSymbol IN ('ORCL'))
UNION ALL
SELECT
 0,
 TxnDate,
 TickerSymbol,
 EquityReturn
FROM FL_DEMO.finEquityReturns
WHERE (TxnDate IN ('2007-11-15', '2007-11-16', '2007-11-19', '2007-11-20', '2007-11-21', '2007-11-22', '2007-11-23', '2007-11-26', '2007-11-27', '2007-11-28')) AND
 (TickerSymbol IN ('MSFT'))
UNION ALL
SELECT
 0,
 TxnDate,
 TickerSymbol,
 EquityReturn
FROM FL_DEMO.finEquityReturns
WHERE (TxnDate IN ('2007-11-15', '2007-11-16', '2007-11-19', '2007-11-20', '2007-11-21', '2007-11-22', '2007-11-23', '2007-11-26', '2007-11-27', '2007-11-28')) AND
 (TickerSymbol IN ('ORCL'))
")


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




