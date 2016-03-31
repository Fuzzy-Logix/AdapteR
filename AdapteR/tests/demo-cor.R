## Fuzzy Logix DB Lytix(TM)
## is a high-speed in-database analytics library
## written in C++, exposing ~700 functions through SQL.
## SQL as low-level language makes analyses
## consumable from all SQL-enabled clients.


## This demo shows how the 
## AdapteR package of Fuzzy Logix is
## easing interaction with the DB Lytix(TM) in-database
## library.
##
## The demo highlights how to build an
## interactive stock returns correlation demo
## computed in database!


## Setting up a connection can be either done with
## ODBC or JDBC
## Setting up a ODBC connection
if(!exists("connection"))
    connection <- flConnect(odbcSource = "Gandalf")


## If ODBC has failed we try to create a JDBC connection
if(!exists("connection")){
    yourJarDir <- "/Users/gregor/fuzzylogix/Teradata/jdbc"
    connection <-
        flConnect(
            host     = "10.200.4.116",
            database = "Fl_demo",
            user = yourUser,
            passwd = yourPassword,
            ## set dir.jdbcjars to add jdbc driver
            ## and security jars to classpath:
            ##    terajdbc4.jar tdgssconfig.jar
            ## CAVE: fully qualified PATH required
            dir.jdbcjars = yourJarDir)
}



#############################################################
## For in-database analytics the matrix is in the warehouse
## to begin with.
sqlQuery(connection,
           "select top 10 * from FL_DEMO.finEquityReturns")




###########################################################
## Correlation Matrix
## The SQL-through R way to compute a
## correlation matrix with DB Lytix:
##
sqlQuery(connection, "
SELECT  a.TickerSymbol           AS Ticker1,
        b.TickerSymbol           AS Ticker2,
        FLCorrel(a.EquityReturn,
                 b.EquityReturn) AS FLCorrel
FROM    finEquityReturns a,
        finEquityReturns b
WHERE   b.TxnDate = a.TxnDate
AND     a.TickerSymbol = 'MSFT'
AND     b.TickerSymbol IN ('AAPL','HPQ','IBM',
                           'MSFT','ORCL')
GROUP BY a.TickerSymbol,
         b.TickerSymbol
ORDER BY 1, 2;")



## A remote matrix is easily created by specifying
## table, row id, column id and value columns
##
eqnRtn <- FLMatrix(database          = "FL_DEMO",
                   table_name        = "finEquityReturns",
                   matrix_id_value   = "",
                   matrix_id_colname = "",
                   row_id_colname    = "TxnDate",
                   col_id_colname    = "TickerSymbol",
                   cell_val_colname  = "EquityReturn")

## this is a medium large matrix
dim(eqnRtn)


########################################
## dimnames support
## sample 20 ticker columns
(randomstocks <- sample(colnames(eqnRtn), 20))

## indices of date rows in december 2016
(dec2006 <- grep("2006-12",rownames(eqnRtn)))


## Inspecting subsets of data in R
## is easy with matrix subsetting syntax:
eqnRtn[dec2006, "MSFT"]


## #####################
## SQL construction
## with this option each R command that uses DBLytix will log
## the SQL sent to Teradata.
## This can in many cases be migrated to a SQL script.
options(debugSQL=FALSE)
options(debugSQL=TRUE)

## NO SQL is sent during the definition of subsetting
E <- eqnRtn[dec2006, randomstocks]

## Data is fetched on demand only, e.g. when printing
print(E)



## The AdapteR way to compute a correlation matrix
## from a matrix with correlated random variables in columns:
## (transparently creating a SQL query a la Manual):
##
flCorr <- cor(eqnRtn[,c('AAPL','HPQ','IBM','MSFT','ORCL')],
              eqnRtn[,c('AAPL','HPQ','IBM','MSFT','ORCL')])

round(as.matrix(flCorr),2)

## the result is in the same format as the R results
rEqnRtn <- as.matrix(eqnRtn[,c('AAPL','HPQ','IBM','MSFT','ORCL')])
rEqnRtn <- na.omit(rEqnRtn)

rCorr <- cor(rEqnRtn, rEqnRtn)
round(rCorr,2)



############################################################
## And of course you can now use  
## thousands of R packages to operate on DB Lytix results, 
require(gplots)
## install.packages("gplots")

M <- cor(eqnRtn[,c('AAPL','HPQ','IBM','MSFT','ORCL')])
heatmap.2(as.matrix(M),
          symm=TRUE, 
          distfun=function(c) as.dist(1 - c),
          trace="none",
          col=redgreen(100),
          cexCol = 1,
          cexRow = 1)


###########################################################
## Shiny Correlation Plot Demo
##
## metadata can be easily combined on the client
## download metadata from
## https://raw.githubusercontent.com/aaronpk/Foursquare-NASDAQ/master/companylist.csv
metaInfo <- read.csv(
    "/Users/gregor/Downloads/companylist.csv")

## metadata contains sectors and industries
## that will be selectable in the shiny web ui
table(metaInfo$industry)
table(metaInfo$Sector)

stockCorrelPlot <- function(input){
    metastocks <- as.character(
        metaInfo$Symbol[
            metaInfo$industry %in% input$industries |
            metaInfo$Sector %in% input$sectors])
    stocks <- intersect(
        unique(c(input$stocks,
                 metastocks)),
        colnames(eqnRtn))
    if(length(stocks)>100)
        stocks <- sample(stocks,100)
    ##browser()
    withTimeout({
        flCorr <- as.matrix(cor(eqnRtn[,stocks]))
        rownames(flCorr) <- metaInfo$Name[
            match(rownames(flCorr),
                  metaInfo$Symbol)]
        heatmap.2(flCorr, symm=TRUE, 
                  distfun=function(c) as.dist(1 - c),
                  trace="none",
                  col=redgreen(100),
                  cexCol = 1,
                  cexRow = 1)
    }, timeout = 40)
}

require(R.utils)
require(shiny)
shinyApp(
    ui = fluidPage(
        fluidRow(
            column(3,
                   selectInput(
                       "sectors", "Sectors:", 
                       choices = levels(metaInfo$Sector),
                       selected = "Energy",
                       multiple = TRUE)),
            column(3,
                   selectInput(
                       "industries", "Industries:", 
                       choices = levels(metaInfo$industry),
                       selected = "Commercial Banks",
                       multiple = TRUE)),
            column(6,
                   selectInput(
                       "stocks", "Stocks:", 
                       choices = colnames(eqnRtn),
                       selected = c(),
                       multiple = TRUE))),
        fluidRow(plotOutput("correlations"))
    ),
    server = function(input, output) {
        output$correlations <- renderPlot(
          stockCorrelPlot(input), height=1200)
    }
)


###########################################################
##
## Matrix Inversion
##
## The SQL-through R way a la Manual:
sqlQuery(connection, "
WITH z (Matrix_ID, Row_ID, Col_ID, NumVal) AS
(
SELECT a.Matrix_ID,
       a.Row_ID,
       a.Col_ID,
       a.Cell_Val
FROM fl_dev.tblMatrixMulti a
WHERE a.Matrix_ID = 5
)
SELECT a.*
FROM TABLE (
     FLMatrixInvUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.NumVal) HASH BY z.Matrix_ID
  LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID
) AS a
ORDER BY 1,2,3;")

m <- FLMatrix(
              database          = "FL_DEMO",
              table_name        = "tblMatrixMulti",
              matrix_id_colname = "Matrix_ID",
              matrix_id_value   = "5",
              row_id_colname    = "Row_ID",
              col_id_colname    = "Col_ID",
              cell_val_colname  = "Cell_Val",
              dimnames=list(c("a","b","c","d","e"),
                            c("p","q","r","s","t"))
)

## compute the inverse
ms <- solve(m)
ms

## check is R and DB Lytix results match up:
m.r <- as.matrix(m) ## download and convert to R matrix
expect_equal(as.matrix(ms),
             solve(m.r),check.attributes=FALSE)

## Matrix multiplication with inverse results in identity
round(as.matrix(m %*% ms))




options(debugSQL=TRUE)
flM <- as.FLMatrix(matrix(runif(25),5))

flM

flM + flM

flM - flM

flM / flM

(flM %*% flM) - flM

solve(flM) %*% flM - flM







gregor.kappler@fuzzylogix.com

Thanks!
