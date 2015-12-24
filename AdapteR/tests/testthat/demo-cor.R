##################################################################
## Starting a session
##
## loading AdapteR
setwd("/Users/gregor/fuzzylogix/AdapteR/RWrappers/AdapteR")
devtools::load_all(".")

## Unit tests for all functions
require(testthat)
## devtools::test()
devtools::document()


## ODBJ and JDBC is supported
## here we use jdbc
library(RJDBC) 
## the jdbc driver from teradata developer network
## add jdbc driver and security jars to classpath
.jaddClassPath("/Users/gregor/fuzzylogix/terajdbc4.jar")
.jaddClassPath("/Users/gregor/fuzzylogix/tdgssconfig.jar")
library(teradataR)

connection <- tdConnect("10.200.4.116",
                        "gkappler",
                        yourPassword,
                        database="FL_DEMO",
                        dType="jdbc")
## need to add class path twice (recurring problem in MAC as of:
## http://forums.teradata.com/forum/analytics/connecting-to-teradata-in-r-via-the-teradatar-package
## note: wait for some time before rerunning?
ls()


options(debugSQL=FALSE)
FLStartSession(connection)


## a in-memory matrix in R 
(m <- rMatrix <- matrix(1:25,5))

#####################################################################
## R has very nice vector and matrix syntax

## Subsetting
## a row
(m1 <- m[1,])

## Subsetting
## a column
m[,1]

## Subsetting
## a part of a matrix
m[2:5,4:5]





##################################################################
## converting the R matrix into an in-DB object
## (data is transfered through network)
## (and refetched for printing)
##
(m <- flMatrix <- as.FLMatrix(rMatrix,
                              connection))


## you can run above functions on m=flMatrix again!









#############################################################
## For in-database analytix the matrix is in the warehouse
## to begin with.
## 
## A remote matrix is easily created by specifying
##
m <- 
 eqnRtn <- FLMatrix(
         connection,
         database          = "FL_DEMO",
         matrix_table      = "finEquityReturns",
         matrix_id_value   = "",
         matrix_id_colname = "",
         row_id_colname    = "TxnDate",
         col_id_colname    = "TickerSymbol",
         cell_val_colname  = "EquityReturn")


## you can run above functions on m=Equity Returns Example again!

## this is a rather large matrix
dim(eqnRtn)

## with ticker columns and date rows
(randomstocks <- sample(colnames(eqnRtn), 20))

head(dates <- rownames(eqnRtn))

## Inspecting Data is compressed in R with matrix syntax:
dec2006 <- grep("2006-12",dates)
eqnRtn[dec2006, "MSFT"]


E <- eqnRtn[dec2006, randomstocks]
E




###########################################################
## Correlation Matrix
## The SQL-through R way to a la Manual:
dbGetQuery(connection, "
SELECT a.TickerSymbol           AS Ticker1,
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


#################################################################
## The AdapteR way to compute a correlation matrix
## from a matrix with correlated random variables in columns:
## (transparently creating a SQL query a la Manual):
randomstocks <- c('AAPL','HPQ','IBM','MSFT','ORCL')

rEqnRtn <- as.matrix(eqnRtn[,randomstocks])
rEqnRtn <- na.omit(rEqnRtn)

rCorr <- cor(
    rEqnRtn[,c('AAPL','MSFT')],
    rEqnRtn[,c('AAPL','HPQ','IBM','MSFT','ORCL')])
round(rCorr,2)




flCorr <- cor(
    eqnRtn[,c('AAPL','MSFT')],
    eqnRtn[,randomstocks])
round(flCorr,2)


options(debugSQL = TRUE)
M <- cor(eqnRtn[,randomstocks])
M
## And of course you can now use the full power of
## tens of thousands of R packages, e.g.
##
require(gplots)
heatmap.2(as.matrix(M),
          symm=TRUE, 
          distfun=function(c) as.dist(1 - c),
          trace="none",
          col=redgreen(100),
          cexCol = 1,
          cexRow = 1)

## From this we will see an interactive "shiny" demo


###########################################################
##
## -- but first a look under the hood:
##
## memory consumption
##
Nstocks <- 100
subEqnRtn <- eqnRtn[, sample(colnames(eqnRtn),Nstocks)]
dim(eqnRtn)
dim(subEqnRtn)

## only dimension names are in local memory:
print(object.size(eqnRtn),units = "Kb")
print(object.size(eqnRtn@dimnames),units = "Kb")
print(object.size(subEqnRtn),units = "Kb")
print(object.size(subEqnRtn@dimnames),units = "Kb")

## Download a subset of the remote Table into R Memory
## rEqnRtn <- as.matrix(subEqnRtn)

## compare memory consumption:
## print(object.size(rEqnRtn),units = "Kb")


##
## SQL construction
## with this option each R command that uses DBLytix will log
## the SQL sent to Teradata.
## Such a dump can in many cases be used as a pure-sql script!
options(debugSQL=TRUE)

## Try some commands above 

## BTW:
E <- subEqnRtn
## where clauses are dynamically constructed
constructWhere(constraintsSQL(E))
## dynamic where clauses support local names
## so that SQL can be constructed flexibly
constructWhere(constraintsSQL(E,"a"))


###########################################################
## Finally the
## Shiny Demo
##
## metadata can be quickly combined on the client
##
metaInfo <- read.csv("companylist.csv")
table(metaInfo$industry)
table(metaInfo$Sector)

## BTW: there is more demo after shiny: matrix inversion and multiplication
randomstocks <- c('AAPL','HPQ','IBM','MSFT','ORCL')
require(R.utils)
require(shiny)
shinyApp(
    ui = fluidPage(
        fluidRow(
            column(3,
                   selectInput("sectors", "Sectors:", 
                               choices = levels(metaInfo$Sector),
                               selected = "Energy",
                               multiple = TRUE),
                   selectInput("industries", "Industries:", 
                               choices = levels(metaInfo$industry),
                               selected = "Commercial Banks",
                               multiple = TRUE),
                   selectInput("stocks", "Stocks:", 
                               choices = colnames(eqnRtn),
                               selected = randomstocks,
                               multiple = TRUE)),
            column(9,
                   plotOutput("correlations"))
    )),
    server = function(input, output) {
        output$correlations <- renderPlot({
            stocks <- intersect(
                unique(c(input$stocks,
                         as.character(metaInfo$Symbol
                                      [metaInfo$industry %in% input$industries |                                       metaInfo$Sector %in% input$sectors]))),
                colnames(eqnRtn))
            if(length(stocks)>100)
                stocks <- sample(stocks,100)
            ##browser()
            withTimeout({
                flCorr <- as.matrix(cor(eqnRtn[,stocks]))
                rownames(flCorr) <- metaInfo$Name[match(rownames(flCorr),
                                                        metaInfo$Symbol)]
                heatmap.2(flCorr, symm=TRUE, 
                          distfun=function(c) as.dist(1 - c),
                          trace="none",
                          col=redgreen(100),
                          cexCol = 1,
                          cexRow = 1)
                ## hmap(flCorr,
                ##      method="OLO", distfun = dist_cor,
                ##      col=greenred(100), labRow=FALSE)
            }, timeout = 10)
        }, height=1200)
    }
)


###########################################################
##
## Matrix Inversion
##
## The SQL-through R way a la Manual:
dbGetQuery(connection, "
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
FLMatrixInvUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.NumVal)
HASH BY z.Matrix_ID
LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID
) AS a
ORDER BY 1,2,3;")

m <- FLMatrix(connection,
              database          = "FL_DEMO",
              matrix_table      = "tblMatrixMulti",
              matrix_id_colname = "Matrix_ID",
              matrix_id_value   = "5",
              row_id_colname    = "Row_ID",
              col_id_colname    = "Col_ID",
              cell_val_colname  = "Cell_Val")
dim(m)

constructSelect(m)

m.r <- as.matrix(m)

ms <- solve(m)
dim(ms)
#ms
#m.r

expect_equal(as.matrix(ms), solve(m.r))

## gk: todo: do not fetch names
m %*% ms



## lm(y ~ x + x2 + x3, data=D)
## lm("y ~ x + x2 + x3", data=D)

## formula <- "y ~ x + x2 + x3"
## formula <- prepare(data=D,DV=y)
## lm(formula, data=D)

## many functions provide important
## functions on matrices

## multiplication
2*m[2:5,4:5]

## matrix multiplication
m[2:5,4:5] %*% m[4:5,2:5]
