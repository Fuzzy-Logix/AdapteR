
## This script is for demoing the AdapteR Matrix capabilities
## startup and setup:
source("./setup-jdbc.R")

#############################################################
## For in-database analytix the matrix is in the warehouse
## to begin with.
dbGetQuery(connection,
           "select top 10 * from FL_DEMO.finEquityReturns")
## 
## A remote matrix is easily created by specifying
##
eqnRtn <- FLMatrix(connection,
                   database          = "FL_DEMO",
                   table_name  = "finEquityReturns",
                   matrix_id_value   = "",
                   matrix_id_colname = "",
                   row_id_colname    = "TxnDate",
                   col_id_colname    = "TickerSymbol",
                   cell_val_colname  = "EquityReturn")

## this is a rather large matrix
dim(eqnRtn)


## you can run above functions on m=Equity Returns Example again!

## with ticker columns and date rows
(randomstocks <- sample(colnames(eqnRtn), 20))

head(dates <- rownames(eqnRtn))

## Inspecting subsets of data in R with matrix subsetting syntax:
dec2006 <- grep("2006-12",dates)
eqnRtn[dec2006, "MSFT"]


## NO SQL is sent during the definition of subsetting
E <- eqnRtn[dec2006, randomstocks]

## Data is fetched on demand only, e.g. when printing
print(E)




###########################################################
## Correlation Matrix
## The SQL-through R way to compute a correlation matrix with DB Lytix:
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
flCorr <- cor(eqnRtn[,c('AAPL','MSFT')],
              eqnRtn[,c('AAPL','HPQ','IBM','MSFT','ORCL')])

round(as.matrix(flCorr),2)

## the result is in the same format as the R results
{
    rEqnRtn <- as.matrix(eqnRtn[,c('AAPL','HPQ','IBM','MSFT','ORCL')])
    rEqnRtn <- na.omit(rEqnRtn)

    rCorr <- cor(rEqnRtn[,c('AAPL','MSFT')],
                 rEqnRtn)
    round(rCorr,2)
}


## And of course you can now use  
## thousands of R packages to operate on DB Lytix results, e.g. 
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
metaInfo <- read.csv("/Users/gregor/Downloads/companylist.csv")

## metadata contains sectors and industries
## that will be selectable in the shiny web ui
table(metaInfo$industry)
table(metaInfo$Sector)

require(R.utils)
require(shiny)
shinyApp(
    ui = fluidPage(
        fluidRow(
            column(3,
                   selectInput("sectors", "Sectors:", 
                               choices = levels(metaInfo$Sector),
                               selected = "Energy",
                               multiple = TRUE)),
            column(3,
                   selectInput("industries", "Industries:", 
                               choices = levels(metaInfo$industry),
                               selected = "Commercial Banks",
                               multiple = TRUE)),
            column(6,
                   selectInput("stocks", "Stocks:", 
                               choices = colnames(eqnRtn),
                               selected = randomstocks,
                               multiple = TRUE))),
        fluidRow(plotOutput("correlations"))
    ),
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
                colnames(flCorr) <- metaInfo$Name[match(rownames(flCorr),
                                                        metaInfo$Symbol)]
                heatmap.2(flCorr, symm=TRUE, 
                          distfun=function(c) as.dist(1 - c),
                          trace="none",
                          col=redgreen(100),
                          cexCol = 1,
                          cexRow = 1)
            }, timeout = 40)
        }, height=1200)
    }
)



##
## SQL construction
## with this option each R command that uses DBLytix will log
## the SQL sent to Teradata.
## Such a dump can in many cases be used as a pure-sql script!
options(debugSQL=TRUE)
## Try some commands above 

###########################################################
##
## A look under the hood:
##
## memory consumption
##
Nstocks <- 100
subEqnRtn <- eqnRtn[, sample(colnames(eqnRtn),Nstocks)]
rEqnRtn <- as.matrix(subEqnRtn)
dim(eqnRtn)
dim(subEqnRtn)

## only dimension names are in local memory:
cat(paste0("Total client memory size for remote equity return table\n"))
print(object.size(eqnRtn),units = "Kb")
cat(paste0("dimnames client memory size for remote equity return table\n"))
print(object.size(eqnRtn@dimnames),units = "Kb")
cat(paste0("total client memory size for subset of remote equity return table\n"))
print(object.size(subEqnRtn),units = "Kb")
cat(paste0("dimnames client memory size for subset of remote equity return table\n"))
print(object.size(subEqnRtn@dimnames),units = "Kb")

## Download a subset of the remote Table into R Memory
## rEqnRtn <- as.matrix(subEqnRtn)

## compare memory consumption:
cat(paste0("dimnames client memory size for r matrix with subset of equity return table\n"))
print(object.size(rEqnRtn),units = "Kb")




## BTW:
E <- subEqnRtn
## where clauses are dynamically constructed
cat(constructWhere(constraintsSQL(E)))
## dynamic where clauses support local names
## so that SQL can be constructed flexibly



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
              table_name= "tblMatrixMulti",
              matrix_id_colname = "Matrix_ID",
              matrix_id_value   = "5",
              row_id_colname    = "Row_ID",
              col_id_colname    = "Col_ID",
              cell_val_colname  = "Cell_Val",
              dimnames=list(c("a","b","c","d","e"),
                            c("p","q","r","s","t"))
)

m

ms <- solve(m)

ms

as.matrix(m)
solve(as.matrix(m))


cat(constructSelect(m@mapSelect))

## compute the inverse
ms <- solve(m)
rm <- as.matrix(m)
rownames(rm) <- c("a","b","c","d","e")
colnames(rm) <- c("p","q","r","s","t")


## check is R and DB Lytix results match up:
m.r <- as.matrix(m) ## download and convert to R matrix
expect_equal(as.matrix(ms), solve(m.r))

## Matrix multiplication witrh inverse results in identity
round(as.matrix(m %*% ms))





## Roadmap:
## lm(y ~ x + x2 + x3, data=D)
## lm("y ~ x + x2 + x3", data=D)

## formula <- "y ~ x + x2 + x3"
## formula <- prepare(data=D,DV=y)
## lm(formula, data=D)
