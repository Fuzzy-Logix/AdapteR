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
if(!exists("connection")) {
    demo("connecting", package="AdapteR")
}

#############################################################
## For in-database analytics the matrix is in the warehouse
## to begin with.
sqlQuery(connection,
           "select top 10 * from FL_TRAIN.finEquityReturns")

vtemp <- readline("Above: The table has equity returns stored as triples (what was the equity return of which ticker on what date).\nThese triples define a matrix in deep format.")

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
FROM    FL_TRAIN.finEquityReturns a,
        FL_TRAIN.finEquityReturns b
WHERE   b.TxnDate = a.TxnDate
AND     a.TickerSymbol IN ('AAPL')
AND     b.TickerSymbol IN ('AAPL','HPQ','IBM',
                           'MSFT','ORCL')
GROUP BY a.TickerSymbol,
         b.TickerSymbol
ORDER BY 1, 2;")

vtemp <- readline("Above: The SQL-through R way to compute a correlation matrix with DB Lytix.")

## A remote matrix is easily created by specifying
## table, row id, column id and value columns
##
eqnRtn <- FLMatrix(table_name        = "finEquityReturns",
                   row_id_colname    = "TxnDate",
                   col_id_colname    = "TickerSymbol",
                   cell_val_colname  = "EquityReturn")

## the equity return matrix is about 3k rows and cols
dim(eqnRtn)

vtemp <- readline("Above: a remote matrix is defined.")

## 1. select the desired colums from the full matrix
sm <- eqnRtn[,c('AAPL','HPQ','IBM','MSFT','ORCL')]

vtemp <- readline("Next: the R/AdapteR way to compute a correlation matrix -- transparently in-database")

## 2. use the default R 'cor' function
flCorr <- cor(sm)
flCorr

vtemp <- readline("Next: the SQL syntax created for you")


## with this option each R command that uses DBLytix will log
## the SQL sent to Teradata.
## Such a dump can in many cases be used as a pure-sql script!
oldDebugSQL <- getOption("debugSQL")
options(debugSQL=TRUE)

## Note that no SQL is sent when defining data-sets
## 1. select the desired colums from the full matrix
sm <- eqnRtn[,c('AAPL','HPQ','IBM','MSFT','ORCL')]

## 2. use the default R 'cor' function
flCorr <- cor(sm)
vtemp <- readline("Note that SQL is not sent yet during definition")

flCorr
vtemp <- readline("Note that SQL is sent when data is printed or otherwise used")
options(debugSQL=oldDebugSQL)
## Casting methods fetch (selected) data from the warehouse into R memory
rEqnRtn <- as.matrix(eqnRtn[,c('AAPL','HPQ','IBM','MSFT','ORCL')])
rEqnRtn <- na.omit(rEqnRtn)

## the result is in the same format as the R results
rCorr <- cor(rEqnRtn, rEqnRtn)
round(rCorr,2)
round(flCorr,2)

vtemp <- readline("Note: The result is in the same format as the R results.")


########################################
## dimnames support
## sample 20 ticker columns
(randomstocks <- sample(colnames(eqnRtn), 20))

## indices of date rows in december 2016
(dec2006 <- grep("2006-12",rownames(eqnRtn)))

vtemp <- readline("Above: dimnames and index support")

eqnRtn[dec2006[1:5], c("HPQ","MSFT")]

vtemp <- readline("Above: Inspecting subsets of data in R is easy with matrix subsetting syntax")

E <- eqnRtn[dec2006, randomstocks]
vtemp <- readline("NO SQL is sent during the definition of subsetting")

print(E)
vtemp <- readline("Data is fetched on demand only, e.g. when printing")



############################################################
## And of course you can now use
## thousands of R packages to operate on DB Lytix results,
require(gplots)
## install.packages("gplots")

metaInfo <- tryCatch({
    read.csv("http://raw.githubusercontent.com/aaronpk/Foursquare-NASDAQ/master/companylist.csv")
}, error=function(e) NULL)

if(is.null(metaInfo))
    metaInfo <- tryCatch({
        read.csv("https://raw.githubusercontent.com/aaronpk/Foursquare-NASDAQ/master/companylist.csv")
    }, error=function(e) NULL)


M <- cor(eqnRtn[,intersect(
    metaInfo$Symbol[metaInfo$Sector %in% c("Basic Industries")],
    colnames(eqnRtn))])

heatmap.2(as.matrix(M),
          symm=TRUE,
          distfun=function(c) as.dist(1 - c),
          trace="none",
          col=redgreen(100),
          cexCol = 1,
          cexRow = 1)

vtemp <- readline("You can use FL results in other R packages, e.g. plotting -- or shiny (next)")

run.FLCorrelationShiny <- function (){
###########################################################
    ## Shiny Correlation Plot Demo
    ##
    ## metadata can be easily combined on the client
    ## download metadata from

    ## metadata contains sectors and industries
    ## that will be selectable in the shiny web ui
    table(metaInfo$industry)
    table(metaInfo$Sector)
    stockCorrelPlot <- function(input){
        ## get selected and available ticker symbols
        metastocks <- as.character(
            metaInfo$Symbol[
                metaInfo$industry %in% input$industries |
                metaInfo$Sector %in% input$sectors])
        stocks <- intersect(
            unique(c(input$stocks,
                     metastocks)),
            colnames(eqnRtn))

        ## compute correlation matrix
        flCorr <- as.matrix(cor(eqnRtn[,stocks]))

        ## plot with company names and stocks
        rownames(flCorr) <- metaInfo$Name[
            match(rownames(flCorr),
                  metaInfo$Symbol)]
        heatmap.2(flCorr, symm=TRUE,
                  distfun=function(c) as.dist(1 - c),
                  trace="none",
                  col=redgreen(100),
                  cexCol = 1, srtCol=90,
                  cexRow = 1)
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
            stockCorrelPlot(input), height=800)
    }
    )
}

assign("metaInfo",metaInfo,envir=environment(run.FLCorrelationShiny))

vtemp <- readline("To explore correlations interactively, we defined a function above. \n Simply execute\n> run.FLCorrelationShiny()\nafter ending the Demo.\nEnd the demo now:")

### END ####
### Thank You ####
