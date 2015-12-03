FLStartSession(connection)
options(debugSQL=TRUE)


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
m <- eqnRtn <- FLMatrix(connection,
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


E <- eqnRtn[dec2006,
            randomstocks]
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

require(gplots)
require(corrplot)
corrplot(cor(rEqnRtn))

corrplot(as.matrix(flCorr))

randomstocks <- sample(colnames(eqnRtn),
                       100)


flCorr <- cor(eqnRtn[,randomstocks])

corrplot(as.matrix(flCorr),
         method="color",
         outline = FALSE,
         order = "hclust",
         hclust.method = "ward",
         addrect = 10,
         tl.col="black",tl.cex = .5)


ord <- corrMatOrder(flCorr,
                    order = "hclust",
                    hclust.method = "ward")
flCorr <- flCorr[ord, ord]


library(seriation) ## for pimage and hmap
pimage(as.matrix(flCorr))

hmap(as.matrix(flCorr))


## heatmap with correlation-based distance, green-red color (greenred is 
## predefined) and optimal leaf ordering and no row label
dist_cor <- function(x) as.dist(1-cor(t(x)))

     

hmap(as.matrix(flCorr),
     method="OLO", distfun = dist_cor,
     col=greenred(100),
     zlim=c(-1,1),
     labRow=FALSE)

     ## order-based heatmap
     hmap(as.matrix(flCorr), method="MDS_angle", distfun = dist_cor, col=greenred(100))  
     
     ## order-based without dissimilarity matrices
     hmap(as.matrix(flCorr), method="MDS_angle", distfun = dist_cor, showdist = FALSE, 
       col=greenred(100))  


metaInfo <- read.csv("~/Downloads/companylist.csv")
table(metaInfo$industry)
table(metaInfo$Sector)



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



image(as.matrix(flCorr))

## Selection constructs dynamic where clauses
constructWhere(constraintsSQL(E))
## dynamic where clauses support local names
constructWhere(constraintsSQL(E,"a"))



##################################################################
## Let us look at memory consumption:
##
Nstocks <- 10
subEqnRtn <- eqnRtn[, sample(colnames(eqnRtn),Nstocks)]

## only dimension names are in local memory:
print(object.size(eqnRtn),units = "Kb")
print(object.size(eqnRtn@dimnames),units = "Kb")
print(object.size(subEqnRtn),units = "Kb")
print(object.size(subEqnRtn@dimnames),units = "Kb")

## Download a subset of the remote Table into R Memory
rEqnRtn <- as.matrix(subEqnRtn)

## compare memory consumption:
print(object.size(rEqnRtn),units = "Kb")









## many functions provide important
## functions on matrices
dim(m)


solve(m)


## multiplication
2*m[2:5,4:5]

## matrix multiplication
m[2:5,4:5] %*% m[4:5,2:5]
