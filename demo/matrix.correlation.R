## Fuzzy Logix DB Lytix(TM)
## Stock correlation Demo AdapteR
if(!exists("connection")) {
  stop("no connection variable found. Run demo(connecting) \n ")
}
options(debugSQL=FALSE)
#############################################################
sqlQuery(connection,
           limitRowsSQL(pSelect=paste0("select * from ",getTestTableName("finEquityReturns")),
                        pRows=10))

vtemp <- readline("Above: The table has equity returns stored as triples (what was the equity return of which ticker on what date).\nThese triples define a matrix in deep format.")

eqnRtn <- FLMatrix(table_name        = getTestTableName("finEquityReturns"),
                   row_id_colname    = "TxnDate",
                   col_id_colname    = "TickerSymbol",
                   cell_val_colname  = "EquityReturn",
                   sparse = FALSE)


dim(eqnRtn)

vtemp <- readline("Above: a remote matrix is defined.")

sm <- eqnRtn[,c('AAPL','HPQ','IBM','MSFT','ORCL')]

vtemp <- readline("Next: the R/AdapteR way to compute a correlation matrix -- transparently in-database")

## 2. use the default R 'cor' function
flCorr <- cor(sm)
flCorr

vtemp <- readline("Note that SQL is sent when data is printed or otherwise used")


rEqnRtn <- as.matrix(sm)
rEqnRtn <- na.omit(rEqnRtn)

rCorr <- cor(rEqnRtn, rEqnRtn)
round(rCorr,2)
round(flCorr,2)

vtemp <- readline("Note: The result is in the same format as the R results.")


########################################

(randomstocks <- sample(colnames(eqnRtn), 20))

(dec2006 <- grep("2006-12",rownames(eqnRtn)))

vtemp <- readline("Above: dimnames and index support")

E <- eqnRtn[dec2006, randomstocks]
vtemp <- readline("NO SQL is sent during the definition of subsetting")

if(!is.Hadoop())
print(E)
vtemp <- readline("Data is fetched on demand only, e.g. when printing")



############################################################
if (!requireNamespace("gplots", quietly = TRUE)){
    install.packages("gplots")
}
require(gplots)

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

## End of the scripted demo.
### Thank You ####
