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
## The demo gives an overview of kmeans
## functionality of AdapteR. Economic data
## is used to identify clusters of countries.

### Pre-setup
oldWarn <- getOption("warn")
options(warn=-1)


if(!exists("connection")) {
    demo("connecting", package="AdapteR")
}


#############################################################
#### Data Preparation
vtemp <- readline("Data Preparation: \n ")

vtableName <- getTestTableName("ARmedEconomicData")

## Display subset of Table in database
sqlQuery(getFLConnection(),
        limitRowsSQL(paste0("SELECT * FROM ",vtableName),5))

vtemp <- readline("Above: Examine the data in the database \n ")

####
## Process,subset input deepTable -- Data Prep
## required by DB-Lytix functions
if(!exists("resultList"))
resultList <- FLReshape(data=vtableName,
                        formula=CountryName ~ IndicatorCode,
                        value.var="TimeSeriesVal",
                        subset="IndicatorCode in ('NY.GDP.MKTP.KD.ZG','FP.CPI.TOTL.ZG') and Years=2010",
                        outTable="tbl1",
                        drop=TRUE)

deepTable <- resultList[["table"]]
Mappings <- resultList[["Dimnames"]]

vtemp <- readline("Above: FLReshape used to process,subset input deepTable \n ")

## Examine deep table
head(deepTable)
## Examine Mappings
head(Mappings)

## Run kmeans on deepTable
vtemp <- readline("Press ENTER to start kmeans in-database: \n ")
if(!exists("kmeansobject"))
kmeansobject <- kmeans(deepTable,6)

vtemp <- readline("kmeans Run Completed \n ")

## Examine Result object
vtemp <- readline("Press ENTER to display components of output object: \n ")
## Fetch the resulting clusters
clusters <- as.vector(kmeansobject$cluster)
clusters

Sys.sleep(3)

kmeansobject$centers

kmeansobject$size

kmeansobject$betweenss

Sys.sleep(3)

kmeansobject$withinss

Sys.sleep(3)

kmeansobject$tot.withinss

Sys.sleep(3)

kmeansobject$totss


## Visualization using pltoly
vtemp <- readline("Data Visualization: \n ")

## Fetch the processed data
medEconomicData <- as.data.frame(deepTable)

vtemp <- readline("Above: Fetch Economic Data for plotting \n ")
## Set rownames and colnames
rownames(medEconomicData) <- as.vector(Mappings[[1]])
colnames(medEconomicData) <- as.vector(Mappings[[2]])
vIndicatorMap <- c(NY.GDP.MKTP.KD.ZG="GDP",
                    FP.CPI.TOTL.ZG="Inflation")
colnames(medEconomicData) <- vIndicatorMap[colnames(medEconomicData)]

## Examine subset of data
head(medEconomicData)
dim(medEconomicData)
vtemp <- readline("Above: Examine the data before plotting \n ")
require(plotly)
colr <- c("grey","yellow","blue","green","brown","orange")
p1 <- plot_ly(medEconomicData,x= Inflation,y= GDP,
              type = 'scatter', mode = 'markers',
              text= paste('Country:',rownames(medEconomicData),
                           '</br> Inflation:',Inflation,
                           '</br> GDP:',GDP,
                           '</br> Cluster:',clusters),
              hoverinfo="text",
              marker=list(color=colr[clusters]))
p1

vtemp <- readline("Above: ScatterPlot of clusters vs features \n ")

## Plot clusters on a world map
medEconomicData$CountryName <- rownames(medEconomicData)

p2 <- plot_ly(medEconomicData) %>% 
      add_trace(
            z = colr[clusters],
            text= paste('Country:',CountryName,
                        '</br> Inflation:',Inflation,
                        '</br> GDP:',GDP,
                        '</br> Cluster:',clusters), 
            locations = CountryName, 
            type = "choropleth",locationmode="country names",
            filename="choropleth/world",title="clusters",
            hoverinfo="text") %>%
      layout(title="country clustering Economic data")
p2

vtemp <- readline("Above: Geographical distribution of clusters \n ")

## Effect of GDP on clusters formed
medEconomicDataOrdered <- cbind(medEconomicData,
                                cluster=clusters)
medEconomicDataOrdered <- medEconomicDataOrdered[order(medEconomicDataOrdered$GDP),]

p3 <- plot_ly(medEconomicDataOrdered,
              x=CountryName,y=GDP,type="bar",
              marker=list(color=colr[medEconomicDataOrdered$cluster])) %>%
      layout(title="Effect of GDP on clusters formed",
             xaxis=list(title="Country Name"),
             yaxis=list(title="GDP"))
p3

vtemp <- readline("Above: Effect of GDP on clusters formed \n ")

## Effect of Inflation on clusters formed
medEconomicDataOrdered <- medEconomicDataOrdered[order(medEconomicDataOrdered$Inflation),]

p4 <- plot_ly(medEconomicDataOrdered,
              x=CountryName,y=Inflation,type="bar",
              marker=list(color=colr[medEconomicDataOrdered$cluster])) %>%
  layout(title="Effect of Inflation on clusters formed",
         xaxis=list(title="Country Name"),
         yaxis=list(title="Inflation"))
p4

vtemp <- readline("Above: Effect of Inflation on clusters formed \n ")

## Effect of GDP on clusters formed -- world heat map view
p5 <- plot_ly(medEconomicData) %>% 
      add_trace(
              z = GDP,
              text= paste('Country:',CountryName,
                        '</br> GDP:',GDP,
                        '</br> Inflation:',Inflation,
                        '</br> Cluster:',clusters),
              locations = CountryName, 
              type = "choropleth",locationmode="country names",
              filename="choropleth/world",title="clusters",
              hoverinfo="text") %>%
      layout(title="GDP and Clusters distribution")
p5

vtemp <- readline("Above: Effect of GDP on clusters formed -- world heat map view \n ")

## Effect of Inflation on clusters formed -- world heat map view
p5 <- plot_ly(medEconomicData) %>% 
      add_trace(
              z = GDP,
              text= paste('Country:',CountryName,
                        '</br> Inflation:',Inflation,
                        '</br> GDP:',GDP,
                        '</br> Cluster:',clusters),
              locations = CountryName, 
              type = "choropleth",locationmode="country names",
              filename="choropleth/world",title="clusters",
              hoverinfo="text") %>%
      layout(title="Inflation and Clusters distribution")
p5

vtemp <- readline("Above: Effect of Inflation on clusters formed -- world heat map view \n ")

####### END #######
#### Thank You ####
## clean up
options(warn=oldWarn)
