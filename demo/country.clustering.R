## Country Clustering Demo AdapteR

if(!exists("connection")) {
  stop("no connection variable found. Run demo(connecting) \n ")
}
oldWarn <- getOption("warn")
options(warn=-1)
options(debugSQL=FALSE)

vtableName <- getTestTableName("medEconomicData")

## Display subset of Table in database
sqlQuery(getFLConnection(),
        limitRowsSQL(paste0("SELECT * FROM ",vtableName),5))

vtemp <- readline("Above: Examine the data in the database \n ")

####
## Process,subset input deepTable -- Data Prep
## required by DB-Lytix functions
resultList <- FLReshape(data=vtableName,
                        formula=CountryName ~ IndicatorCode,
                        value.var="TimeSeriesVal",
                        subset="IndicatorCode in ('NY.GDP.MKTP.KD.ZG','FP.CPI.TOTL.ZG') and Years=2010",
                        outTable="ARtblmedEconomicDataDeep",
                        drop=TRUE)

deepTable <- resultList[["table"]]
Mappings <- resultList[["Dimnames"]]

vtemp <- readline("Above: FLReshape used to process,subset input deepTable \n ")

## Examine deep table
head(deepTable)

vtemp <- readline("Press ENTER to start kmeans in-database: \n ")
kmeansobject <- kmeans(deepTable,4)

vtemp <- readline("kmeans Run Completed\n Press ENTER to display components of output object: \n ")


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

## Plot clusters on a world map
medEconomicData$CountryName <- rownames(medEconomicData)

if (!requireNamespace("plotly", quietly = TRUE)){
    install.packages("plotly")
}
require(plotly)

attach(medEconomicData)
colr <- c("grey","yellow","blue","green","brown","orange")
p1 <- plot_ly(x= Inflation,
              y= GDP,
              type = 'scatter', mode = 'markers',
              text= paste('</br> Country:',rownames(medEconomicData),
                           '</br> Inflation:',Inflation,
                           '</br> GDP:',GDP,
                           '</br> Cluster:',clusters),
              hoverinfo="text",
              marker=list(color=colr[clusters]))
p1

vtemp <- readline("Above: ScatterPlot of clusters vs features \n ")


p2 <- plot_ly() %>% 
      add_trace(
            z = colr[clusters],
            text= paste('</br> Country:',CountryName,
                        '</br> Inflation:',Inflation,
                        '</br> GDP:',GDP,
                        '</br> Cluster:',clusters), 
            locations = CountryName, 
            type = "choropleth",locationmode="country names",
            filename="choropleth/world",title="clusters",
            hoverinfo="text") %>%
      layout(title="country clustering Economic data")
p2

detach(medEconomicData)

vtemp <- readline("Above: Geographical distribution of clusters \n ")

## Effect of GDP on clusters formed
medEconomicDataOrdered <- cbind(medEconomicData,
                                cluster=clusters)
medEconomicDataOrdered <- medEconomicDataOrdered[order(medEconomicDataOrdered$GDP),]

attach(medEconomicDataOrdered)
CountryName <- factor(CountryName, levels = unique(CountryName))
p3 <- plot_ly(x=CountryName,y=GDP,type="bar",
              marker=list(color=colr[medEconomicDataOrdered$cluster])) %>%
      layout(title="Effect of GDP on clusters formed",
             xaxis=list(title="Country Name"),
             yaxis=list(title="GDP"))
p3
detach(medEconomicDataOrdered)

vtemp <- readline("Above: Effect of GDP on clusters formed \n ")

## Effect of Inflation on clusters formed
medEconomicDataOrdered <- medEconomicDataOrdered[order(medEconomicDataOrdered$Inflation),]
attach(medEconomicDataOrdered)
CountryName <- factor(CountryName, levels = unique(CountryName))
p4 <- plot_ly(x=CountryName,y=Inflation,type="bar",
              marker=list(color=colr[medEconomicDataOrdered$cluster])) %>%
  layout(title="Effect of Inflation on clusters formed",
         xaxis=list(title="Country Name"),
         yaxis=list(title="Inflation"))
p4
detach(medEconomicDataOrdered)

vtemp <- readline("Above: Effect of Inflation on clusters formed \n ")

## Effect of GDP on clusters formed -- world heat map view
attach(medEconomicData)
p5 <- plot_ly() %>% 
      add_trace(
              z = GDP,
              text= paste('</br> Country:',CountryName,
                        '</br> GDP:',GDP,
                        '</br> Inflation:',Inflation,
                        '</br> Cluster:',clusters),
              locations = CountryName, 
              type = "choropleth",locationmode="country names",
              filename="choropleth/world",title="clusters",
              hoverinfo="text") %>%
      layout(title="GDP and Clusters distribution")
p5
detach(medEconomicData)

vtemp <- readline("Above: Effect of GDP on clusters formed -- world heat map view \n ")

## Effect of Inflation on clusters formed -- world heat map view
attach(medEconomicData)
p5 <- plot_ly() %>% 
      add_trace(
              z = GDP,
              text= paste('</br> Country:',CountryName,
                        '</br> Inflation:',Inflation,
                        '</br> GDP:',GDP,
                        '</br> Cluster:',clusters),
              locations = CountryName, 
              type = "choropleth",locationmode="country names",
              filename="choropleth/world",title="clusters",
              hoverinfo="text") %>%
      layout(title="Inflation and Clusters distribution")
p5
detach(medEconomicData)

vtemp <- readline("Above: Effect of Inflation on clusters formed -- world heat map view \n ")

## END ##
# Thank You #
