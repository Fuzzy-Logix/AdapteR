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

vtableName <- getTestTableName("medeconomicdataAmal")
#############################################################

##################### KMeans Clustering ########################
################################################################
#### Data Preparation
vtemp <- readline("Data Preparation: \n ")

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
## Examine Mappings
head(Mappings)

## Run kmeans on deepTable
vtemp <- readline("Press ENTER to start kmeans in-database: \n ")
kmeansobject <- kmeans(deepTable,6)

vtemp <- readline("kmeans Run Completed\n Press ENTER to display components of output object: \n ")
## Examine Result object
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
## Plot clusters on a world map
medEconomicData$CountryName <- rownames(medEconomicData)

attach(medEconomicData)
if (!requireNamespace("plotly", quietly = TRUE)){
    install.packages("plotly")
}
require(plotly)

colr <- c("grey","yellow","blue","green","brown","orange")
p1 <- plot_ly(data=medEconomicData,
              x= Inflation,
              y= GDP,
              type = 'scatter', mode = 'markers',
              text= paste('Country:',rownames(medEconomicData),
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

detach(medEconomicData)

vtemp <- readline("Above: Geographical distribution of clusters \n ")

## Effect of GDP on clusters formed
medEconomicDataOrdered <- cbind(medEconomicData,
                                cluster=clusters)
medEconomicDataOrdered <- medEconomicDataOrdered[order(medEconomicDataOrdered$GDP),]

attach(medEconomicDataOrdered)
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
detach(medEconomicData)

vtemp <- readline("Above: Effect of GDP on clusters formed -- world heat map view \n ")

## Effect of Inflation on clusters formed -- world heat map view
attach(medEconomicData)
p5 <- plot_ly() %>% 
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
detach(medEconomicData)

vtemp <- readline("Above: Effect of Inflation on clusters formed -- world heat map view \n ")


################################## Survival Age Prediction Demo ################################
## getting the table name
## change database to FLRev_4878 for rTree functionality
voldDatabase <- getOption("ResultDatabaseFL")
# sqlQuery(connection,"database FLRev_4878;")
# sqlQuery(connection,"SET ROLE ALL;")

# connection <- flConnect(odbcSource = "Gandalf",database = "FLRev_4878",platform="TD",pkg = "dbc")
setCurrentDatabase("FLRev_4878")
#### Data Preparation
vtemp <- readline("Data Preparation: \n ")
## constructing a deep table for decision tree implementation
resultList <- FLReshape(data=vtableName,
                        formula=CountryName ~ IndicatorCode,
                        value.var="TimeSeriesVal",
                        subset="IndicatorCode in ('SH.ANM.NPRG.ZS','SH.DYN.NMRT','SP.URB.TOTL','SL.TLF.CACT.ZS',
                                                  'NY.GNP.PCAP.PP.CD','SP.POP.DPND.OL','SM.POP.REFG.OR','AG.LND.FRST.ZS',
                                                  'SH.H2O.SAFE.ZS','NY.ADJ.AEDU.GN.ZS','SP.RUR.TOTL','EG.NSF.ACCS.ZS',
                                                  'NY.GDP.MINR.RT.ZS','SH.STA.ACSN.UR','NY.GDP.PETR.RT.ZS','EN.ATM.CO2E.PP.GD.KD',
                                                  'NY.GNP.ATLS.CD','SP.RUR.TOTL.ZS','SL.TLF.ACTI.1524.ZS','SH.XPD.PUBL.ZS',
                                                  'SH.XPD.TOTL.ZS','SP.POP.65UP.TO.ZS','NY.GDP.PCAP.PP.CD','NY.GDP.PCAP.KD.ZG',
                                                  'EG.ELC.ACCS.UR.ZS','IT.NET.USER.P2','SP.POP.1564.TO.ZS','EN.ATM.CO2E.PP.GD',
                                                  'SL.TLF.ACTI.ZS','IC.IMP.DURS','SP.DYN.LE00.IN') and Years=2010",
                        outTable="ARtblmedEconomicDataDeep",
                        dependentColumn='SP.DYN.LE00.IN',
                        drop=TRUE)

deepTable <- resultList$table

## mapping tables for metadata

vmap1<-sqlQuery(connection,"select varid, varidnames from ARtblmedEconomicDataDeep where obsid=1 order by 1,2")
vmap2<-sqlQuery(connection,"select distinct(obsidnames) from ARtblmedEconomicDataDeep order by 1")

colnames(vmap2)<-"CountryNames"

#### Fitting regression tree
vtemp <- readline("Running rtree in-database: \n ")
## running regression tree on the table

flobj<-rtree(deepTable, formula = -1~.)

vtemp <- readline("Plotting the output: \n ")
plot(flobj)

## prediction from the decision tree model
vtemp <- readline("Scoring on same dataset: \n ")
pred<-predict(flobj)

## aggregate mean for prediction values
pred2<-rowMeans(pred)

#####################################################
############## Data Visualization ###################
#####################################################
vtemp <- readline("Plot predicted ages on world map: \n ")
## plotting predictions on world map
l<-data.frame(as.vector(pred2), vmap2)
colnames(l)<-c("PredictedAge","CountryNames")
attach(l)
p5 <- plot_ly() %>%
  add_trace(
    z = l$PredictedAge,
    text= paste('Country:',l$CountryNames,
                '</br> Predicted Life expectancy:',l$PredictedAge),
    locations = l$CountryNames,
    type = "choropleth",locationmode="country names",
    filename="choropleth/world",title="Life expectancy prediction",
    hoverinfo="text") %>%
  layout(title="Life expectancy predictive model")
p5
detach(l)
## plotting differences between predicted age and actual age

ret<-sqlQuery(getFLConnection(),"SELECT obsid, num_val FROM ARtblmedEconomicDataDeep WHERE varid = -1 ORDER BY 1")
ret2<-ret[,2]
l<-data.frame(ret2 - as.vector(pred2), vmap2)
colnames(l)<-c("PredictedAgeDifference","CountryNames")
attach(l)
p5 <- plot_ly(l) %>%
  add_trace(
    z = l$PredictedAgeDifference,
    text= paste('Country:',l$CountryNames,
                '</br> Predicted Life expectancy:',l$PredictedAgeDifference),
    locations = l$CountryNames,
    type = "choropleth",locationmode="country names",
    filename="choropleth/world",title="Life expectancy prediction",
    hoverinfo="text") %>%
  layout(title="Life expectancy predictive model")
p5
detach(l)
####### END #######
#### Thank You ####
## clean up
options(warn=oldWarn)
setCurrentDatabase(voldDatabase)
# demo("connecting")
# sqlQuery(connection,paste0("database ",voldDatabase,";"))
# sqlQuery(connection,"SET ROLE ALL;")

