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

################################## Survival Age Prediction Demo ################################
## getting the table name
#### Data Preparation
vtemp <- readline("Data Preparation: \n ")
vtableName <- getTestTableName("medEconomicData")

## Display subset of Table in database
sqlQuery(getFLConnection(),
        limitRowsSQL(paste0("SELECT * FROM ",vtableName),5))

vtemp <- readline("Above: Examine the data in the database \n ")

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
                '</br> Actual Life expectancy greater than predicted by (years):',l$PredictedAgeDifference),
    locations = l$CountryNames,
    type = "choropleth",locationmode="country names",
    filename="choropleth/world",title="Is the Actual Life Expectancy Greater Than Predicted?",
    hoverinfo="text") %>%
  layout(title="Is the Actual Life Expectancy Greater Than Predicted?")
p5
detach(l)
####### END #######
#### Thank You ####
## clean up
options(warn=oldWarn)
# demo("connecting")
# sqlQuery(connection,paste0("database ",voldDatabase,";"))
# sqlQuery(connection,"SET ROLE ALL;")

