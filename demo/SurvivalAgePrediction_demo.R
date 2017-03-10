library(reshape2)
library(plyr)
library(rpart)
library(plotly)

## read the data
setwd("E:/Office Stuff/Use Cases/World")
rdata<-read.csv(file="indicators.csv")

## selection of variables
indicators<-read.csv(file="codes2.csv", header = T)
codes<-indicators[,2]

## subsetting data for year 2010
modrdata<-reshape2::dcast(rdata,"CountryName~IndicatorName",
                value.var="Value",
                subset=.(IndicatorCode %in% codes & Year==2010))

rownames(modrdata)<-modrdata$CountryName
modrdata$CountryName<-NULL

## survival to age 65 for males
robj<-rpart(data = modrdata, formula= modrdata$`Life expectancy at birth, total (years)`~.)

## prediction

predictedAgeExpectancy<-predict(robj)
predictedAgeExpectancy<-as.data.frame(predictedAgeExpectancy)
colnames(predictedAgeExpectancy)<-"PredictedAge"
head(predictedAgeExpectancy)
predictedAgeExpectancy$CountryName<-rownames(predictedAgeExpectancy)
predictedAgeExpectancy$ActualLifeExpectancy<-modrdata[rownames(predictedAgeExpectancy),"Life expectancy at birth, total (years)"]
predictedAgeExpectancy$DifferenceAgePrediction<-abs(predictedAgeExpectancy$PredictedAge -predictedAgeExpectancy$ActualLifeExpectancy)
## plotting

p5 <- plot_ly(predictedAgeExpectancy) %>% 
  add_trace(
    z = predictedAgeExpectancy$DifferenceAgePrediction,
    text= paste('Country:',predictedAgeExpectancy$CountryName,
                '</br> Predicted Life expectancy:',predictedAgeExpectancy$PredictedAge,
                '</br> Actual life expectancy:' ,predictedAgeExpectancy$ActualLifeExpectancy),
    locations = predictedAgeExpectancy$CountryName, 
    type = "choropleth",locationmode="country names",
    filename="choropleth/world",title="Life expectancy prediction",
    hoverinfo="text") %>%
  layout(title="Life expectancy predictive model")
p5

## What's wrong with CAR, Zimbabwe, Greenland, etc.
vtableName <- getTestTableName("medeconomicdataAmal")

indi<-sqlQuery(connection,"select count(countryname) as xyz, indicatorcode from medEconomicDataAmal where years=2010 group by indicatorcode")
indicat<-indi[indi$xyz>200,2]
resultList <- FLReshape(data=vtableName,
                        formula=CountryName ~ IndicatorCode,
                        value.var="TimeSeriesVal",
                        subset="IndicatorCode in ('SH.ANM.NPRG.ZS','SH.DYN.NMRT','SP.URB.TOTL','SL.TLF.CACT.ZS',        
                                                  'NY.GNP.PCAP.PP.CD','SP.POP.DPND.OL','SM.POP.REFG.OR','AG.LND.FRST.ZS',
                                                  'SH.H2O.SAFE.ZS','NY.ADJ.AEDU.GN.ZS','SP.DYN.LE00.MA.IN','EG.NSF.ACCS.ZS',        
                                                  'NY.GDP.MINR.RT.ZS','SH.STA.ACSN.UR','NY.GDP.PETR.RT.ZS','EN.ATM.CO2E.PP.GD.KD',  
                                                  'NY.GNP.ATLS.CD','SP.RUR.TOTL.ZS','SL.TLF.ACTI.1524.ZS','SH.XPD.PUBL.ZS',        
                                                  'SH.XPD.TOTL.ZS','SP.POP.65UP.TO.ZS','NY.GDP.PCAP.PP.CD','NY.GDP.PCAP.KD.ZG',     
                                                  'EG.ELC.ACCS.UR.ZS','IT.NET.USER.P2','SP.POP.1564.TO.ZS','EN.ATM.CO2E.PP.GD',     
                                                  'SL.TLF.ACTI.ZS','IC.IMP.DURS','SP.DYN.LE00.IN') and Years=2010",
                        outTable="tbl11",
                        dependentColumn='SP.DYN.LE00.IN',
                        drop=TRUE)
tbl<-resultList$table
vmap1<-sqlQuery(connection,"select varid, varidnames from tbl11  where obsid=1 order by 1,2")
vmap2<-sqlQuery(connection,"select distinct(obsidnames) from tbl11 order by 1")
flobj<-rpart(tbl, formula = -1~.)

flobj1<-bagging(tbl)
pred<-predict(flobj)

flobj<-randomForest(tbl ,  formula = -1~.)

