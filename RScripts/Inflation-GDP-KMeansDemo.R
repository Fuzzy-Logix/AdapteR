#cleanup
rm(list=ls())
require(graphics)

library(RLytix)
library(ggplot2)
library(rworldmap)

# Connect to ODBC
DBConnect <- odbcConnect("Gandalf")

# Attach Table 
KMeansTbl <- FLTable(DBConnect, database = "FL_R_WRAP", table = "tblGDPGrowthInflationFiltered")

# Run KMeans
KMeansAnalysis <- FLKMeans(KMeansTbl, centers = 4, max_iter = 20, nstart = 1, primary_key = "CountryID", exclude = c("CountryCode", "CountryName"), where_clause = "")

# Fetch Results
KMeansAnalysis <- FLFetch(KMeansAnalysis)

# Fetching data for Plotting
allpoints  <- sqlQuery(DBConnect,"SELECT * FROM tblGDPGrowthInflationFiltered")
merged     <- merge(KMeansAnalysis@cluster, allpoints, by.x="ObsID",by.y="CountryID")

Dendrogram <- KMeansAnalysis@centers
centers    <- data.frame(x = Dendrogram[Dendrogram$VarID == 1,"Centroid"],
                         y = Dendrogram[Dendrogram$VarID == 2,"Centroid"])

# Plotting
ggplot(data=merged, aes(x=CPI2013, y=GDPGrowth2013, color=ClusterID )) +
geom_point() +
geom_point(data=centers, aes(x=x,y=y, color='Center'), size = 4.5, shape = 5, show_guide = FALSE) +
geom_point(data=centers, aes(x=x,y=y, color='Center'), size = 3)

sPDF <- joinCountryData2Map( merged, joinCode = "ISO3", nameJoinColumn = "CountryCode", nameCountryColumn = "CountryName", verbose = TRUE)
mapCountryData( sPDF, nameColumnToPlot="ClusterID", mapTitle="", numCats = 3, colourPalette = "rainbow", catMethod = "categorical")