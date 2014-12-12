#cleanup
rm(list=ls())
setwd("C:/Users/STPL/Fuzzy Logix/RWrappers/trunk")

#includes
source("S4//S4FLTable.r")
source("S4//results.r")
source("S4//utilities.r")
source("S4//FLDataPrep.r")
source("S4//FLKmeans.r")
library(RODBC)
require(graphics)

library()
# Connect to ODBC
DBConnect <- odbcConnect("Gandalf")

# Attach Table 
KMeansTbl <- FLTable(DBConnect,DBName="FL_R_WRAP",TableName="fzzlKMeansDemo")

# Run KMeans
KMeansAnalysis <- FLKMeans(KMeansTbl, 2, iter.max = 20, nstart = 1,PrimaryKey = "ID")

# Fetch Results
KMeansAnalysis <- fetch.results(KMeansAnalysis)

# Plotting

allpoints <- sqlQuery(DBConnect,"SELECT * FROM tblKMeansDemo")

merged <- merge(KMeansAnalysis@cluster,allpoints,by.x="ObsID",by.y="ID")
plot(merged$X,merged$Y,col = as.numeric(merged$ClusterID) + 3)
Dendrogram <- KMeansAnalysis@centers
centers <- list(x=Dendrogram[Dendrogram$VarID == 1,"Centroid"],
				y=Dendrogram[Dendrogram$VarID == 2,"Centroid"])
points(centers[["x"]],centers[["y"]], col = c(6,2), pch = 8, cex = 2)