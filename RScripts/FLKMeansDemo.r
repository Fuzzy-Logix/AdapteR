#cleanup
rm(list=ls())
require(graphics)

library(RLytix)
# Connect to ODBC
DBConnect <- odbcConnect("Gandalf")

# Attach Table 
KMeansTbl <- FLTable(DBConnect, database = "FL_R_WRAP", table = "tblKMeansDemo")

# Run KMeans
KMeansAnalysis <- FLKMeans(KMeansTbl, centers = 2, max_iter = 20, nstart = 1, primary_key = "ID")

# Fetch Results
KMeansAnalysis <- FLFetch(KMeansAnalysis)

# Plotting
allpoints <- sqlQuery(DBConnect,"SELECT * FROM tblKMeansDemo")
merged <- merge(KMeansAnalysis@cluster, allpoints, by.x="ObsID",by.y="ID")

color_map <- c("green", "red")
point_colors <- sapply(as.numeric(merged$ClusterID), function(x) color_map[x])

plot(merged$X,merged$Y,col = point_colors, xlab="x", ylab="y")
Dendrogram <- KMeansAnalysis@centers
centers <- list(x=Dendrogram[Dendrogram$VarID == 1,"Centroid"],
				y=Dendrogram[Dendrogram$VarID == 2,"Centroid"])
points(centers[["x"]],centers[["y"]], col = c("red","green"), pch = ".", cex = 8)