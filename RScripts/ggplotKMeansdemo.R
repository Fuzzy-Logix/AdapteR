#cleanup
rm(list=ls())
require(graphics)

library(RLytix)
# Connect to ODBC
DBConnect <- odbcConnect("Gandalf")

# Attach Table 
KMeansTbl <- FLTable(DBConnect, database = "FL_R_WRAP", table = "tblKMeansDemo")

# Run KMeans
KMeansAnalysis <- FLKMeans(KMeansTbl, centers = 5, max_iter = 20, nstart = 1, primary_key = "ID")

# Fetch Results
KMeansAnalysis <- FLFetch(KMeansAnalysis)

# Fetching data for Plotting
allpoints  <- sqlQuery(DBConnect,"SELECT * FROM tblKMeansDemo")
merged     <- merge(KMeansAnalysis@cluster, allpoints, by.x="ObsID",by.y="ID")

Dendrogram <- KMeansAnalysis@centers
centers    <- data.frame(x = Dendrogram[Dendrogram$VarID == 1,"Centroid"],
                         y = Dendrogram[Dendrogram$VarID == 2,"Centroid"])

# Plotting
ggplot(data=merged, aes(x=X, y=Y, color=ClusterID )) +
geom_point() +
geom_point(data=centers, aes(x=x,y=y, color='Center'), size = 4.5, shape = 5, show_guide = FALSE) +
geom_point(data=centers, aes(x=x,y=y, color='Center'), size = 3)