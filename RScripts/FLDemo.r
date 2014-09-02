# cleanup workspace

rm(list=ls())
setwd("C:/Users/STPL/Fuzzy Logix/RWrappers/trunk/S3")

#include

source("S3FLTable.r")
source("S3FLkmeans.r")
library(RODBC)
require(graphics)

#Prep Demo

Tbl <- FLTable(DSN="Gandalf",DBName="FL_TRAIN",TableName="tblAbaloneWideTest")
TblPrep <- FLDataPrep(Tbl,Exclude=c("Rings"),ClassSpec=list(SEX="M",DummyCat="D"))

#KMeans Demo with Deep Table

Tbl2 <- FLTable(DSN="Gandalf",DBName="FL_TRAIN",TableName="tblUSArrests")
Tbl2[["DeepTableName"]] = "tblUSArrests"
res2 <- FLkmeans(Tbl2, 2, iter.max = 20, nstart = 2)

#KMeans Demo with Wide Table

Tbl3 <- FLTable(DSN="Gandalf",DBName="FL_TRAIN",TableName="fzzlKMeansDemo")
Tbl3Prep <- FLDataPrep(Tbl3)
res3 <- FLkmeans(Tbl3Prep, 2, iter.max = 20, nstart = 1)

allpoints <- select.all(Tbl3)
merged <- merge(res3$ClusterID,allpoints,by.x="ObsID",by.y="ID")
#centers <- matrix(c(res3$Dendrogram[res3$Dendrogram$VarID == 1,"Centroid"],res3$Dendrogram[res3$Dendrogram$VarID == 2,"Centroid"]),ncol=2,dimnames = list(c(),c("x","y")))

plot(merged$X,merged$Y,col = merged$ClusterID)
centers <- list(x=res3$Dendrogram[res3$Dendrogram$VarID == 1,"Centroid"],
				y=res3$Dendrogram[res3$Dendrogram$VarID == 2,"Centroid"])
points(centers[["x"]],centers[["y"]], col = 1:2, pch = 8, cex = 2)