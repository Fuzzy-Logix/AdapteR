rm(list=ls())
source("S4FLTable.r")
source("FLDecisionTree.r")

#Prep Demo

#Tbl <- FLTable(DSN="Gandalf",DBName="FL_TRAIN",TableName="tblAbaloneWideTest")
#TblPrep <- FLDataPrep(Tbl,DepCol = c("MPG"), Exclude=c("CarNum"),ClassSpec=list(CarName="BMW"))

#DecisionTree Demo

Tbl2 <- FLTable(Connection="Gandalf",DBName="FL_TRAIN",TableName="tblDTData")
Tbl2[["DeepTableName"]] = "tblDTData"
res <- FLDecisionTree(Tbl2, NumOfSplits = 100, MaxLevel = 4, PurityThreshold = 0.8)