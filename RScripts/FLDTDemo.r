rm(list=ls())
setwd("D:/R wrappers/RWrappers/trunk")

# include
source("S4//S4FLTable.r")
source("S4//FLDataPrep.r")
source("S4//FLDecisionTree.r")
source("S4//utilities.r")
source("S4//results.r")

library(RODBC)
DBConnect <- odbcConnect("Gandalf")

# Create FLTable object
Tbl2 <-  FLTable(DBConnect, DBName = "FL_R_WRAP", TableName = "tblDecisionTreeMulti")
res2 <- FLDecisionTree(Tbl2, MinObsforParent = 10, MaxLevel = 5, PurityThreshold = 0.95)

#  Display result
res2 <- fetch.results(res2)
res2 