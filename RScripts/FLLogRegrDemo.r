rm(list=ls())
setwd("D:/R wrappers/RWrappers/trunk")

# include
source("S4//S4FLTable.r")
source("S4//FLDataPrep.r")
source("S4//utilities.r")
source("S4//results.r")

library(RODBC)
DBConnect <- odbcConnect("Gandalf")
########################################################################################################

# Demo for FLDecisionTree
source("S4//FLDecisionTree.r")
# Create FLTable object
Tbl <-  FLTable(DBConnect, DBName = "FL_R_WRAP", TableName = "tblAutoMpg")
# Perform Decision Tree Analysis
res <- FLDecisionTree(Tbl, DepCol = "Weight" ,PrimaryKey = "ObsID", MinObsforParent = 10, MaxLevel = 5, PurityThreshold = 0.95, Exclude = c("CarNum", "CarNumber"), ClassSpec = list(CarName = "BMW"))

########################################################################################################
# Demo for FLLogRegr
source("S4//FLLogRegr.r")
# Create FLTable object
Tbl <-  FLTable(DBConnect, DBName = "FL_R_WRAP", TableName = "LogRegrRWrapperDemo")
# Perform Logistic Regression
res <- FLLogRegr(Tbl, DepCol = "Bought", PrimaryKey = "ObsID", MaxIterations = 25, pThreshold = 0.1)

########################################################################################################
# Demo for FLLogRegrStep
source("S4//FLLogRegrStep.r")
# Create FLTable object
Tbl <-  FLTable(DBConnect, DBName = "FL_R_WRAP", TableName = "LogRegrRWrapperDemo")
# Perform Logistic Regression
res <- FLLogRegrStep(Tbl, DepCol = "Bought", PrimaryKey = "ObsID", Type = "BW", MaxIterations = 25, pThreshold = 0.1)

########################################################################################################
# Demo for FLLDA
source("S4//FLLDA.r")
# Create FLTable object
Tbl <-  FLTable(DBConnect, DBName = "FL_R_WRAP", TableName = "tblirisdata")
# Perform LDA
res <- FLLDA(Tbl,DepCol = "SpeciesID" ,PrimaryKey = "ObsID",Exclude = c("Species"))

########################################################################################################
# Demo for FLMDA
source("S4//FLMDA.r")
# Create FLTable object
Tbl2 <- FLTable(DBConnect, DBName = "FL_R_WRAP", TableName = "tblMDA")
# Perform MDA
res2 <- FLMDA(Tbl2, Subclasses = 3, Iterations = 10,Initialization = 2,Hypotheses = 5)
########################################################################################################

#  Display result
res2 <- fetch.results(res2)
res2 