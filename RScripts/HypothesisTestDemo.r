rm(list=ls())
setwd("D:/R wrappers/RWrappers/trunk")

# include
source("S4//S4FLTable.r")
source("S4//FLDataPrep.r")
source("S4//utilities.r")

library(RODBC)
DBConnect <- odbcConnect("Gandalf")

library(HypoR)
DBConnect <- odbcConnect("Gandalf")
########################################################################################################

# Demo for FLtTest1S
source("S4//tTest.r")
# Create FLTable object
Tbl <-  FLTable(DBConnect, "FL_R_WRAP", "RWrappertTest1sTest")
# Perform t-Test
res <- FLtTest(Tbl, input1 = "Num_Val", mu = 1.0)
########################################################################################################

# Demo for FLtTest2S
source("S4//tTest.r")
# Create FLTable object
library(HypoR)
DBConnect <- odbcConnect("Gandalf")
Tbl <-  FLTable(DBConnect, "FL_R_WRAP", "RWrappertTest2sTest")
res <- FLtTest(Tbl, input1 = "InVal1", input2 = "InVal2", mu = 1.0)
# Perform t-Test
res <- FLtTest(Tbl, "InVal1", "InVal2")
res <- FLtTest(Tbl, input1 = "InVal1", input2 = "InVal2", mu = 1.0)
########################################################################################################

# Demo for FLz.Test1S
source("S4//FLzTest.r")
# Create FLTable object
Tbl <-  FLTable(DBConnect, "FL_R_WRAP", "RWrappertTest1sTest")
# Perform z-Test
res <- FLz.Test(Tbl, "Num_Val", mu = 0.45)
########################################################################################################

# Demo for FLz.Test2S
source("S4//FLzTest.r")
# Create FLTable object
library(HypoR)
DBConnect <- odbcConnect("Gandalf")
Tbl <-  FLTable(DBConnect, "FL_R_WRAP", "RWrappertTest2sTest")
# Perform z-Test
res <- FLzTest(Tbl, input1 = "InVal1", input2 = "InVal2")
########################################################################################################
