rm(list=ls())
setwd("D:/R wrappers/RWrappers/trunk")

# include
source("S4//S4FLTable.r")
source("S4//FLDataPrep.r")
source("S4//utilities.r")

library(RODBC)
DBConnect <- odbcConnect("Gandalf")

########################################################################################################

# Demo for FLt.Test1S
source("S4//tTest.r")
# Create FLTable object
Tbl <-  FLTable(DBConnect, DBName = "FL_R_WRAP", TableName = "RWrappertTest1sTest")
# Perform t-Test
res <- FLt.Test(Tbl, "Num_Val", mu = 1.0)
########################################################################################################

# Demo for FLt.Test2S
source("S4//tTest.r")
# Create FLTable object
Tbl <-  FLTable(DBConnect, DBName = "FL_R_WRAP", TableName = "RWrappertTest2sTest")
# Perform t-Test
res <- FLt.Test(Tbl, "InVal1", "InVal2")
########################################################################################################

# Demo for FLz.Test1S
source("S4//FLzTest.r")
# Create FLTable object
Tbl <-  FLTable(DBConnect, DBName = "FL_R_WRAP", TableName = "RWrappertTest1sTest")
# Perform z-Test
res <- FLz.Test(Tbl, "Num_Val", mu = 0.45)
########################################################################################################

# Demo for FLz.Test2S
source("S4//FLzTest.r")
# Create FLTable object
Tbl <-  FLTable(DBConnect, DBName = "FL_R_WRAP", TableName = "RWrappertTest2sTest")
# Perform z-Test
res <- FLz.Test(Tbl, "InVal1", "InVal2")
########################################################################################################
